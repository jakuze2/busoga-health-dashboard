# 09_explain.R
# Explainable AI: which factors drive the monthly numbers, by sub-county, and how much.
#
# For each outcome below, a random forest (ranger) is fitted to sub-county x month data on the log
# scale, log(1 + value), with climate, season, reporting, last month's level and each sub-county's
# usual level as inputs. Shapley values (SHAP, estimated by permutation sampling) split every
# prediction into the contribution of each input. On the log scale these contributions multiply:
# exp(contribution) - 1 is the % by which an input moved the month up or down.
#   * accuracy is checked on the last 12 months, which the model does not see when it is trained
#   * a factor (other than the sub-county's own past levels) is described as "consistent" only if
#     it ranks in the top three of those factors in at least 4 of 5 refits on resampled
#     sub-counties; otherwise the text calls it less certain
#   * these are associations learned from routine data, not causal effects
# Writes app/data/xai.rds. Runs in the monthly and full refreshes (a few minutes per outcome).

suppressPackageStartupMessages({ library(data.table); library(ranger) })
log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), sprintf(...), "\n", sep = "")
set.seed(20260923)

OUTCOMES <- list(
  list(id = "malaria",    code = "MAL09", label = "Confirmed malaria cases per 1,000 people", short = "malaria cases",
       per_pop = TRUE,  unit = "per 1,000 people per month"),
  list(id = "positivity", code = "MAL02", label = "Malaria test positivity", short = "malaria test positivity",
       per_pop = FALSE, unit = "% of tests positive"),
  list(id = "opd",        code = "SRV01", label = "OPD new attendances per 1,000 people", short = "OPD attendance",
       per_pop = TRUE,  unit = "per 1,000 people per month"),
  list(id = "deliveries", code = "DEL01", label = "Facility deliveries per 1,000 people", short = "facility deliveries",
       per_pop = TRUE,  unit = "per 1,000 people per month"))

FEATURES <- c(
  usual       = "Usual level in the sub-county (previous 12 months)",
  prev        = "Level last month",
  month       = "Time of year (calendar month)",
  rain0       = "Rainfall this month",
  rain1       = "Rainfall one month earlier",
  rain2       = "Rainfall two months earlier",
  spi3        = "Wetness over the last 3 months (SPI-3)",
  tmax0       = "Average maximum temperature this month",
  tmax1       = "Average maximum temperature one month earlier",
  heat        = "Hot days this month (max above 32 °C)",
  complete    = "Reporting completeness in the sub-county")

im  <- readRDS("app/data/ind_month.rds")
ou  <- fread("app/data/orgunits.csv", na.strings = "")
pop <- readRDS("app/data/population.rds")
cl  <- readRDS("app/data/climate.rds")
rep <- readRDS("app/data/reporting.rds")
ind <- fread("app/data/indicators.csv")

sc   <- ou[level_name == "subcounty", .(uid, district_uid = uid_l3)]
dist <- ou[level_name == "district", .(district_uid = uid, district = name)]
sc   <- merge(sc, dist, by = "district_uid")

# ---- shared inputs, sub-county x month -------------------------------------------------------
cl <- cl[level == "subcounty", .(uid, period, rain_mm, spi3, tmax_c, heat_days)]
setorder(cl, uid, period)
cl[, `:=`(rain1 = shift(rain_mm, 1), rain2 = shift(rain_mm, 2), tmax1 = shift(tmax_c, 1)), by = uid]
fac <- ou[level_name == "facility", .(fuid = uid, uid = uid_l5)]
rp  <- rep[dataset == "RtEYsASU7PG"][fac, on = .(uid = fuid), nomatch = NULL][
  , .(complete = if (sum(expected)) 100 * sum(actual) / sum(expected) else NA_real_), by = .(uid = i.uid, period)]
pp  <- pop[level == "subcounty", .(uid, year, pop)]

build <- function(o) {
  x <- im[level == "subcounty" & code == o$code, .(uid, period, num, den)]
  x <- x[uid %in% sc$uid]
  x[, year := period %/% 100L]
  x <- merge(x, pp, by = c("uid", "year"), all.x = TRUE)
  factor <- ind$factor[ind$code == o$code]
  x[, y := if (o$per_pop) 1000 * num / pop else fifelse(den > 0, factor * num / den, NA_real_)]
  if (!o$per_pop) x[den < 20, y := NA_real_]                     # too few tests for a stable rate
  x <- x[is.finite(y)]
  setorder(x, uid, period)
  # the sub-county's usual level: mean of the previous 12 months (needs at least 6)
  x[, usual := { z <- shift(frollmean(y, 12, na.rm = TRUE, algo = "exact"), 1); n <- shift(frollsum(!is.na(y), 12), 1)
                 fifelse(n >= 6, z, NA_real_) }, by = uid]
  x[, prev := { pv <- shift(y); pp <- shift(period); fifelse(!is.na(pp) & (period - pp == 1L | period - pp == 89L), pv, NA_real_) }, by = uid]
  x <- merge(x, cl, by = c("uid", "period"), all.x = TRUE)
  x <- merge(x, rp, by = c("uid", "period"), all.x = TRUE)
  x[, `:=`(month = period %% 100L, rain0 = rain_mm, tmax0 = tmax_c, heat = heat_days)]
  x <- x[complete.cases(x[, c("y", names(FEATURES)), with = FALSE])]
  # model scale: log(1 + value) for the outcome and for its own past levels
  x[, `:=`(ly = log1p(y), usual_raw = usual, prev_raw = prev)][, `:=`(usual = log1p(usual), prev = log1p(prev))]
  x[, c("uid", "period", "y", "ly", "usual_raw", "prev_raw", "num", "den", "pop", names(FEATURES)), with = FALSE]
}

fit <- function(d, imp = "none") ranger(x = as.data.frame(d[, names(FEATURES), with = FALSE]), y = d$ly, num.trees = 300,
                                        min.node.size = 10, mtry = 4, importance = imp, num.threads = 2, seed = 1)

# permutation-sampling Shapley values (Strumbelj & Kononenko 2014), antithetic, one batch of
# predictions per explained row
shap_rows <- function(model, X, bg, M = 10) {
  p <- ncol(X); feats <- colnames(X)
  out <- matrix(0, nrow(X), p, dimnames = list(NULL, feats))
  for (i in seq_len(nrow(X))) {
    perms <- lapply(seq_len(M), function(m) sample(p)); perms <- c(perms, lapply(perms, rev))
    zs <- bg[sample(nrow(bg), length(perms), replace = TRUE), , drop = FALSE]
    rows <- vector("list", length(perms)); jj <- integer()
    for (m in seq_along(perms)) {
      o <- perms[[m]]; z <- zs[m, ]; x <- X[i, ]
      # walk the permutation: rows k = features o[1..k] from x, the rest from z
      mat <- matrix(rep(unlist(z), p + 1), nrow = p + 1, byrow = TRUE, dimnames = list(NULL, feats))
      for (k in seq_len(p)) mat[(k + 1):(p + 1), o[k]] <- unlist(x[o[k]])
      rows[[m]] <- mat; jj <- c(jj, o)
    }
    big <- do.call(rbind, rows)
    pr <- predict(model, as.data.frame(big), num.threads = 2)$predictions
    pr <- matrix(pr, nrow = p + 1)
    d <- pr[-1, , drop = FALSE] - pr[-(p + 1), , drop = FALSE]          # marginal gain of each added feature
    out[i, ] <- tapply(as.vector(d), jj, mean)[as.character(seq_len(p))]
  }
  # sampling noise: make each row add up exactly to prediction - baseline, shared in proportion
  # to the size of each contribution (the efficiency property of Shapley values)
  target <- predict(model, X, num.threads = 2)$predictions - mean(predict(model, bg, num.threads = 2)$predictions)
  gap <- target - rowSums(out); w <- abs(out) / pmax(rowSums(abs(out)), 1e-9)
  out + gap * w
}

res <- list()
for (o in OUTCOMES) {
  t0 <- Sys.time()
  d <- build(o)
  if (nrow(d) < 500) { log_msg("%s: only %d rows, skipped", o$id, nrow(d)); next }
  last <- max(d$period); cut <- sort(unique(d$period), decreasing = TRUE)[12]
  # 1. accuracy on the last 12 months, unseen in training, against two simple benchmarks
  m_test <- fit(d[period < cut]); te <- d[period >= cut]
  pr <- predict(m_test, as.data.frame(te[, names(FEATURES), with = FALSE]), num.threads = 2)$predictions
  r2f <- function(p) 1 - sum((te$ly - p)^2) / sum((te$ly - mean(te$ly))^2)
  r2 <- r2f(pr); r2_usual <- r2f(te$usual); r2_prev <- r2f(te$prev)
  mae <- mean(abs(te$y - expm1(pr)))
  # 2. stability: refits on resampled sub-counties, importance ranks
  scs <- unique(d$uid)
  ranks <- rbindlist(lapply(1:5, function(b) {
    s <- sample(scs, length(scs), replace = TRUE)
    db <- rbindlist(lapply(s, function(u) d[uid == u]))
    vi <- fit(db, "impurity")$variable.importance
    vi <- vi[!names(vi) %in% c("usual", "prev")]        # rank the other factors among themselves
    data.table(feature = names(vi), rank = frank(-vi, ties.method = "first"), b = b)
  }))
  stab <- ranks[, .(top3 = sum(rank <= 3), mean_rank = mean(rank)), by = feature]
  # 3. final model on all months; SHAP for the last 12 months of every sub-county
  m <- fit(d)
  Xall <- as.data.frame(d[, names(FEATURES), with = FALSE])
  bg <- Xall[sample(nrow(Xall), min(400, nrow(Xall))), , drop = FALSE]
  ex <- d[period >= sort(unique(d$period), decreasing = TRUE)[min(12, uniqueN(d$period))]]
  base <- mean(predict(m, bg, num.threads = 2)$predictions)
  S <- shap_rows(m, as.data.frame(ex[, names(FEATURES), with = FALSE]), bg)
  pred <- predict(m, as.data.frame(ex[, names(FEATURES), with = FALSE]), num.threads = 2)$predictions
  sh <- cbind(ex[, .(uid, period, y, usual_level = usual_raw, last_month = prev_raw, pop, num, den)],
              pred = expm1(pred), pred_log = pred, base = base, as.data.table(S))
  sh <- merge(sh, sc[, .(uid, district_uid, district)], by = "uid")
  vals <- cbind(ex[, .(uid, period)], ex[, names(FEATURES), with = FALSE])
  vals[, `:=`(usual = ex$usual_raw, prev = ex$prev_raw)]                  # show inputs in their own units
  imp <- data.table(feature = names(FEATURES), mean_abs = colMeans(abs(S)))[order(-mean_abs)]
  imp <- merge(imp, stab, by = "feature", all.x = TRUE)[order(-mean_abs)]
  imp[, `:=`(label = FEATURES[feature], stable = top3 >= 4)]
  # direction of each input: rank correlation between its value and its SHAP contribution
  imp[, direction := vapply(feature, function(f) { r <- suppressWarnings(cor(vals[[f]], S[, f], method = "spearman"))
                                                   if (!is.finite(r) || abs(r) < 0.2) "mixed" else if (r > 0) "up" else "down" }, "")]
  res[[o$id]] <- list(meta = o, n_rows = nrow(d), n_subcounties = uniqueN(d$uid), from = min(d$period), to = last,
                      test_from = cut, r2 = r2, r2_usual = r2_usual, r2_prev = r2_prev, mae = mae, base = base,
                      importance = imp, shap = sh, values = vals)
  log_msg("%s: %d rows, R2 (log scale) on the last 12 months %.2f (usual level alone %.2f, last month alone %.2f), %.0fs", o$id, nrow(d), r2, r2_usual, r2_prev,
          as.numeric(Sys.time() - t0, units = "secs"))
}
saveRDS(list(features = FEATURES, outcomes = res, built = format(Sys.Date(), "%d %B %Y")), "app/data/xai.rds", compress = "xz")
log_msg("explainable AI results written for %d outcomes", length(res))
