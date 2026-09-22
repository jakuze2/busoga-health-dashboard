# 05b_epidemic.R
# Epidemic detection and short-range prediction from weekly 033B data, at facility,
# sub-county, DLG, district and Busoga level.
#   Seasonal / endemic diseases: normal channel from the same weeks (+/- 2) of up to 5 previous years
#     alert threshold    = 3rd quartile of the baseline (WHO malaria epidemic guidance)
#     epidemic threshold = mean + 2 SD of the baseline
#     flagged only with >= 5 cases and >= 3 baseline years (so from 2023 onward)
#     at facility level, thresholds are computed for malaria only (other diseases are too sparse)
#   Immediately notifiable diseases (IDSR): any single case is an alert, at every level.
#   Prediction: next 4 weeks, ARIMA(1,0,0) on log(1 + cases) with the seasonal baseline as
#     regressor; gives the probability that cases exceed the alert threshold.
# Writes app/data/epi_week.rds, epi_forecast.rds, epi_reporting.rds.

source("R/00_config.R")
dis <- fread("data/meta/epidemic_diseases.csv", na.strings = "")
ou  <- fread("data/meta/orgunits.csv", na.strings = "")
w   <- lapply(list.files("data/raw/weekly", pattern = "rds$", full.names = TRUE), readRDS)
v   <- rbindlist(lapply(w, `[[`, "values")); rr <- rbindlist(lapply(w, `[[`, "reporting"))

map <- rbind(dis[, .(dx = cases, disease, kind, measure = "cases")],
             dis[!is.na(deaths), .(dx = deaths, disease, kind, measure = "deaths")])
v <- merge(v, map, by = "dx")
v <- dcast(v, disease + kind + ou + pe ~ measure, value.var = "value", fun.aggregate = sum, fill = 0)
if (!"deaths" %in% names(v)) v[, deaths := 0]

iso_monday <- function(y, wk) { jan4 <- as.Date(sprintf("%d-01-04", y)); jan4 - (as.integer(format(jan4, "%u")) - 1) + 7 * (wk - 1) }
v[, `:=`(year = as.integer(sub("W.*", "", pe)), week = as.integer(sub(".*W", "", pe)))]
v[, week_start := iso_monday(year, week)]

fac <- ou[level == 6, .(ou = uid, l5 = uid_l5, l4 = uid_l4, l3 = uid_l3)]
v <- merge(v, fac, by = "ou")
agg <- function(key, lvl) { x <- v[, .(cases = sum(cases), deaths = sum(deaths)), by = c(key, "disease", "kind", "year", "week", "week_start")]
  setnames(x, key, "uid"); x[, level := lvl]; x }
ew <- rbind(v[, .(uid = ou, disease, kind, year, week, week_start, cases, deaths, level = "facility")],
            agg("l5", "subcounty"), agg("l4", "dlg"), agg("l3", "district"),
            v[, .(cases = sum(cases), deaths = sum(deaths), uid = cfg$region_uid, level = "region"),
              by = .(disease, kind, year, week, week_start)])

# complete the week grid for series with any cases (033B blanks are zeros); facilities: malaria + notifiable only
all_w <- unique(ew[, .(year, week, week_start)])
keys  <- unique(ew[cases > 0 | deaths > 0, .(level, uid, disease, kind)])
keys  <- keys[level != "facility" | disease %in% c("Malaria (confirmed)", "Suspected malaria (fever)") | kind == "notifiable"]
grid  <- keys[, as.list(all_w), by = .(level, uid, disease, kind)]
ew <- merge(grid, ew, by = c("level", "uid", "disease", "kind", "year", "week", "week_start"), all.x = TRUE)
ew[is.na(cases), cases := 0]; ew[is.na(deaths), deaths := 0]
setorder(ew, level, uid, disease, week_start)
log_msg("weekly series: %s rows, %d area-disease series", format(nrow(ew), big.mark = ","), nrow(keys))

# ---- normal channel via a non-equi join (same week +/- 2, previous 5 years) -----------------
seas_ok <- ew$kind == "seasonal" & (ew$level != "facility" | ew$disease == "Malaria (confirmed)")
tgt  <- ew[seas_ok & year >= 2023, .(level, uid, disease, year, week, tid = .I)]
tgt[, `:=`(y_lo = year - 5L, y_hi = year - 1L, w_lo = week - 2L, w_hi = week + 2L)]
base <- ew[seas_ok, .(level, uid, disease, by_ = year, bw = week, b = cases)]
j <- base[tgt, on = .(level, uid, disease, by_ >= y_lo, by_ <= y_hi, bw >= w_lo, bw <= w_hi), nomatch = NULL,
          .(tid, b, byear = x.by_)]
thr <- j[, .(n_years = uniqueN(byear), n = .N, base_median = median(b), alert_thr = quantile(b, .75, names = FALSE),
             epi_thr = mean(b) + 2 * sd(b)), by = tid][n_years >= 3 & n >= 10]
tg <- merge(tgt[, .(tid, level, uid, disease, year, week)], thr, by = "tid")
ew <- merge(ew, tg[, .(level, uid, disease, year, week, base_median, alert_thr, epi_thr)],
            by = c("level", "uid", "disease", "year", "week"), all.x = TRUE)
ew[, status := fcase(
  kind == "notifiable" & cases >= 1, "Notifiable case",
  kind == "seasonal" & !is.na(epi_thr) & cases > epi_thr & cases >= 5, "Epidemic threshold",
  kind == "seasonal" & !is.na(alert_thr) & cases > alert_thr & cases >= 5, "Alert threshold",
  kind == "seasonal" & is.na(alert_thr), "No baseline",
  default = "Normal")]
setorder(ew, level, uid, disease, week_start)
ew[, above := status %in% c("Alert threshold", "Epidemic threshold")]
ew[, run := { r <- rle(above); unlist(mapply(function(l, x) if (x) seq_len(l) else rep(0L, l), r$lengths, r$values)) },
   by = .(level, uid, disease)]
ew[, above := NULL]

# ---- 4-week prediction --------------------------------------------------------------------
predict_unit <- function(d, h = 4) {
  d <- tail(d[order(week_start)], 156)
  if (nrow(d) < 60 || sum(d$cases) < 50) return(NULL)
  bm <- d$base_median
  if (all(is.na(bm))) return(NULL)
  bm[is.na(bm)] <- median(d$cases)
  f <- tryCatch(arima(log1p(d$cases), order = c(1, 0, 0), xreg = log1p(bm), method = "ML"), error = function(e) NULL)
  if (is.null(f)) return(NULL)
  last <- max(d$week_start); fut <- last + 7 * seq_len(h)
  yago <- function(s, col) { b <- d[[col]][abs(as.numeric(d$week_start - (s - 364))) <= 10]; b <- b[!is.na(b)]; if (length(b)) median(b) else NA_real_ }
  fb  <- vapply(fut, yago, 0, col = "cases"); fb[is.na(fb)] <- median(d$cases)
  thr <- vapply(fut, yago, 0, col = "alert_thr")
  thr[is.na(thr)] <- tail(d$alert_thr[!is.na(d$alert_thr)], 1)
  p <- predict(f, n.ahead = h, newxreg = log1p(fb))
  data.table(week_start = fut, mean = expm1(p$pred), lo80 = pmax(0, expm1(p$pred - 1.2816 * p$se)),
             hi80 = expm1(p$pred + 1.2816 * p$se), alert_thr = thr,
             p_exceed = 1 - pnorm((log1p(pmax(thr, 5)) - p$pred) / p$se))
}
fk <- unique(ew[kind == "seasonal" & !is.na(alert_thr) & (level != "facility" | disease == "Malaria (confirmed)"),
                .(level, uid, disease)])
fc <- rbindlist(lapply(seq_len(nrow(fk)), function(i) {
  k <- fk[i]; r <- predict_unit(ew[level == k$level & uid == k$uid & disease == k$disease])
  if (!is.null(r)) r[, `:=`(level = k$level, uid = k$uid, disease = k$disease)]
  r
}))
log_msg("forecasts: %d series (%d facilities)", uniqueN(fc[, .(uid, disease)]), fc[level == "facility", uniqueN(uid)])

# ---- weekly 033B reporting ------------------------------------------------------------------
rr[, metric := sub(".*\\.", "", dx)]
rr <- dcast(rr, ou + pe ~ metric, value.var = "value", fun.aggregate = sum)
rr <- merge(rr, fac, by = "ou")
rr[, week_start := iso_monday(as.integer(sub("W.*", "", pe)), as.integer(sub(".*W", "", pe)))]
ra <- function(key, lvl) { x <- rr[, .(expected = sum(EXPECTED_REPORTS), actual = sum(ACTUAL_REPORTS)), by = c(key, "week_start")]
  setnames(x, key, "uid"); x[, level := lvl]; x }
rep_w <- rbind(rr[, .(uid = ou, week_start, expected = EXPECTED_REPORTS, actual = ACTUAL_REPORTS, level = "facility")],
               ra("l5", "subcounty"), ra("l4", "dlg"), ra("l3", "district"),
               rr[, .(expected = sum(EXPECTED_REPORTS), actual = sum(ACTUAL_REPORTS), uid = cfg$region_uid, level = "region"), by = week_start])

saveRDS(ew[, .(level, uid, disease, kind, week_start, cases, deaths, base_median, alert_thr, epi_thr, status, run)],
        "app/data/epi_week.rds", compress = "xz")
saveRDS(fc, "app/data/epi_forecast.rds", compress = "xz")
saveRDS(rep_w, "app/data/epi_reporting.rds", compress = "xz")
latest <- max(ew$week_start)
log_msg("latest week %s: %d district-level flags", latest,
        ew[week_start == latest & level == "district" & status %in% c("Notifiable case", "Alert threshold", "Epidemic threshold"), .N])
