# 04_compute.R
# Turn the raw extract into the dashboard data (app/data/*.rds).
#
# How indicator values are computed (mirrors DHIS2 analytics):
#   1. operand value per facility-month = the data element value for that category
#      option combo, or the sum over all combos when the operand has no combo
#   2. numerator and denominator expressions are evaluated per facility-month; a
#      missing operand counts as 0 unless every operand of that expression is missing
#   3. all expressions are linear sums, so numerators and denominators add up exactly
#      from facility to sub-county, DLG, district and region
#   4. population denominators (107a projected population, yearly, sub-county) are
#      evaluated at area level only; the indicator is then annualised as in DHIS2
#   value = num / den * factor  (x 12 / months in the period when annualised)

source("R/00_config.R")

defs <- fread("data/meta/indicator_defs.csv")
ops  <- fread("data/meta/operands.csv", na.strings = "")
ou   <- fread("data/meta/orgunits.csv", na.strings = "")
fac  <- ou[level == 6]

# ---- load raw ---------------------------------------------------------------------
raw <- rbindlist(lapply(list.files("data/raw/de", pattern = "rds$", full.names = TRUE), readRDS), fill = TRUE)
raw <- raw[!is.na(value)]
raw <- unique(raw, by = c("de", "coc", "period", "uid"))     # safety: a value pulled twice counts once
raw[, period := as.integer(period)]
log_msg("raw data values: %s", format(nrow(raw), big.mark = ","))

# ---- operand values per facility-month ----------------------------------------------
op_r <- ops[operand_kind == "routine" & !is.na(de_name)]
with_coc <- op_r[!is.na(coc)]
no_coc   <- op_r[is.na(coc)]
v1 <- raw[with_coc, on = .(de, coc), nomatch = NULL, .(token = i.token, uid, period, value)]
v2 <- raw[de %in% no_coc$de, .(value = sum(value)), by = .(de, uid, period)][
  no_coc, on = "de", nomatch = NULL, .(token = i.token, uid, period, value)]
opv <- rbind(v1, v2)
setkey(opv, token)
log_msg("operand values: %s", format(nrow(opv), big.mark = ","))

# ---- expression evaluation ----------------------------------------------------------
tokens_of <- function(e) unique(regmatches(e, gregexpr("#\\{[^}]+\\}", e))[[1]])
to_fun <- function(e) {
  toks <- tokens_of(e)
  function(x) { for (k in seq_along(toks)) e <- gsub(toks[k], sprintf("(%.10f)", x[k]), e, fixed = TRUE)
                eval(parse(text = e)) }
}
# numeric test: f(a x + b y) == a f(x) + b f(y) for random operand vectors (linear, no offset)
is_linear <- function(e) {
  n <- length(tokens_of(e)); if (!n) return(TRUE)
  f <- to_fun(e); set.seed(1); x <- runif(n, 1, 9); y <- runif(n, 1, 9)
  isTRUE(all.equal(f(2 * x + 3 * y), 2 * f(x) + 3 * f(y), tolerance = 1e-8)) &&
    isTRUE(all.equal(f(rep(0, n)), 0))
}
to_r <- function(e, toks) {
  for (k in seq_along(toks)) e <- gsub(toks[k], sprintf("`t%d`", k), e, fixed = TRUE)
  parse(text = e)[[1]]
}
# evaluate expression e over a long table of operand values -> (uid, period, value)
eval_expr <- function(e, vals, keys = c("uid", "period")) {
  toks <- tokens_of(e)
  if (!length(toks)) return(NULL)                     # constant (e.g. "1"): handled by caller
  sub <- vals[token %in% toks]
  if (!nrow(sub)) return(data.table())
  w <- dcast(sub, as.formula(paste(paste(keys, collapse = "+"), "~ token")),
             value.var = "value", fun.aggregate = sum)
  miss <- setdiff(toks, names(w)); if (length(miss)) w[, (miss) := NA_real_]
  setnames(w, toks, sprintf("t%d", seq_along(toks)))
  cols <- sprintf("t%d", seq_along(toks))
  anyv <- w[, Reduce(`|`, lapply(.SD, Negate(is.na))), .SDcols = cols]
  for (cc in cols) set(w, which(is.na(w[[cc]])), cc, 0)
  w[, value := eval(to_r(e, toks))]
  w[!anyv, value := NA_real_]
  w[, c(keys, "value"), with = FALSE]
}

pop_tokens <- ops[operand_kind == "population", token]
nonlin <- defs[!(vapply(num, is_linear, TRUE) & vapply(den, is_linear, TRUE)), code]
if (length(nonlin)) stop("non-linear expressions, cannot aggregate: ", paste(nonlin, collapse = ", "))

fac_parent <- fac[, .(uid, subcounty_uid = uid_l5, dlg_uid = uid_l4, district_uid = uid_l3)]
res <- list()
for (i in seq_len(nrow(defs))) {
  d <- defs[i]
  n <- eval_expr(d$num, opv)
  if (is.null(n) || !nrow(n)) { log_msg("  %s: no numerator data", d$code); next }
  den_toks <- tokens_of(d$den)
  den_is_pop <- length(den_toks) && all(den_toks %in% pop_tokens)
  if (!length(den_toks)) {                       # constant denominator, e.g. "1"
    n[, den := eval(parse(text = d$den))]
  } else if (den_is_pop) {
    n[, den := NA_real_]                         # filled at area level below
  } else {
    dd <- eval_expr(d$den, opv)
    n <- merge(n, dd, by = c("uid", "period"), all = TRUE, suffixes = c("", ".d"))
    setnames(n, "value.d", "den")
  }
  setnames(n, "value", "num")
  n[, code := d$code]
  res[[d$code]] <- n
}
fm <- rbindlist(res, use.names = TRUE)
fm <- fm[!(is.na(num) & is.na(den))]
log_msg("facility-month indicator rows: %s", format(nrow(fm), big.mark = ","))

# ---- aggregate to areas -------------------------------------------------------------
fm <- merge(fm, fac_parent, by = "uid")
agg <- function(by_col, lvl) {
  x <- fm[, .(num = if (all(is.na(num))) NA_real_ else sum(num, na.rm = TRUE),
              den = if (all(is.na(den))) NA_real_ else sum(den, na.rm = TRUE)),
          by = c(by_col, "code", "period")]
  setnames(x, by_col, "uid"); x[, level := lvl]; x
}
areas <- rbind(agg("subcounty_uid", "subcounty"), agg("dlg_uid", "dlg"), agg("district_uid", "district"),
               fm[, .(uid = cfg$region_uid,
                      num = if (all(is.na(num))) NA_real_ else sum(num, na.rm = TRUE),
                      den = if (all(is.na(den))) NA_real_ else sum(den, na.rm = TRUE),
                      level = "region"), by = .(code, period)])
fm[, level := "facility"]
im <- rbind(fm[, .(level, uid, code, period, num, den)], areas[, .(level, uid, code, period, num, den)])

# population denominators (107a projected population), per area and year as DHIS2 aggregates
# them. Sub-county/DLG years that exist only at district level are estimated by scaling the
# unit's last available value with its district's growth (flagged `carried`).
pop <- readRDS("data/raw/population.rds")[, .(pop = sum(value)), by = .(de, uid, year = as.integer(year))]
lvl_of <- setNames(c("region", "district", "dlg", "subcounty")[match(ou$level, 2:5)], ou$uid)
pop[, level := lvl_of[uid]]
yrs <- as.integer(unique(substr(months_between(), 1, 4)))
dist_of <- setNames(ou$uid_l3, ou$uid)
grid <- CJ(uid = ou[level %in% 2:5, uid], de = unique(pop$de), year = yrs)
grid[, level := lvl_of[uid]]
pop_all <- merge(grid, pop, by = c("level", "uid", "de", "year"), all.x = TRUE)
dpop <- pop_all[level == "district", .(d = uid, de, year, dp = pop)]
pop_all[, d := fifelse(level %in% c("dlg", "subcounty"), dist_of[uid], NA_character_)]
pop_all <- merge(pop_all, dpop, by = c("d", "de", "year"), all.x = TRUE)
setorder(pop_all, level, uid, de, year)
pop_all[, carried := is.na(pop)]
pop_all[, `:=`(last_pop = nafill(pop, "locf"), last_dp = nafill(fifelse(is.na(pop), NA_real_, dp), "locf")), by = .(level, uid, de)]
pop_all[is.na(pop) & !is.na(last_pop), pop := fifelse(!is.na(dp) & !is.na(last_dp) & last_dp > 0, last_pop * dp / last_dp, last_pop)]
pop_all <- pop_all[!is.na(pop), .(level, uid, de, year, pop, carried)]
pv <- pop_all[, .(token = paste0("#{", de, "}"), level, uid, year, value = pop)]

for (cd in defs[area_only == TRUE, code]) {
  d  <- defs[code == cd]
  dd <- eval_expr(d$den, pv, keys = c("level", "uid", "year"))
  if (!nrow(dd)) next
  setnames(dd, "value", "pden")
  im[, year := period %/% 100L]
  im[code == cd, den := dd[.SD, on = .(level, uid, year), x.pden]]
  im[, year := NULL]
}
im <- im[!(code %in% defs[area_only == TRUE, code] & level == "facility")]
log_msg("indicator rows, all levels: %s", format(nrow(im), big.mark = ","))

# ---- reporting ----------------------------------------------------------------------
rep <- rbindlist(lapply(list.files("data/raw/reporting", full.names = TRUE), readRDS))
rep <- dcast(rep[metric %in% c("EXPECTED_REPORTS", "ACTUAL_REPORTS", "ACTUAL_REPORTS_ON_TIME")],
             dataset + uid + period ~ metric, value.var = "value", fun.aggregate = sum)
setnames(rep, c("EXPECTED_REPORTS", "ACTUAL_REPORTS", "ACTUAL_REPORTS_ON_TIME"),
         c("expected", "actual", "on_time"), skip_absent = TRUE)
rep[, period := as.integer(period)]

# ---- write app data -----------------------------------------------------------------
dir.create("app/data", showWarnings = FALSE)
im[, `:=`(level = factor(level, c("region", "district", "dlg", "subcounty", "facility")),
          code = factor(code, defs$code))]
setkey(im, code, level, uid, period)
saveRDS(im, "app/data/ind_month.rds", compress = "xz")
saveRDS(rep, "app/data/reporting.rds", compress = "xz")
saveRDS(pop_all[, .(pop = sum(pop), carried = any(carried)), by = .(level, uid, year)],
        "app/data/population.rds", compress = "xz")
log_msg("wrote app/data: ind_month %s rows, reporting %s rows",
        format(nrow(im), big.mark = ","), format(nrow(rep), big.mark = ","))
