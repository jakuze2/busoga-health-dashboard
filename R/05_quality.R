# 05_quality.R
# Data quality per facility (reporting status, completeness, timeliness, consistency,
# outliers, composite score) and age/sex breakdowns. Writes app/data/dq_*.rds and breakdown.rds.

source("R/00_config.R")
defs <- fread("data/meta/indicator_defs.csv")
ou   <- fread("data/meta/orgunits.csv", na.strings = "")
im   <- readRDS("app/data/ind_month.rds"); im[, `:=`(level = as.character(level), code = as.character(code))]
rep  <- readRDS("app/data/reporting.rds")

last_m <- max(im$period)
m_seq  <- months_between(cfg$start_month, as.Date(sprintf("%d-%02d-01", last_m %/% 100, last_m %% 100)))
last12 <- as.integer(tail(m_seq, 12)); last6 <- as.integer(tail(m_seq, 6))
fac <- ou[level == 6, uid]

# ---- reporting status (105:01 OPD monthly report) ------------------------------------
r1 <- rep[dataset == "RtEYsASU7PG" & uid %in% fac]
st <- r1[, .(exp_all = sum(expected), act_all = sum(actual),
             exp12 = sum(expected[period %in% last12]), act12 = sum(actual[period %in% last12]),
             ot12 = sum(on_time[period %in% last12]),
             exp6 = sum(expected[period %in% last6]), act6 = sum(actual[period %in% last6]),
             last_report = if (any(actual > 0)) max(period[actual > 0]) else NA_integer_,
             first_report = if (any(actual > 0)) min(period[actual > 0]) else NA_integer_), by = uid]
st <- merge(data.table(uid = fac), st, by = "uid", all.x = TRUE)
for (cc in c("exp_all", "act_all", "exp12", "act12", "ot12", "exp6", "act6")) set(st, which(is.na(st[[cc]])), cc, 0)
st[, report_status := fcase(
  exp_all == 0 | exp12 == 0, "Not expected to report",
  act_all == 0, "Never reported",
  exp6 > 0 & act6 == 0, "Stopped reporting",
  act12 / exp12 >= 0.75, "Regular reporter",
  default = "Intermittent")]
st[, `:=`(completeness_12m = fifelse(exp12 > 0, pmin(100, 100 * act12 / exp12), NA_real_),
          timeliness_12m = fifelse(exp12 > 0, pmin(100, 100 * ot12 / exp12), NA_real_))]

# ---- consistency checks on facility-month values -------------------------------------
fm <- im[level == "facility"]
fm <- merge(fm, defs[, .(code, label, unit, area_only)], by = "code")
# Only indicators whose numerator is a true subset of the denominator in the same month and
# facility are checked. Many MoH "%" indicators are proxies (e.g. IPT3 per ANC 1 attendee: the
# women receiving IPT3 are not the women at their first visit), so above 100% is not an error there.
bounded <- intersect(c("ANC03", "ANC04", "ANC05", "ANC06", "ANC07", "ANC10", "DEL09", "DEL11",
                       "MAL02", "MAL06", "MAL07", "HIV03", "HIV05"), defs$code)
# (MAL01 testing rate is excluded: its numerator counts all lab tests, its denominator only OPD fevers)
c1 <- fm[code %in% bounded & !is.na(num) & !is.na(den) & den > 0 & num > den,
         .(uid, period, code, check = paste(label, "above 100%"),
           detail = sprintf("numerator %s > denominator %s", format(num, big.mark = ","), format(den, big.mark = ",")))]
c2 <- fm[code %in% bounded & !is.na(num) & num > 0 & (is.na(den) | den == 0),
         .(uid, period, code, check = paste0(label, ": numerator without denominator"),
           detail = sprintf("numerator %s, denominator 0 or blank", format(num, big.mark = ",")))]
# negative dropouts on annual totals (monthly cohorts do not line up); BCG to MR 1 is left out
# because BCG is given where babies are born and MR 1 wherever they are taken at 9 months
dropouts <- intersect(c("ANC11", "ANC12", "EPI10", "EPI12"), defs$code)
dy <- fm[code %in% dropouts & !is.na(num) & !is.na(den), .(num = sum(num), den = sum(den), n = .N),
         by = .(uid, code, label, year = period %/% 100L)][n >= 6 & den >= 20]
c3 <- dy[num < 0, .(uid, period = year * 100L + 12L, code, check = paste(label, "negative (annual)"),
                    detail = sprintf("%d: later dose exceeds earlier dose by %s", year, format(-num, big.mark = ",")))]
chk <- rbind(c1, c2, c3)
log_msg("consistency flags: %s facility-indicator-months", format(nrow(chk), big.mark = ","))

fm12 <- fm[period %in% last12 & !is.na(num), .(fm_n = uniqueN(period)), by = uid]
fl12 <- chk[period %in% last12, .(flags_12m = .N, flagged_months = uniqueN(period)), by = uid]

# ---- outliers (modified z-score on each facility's own monthly series) ----------------
count_items <- intersect(c("DEL01", "SRV01", "MAL09", "HIV01", "HIV02", "PNC07", "SRV03", "HIV07"), defs$code)
ot <- fm[code %in% count_items & !is.na(num), .(uid, period, code, label, x = num)]
ot[, `:=`(med = median(x), mad = mad(x, constant = 1), n = .N), by = .(uid, code)]
ot <- ot[n >= 12 & mad > 0]
ot[, z := 0.6745 * (x - med) / mad]
out <- ot[abs(z) > 3.5 & abs(x - med) >= 10, .(uid, period, code, item = label, value = x, median = med, z)]
log_msg("outlier values: %s", format(nrow(out), big.mark = ","))
ov12 <- ot[period %in% last12, .(vals12 = .N, outliers_12m = sum(abs(z) > 3.5 & abs(x - med) >= 10)), by = uid]

# ---- composite -----------------------------------------------------------------------
dq <- Reduce(function(a, b) merge(a, b, by = "uid", all.x = TRUE), list(st, fm12, fl12, ov12))
for (cc in c("fm_n", "flags_12m", "flagged_months", "vals12", "outliers_12m")) set(dq, which(is.na(dq[[cc]])), cc, 0L)
dq[, consistency_12m := fifelse(fm_n > 0, 100 * (1 - flagged_months / fm_n), NA_real_)]
dq[, nonoutlier_12m := fifelse(vals12 > 0, 100 * (1 - outliers_12m / vals12), NA_real_)]
dq[, dq_score := fifelse(report_status == "Not expected to report", NA_real_,
                         rowMeans(cbind(completeness_12m, timeliness_12m, consistency_12m, nonoutlier_12m), na.rm = TRUE))]
dq[report_status == "Never reported", dq_score := 0]

saveRDS(dq[, .(uid, report_status, last_report, first_report, completeness_12m, timeliness_12m, consistency_12m,
               nonoutlier_12m, dq_score, flags_12m, outliers_12m)], "app/data/dq_facility.rds", compress = "xz")
saveRDS(chk[, .(uid, period, code, check, detail)], "app/data/dq_checks.rds", compress = "xz")
saveRDS(out, "app/data/dq_outliers.rds", compress = "xz")
log_msg("reporting status: %s", paste(capture.output(print(dq[, .N, by = report_status])), collapse = " | "))

# ---- age / sex breakdowns ------------------------------------------------------------
coc <- fread("data/meta/coc.csv")
ops <- fread("data/meta/operands.csv", na.strings = "")
grp_des <- function(cd) unique(ops$de[vapply(ops$token, grepl, TRUE, x = defs$num[defs$code == cd], fixed = TRUE)])
items <- list("OPD new attendances" = "sv6SeKroHPV", "Confirmed malaria cases" = "wUDxFVBapIc",
              "ANC 1st visits" = "Q9nSogNmKPt", "Deliveries in unit" = "idXOxt69W0e",
              "HIV tests performed" = grp_des("HIV01"), "New HIV positives" = grp_des("HIV02"),
              "New positives linked to care" = unique(ops$de[vapply(ops$token, grepl, TRUE,
                                                     x = defs[code == "HIV04", num], fixed = TRUE)]))
raw <- rbindlist(lapply(list.files("data/raw/de", pattern = "rds$", full.names = TRUE), readRDS), fill = TRUE)
raw <- raw[de %in% unlist(items) & !is.na(value)]
raw <- merge(raw, coc, by = "coc", all.x = TRUE)
parse_coc <- function(nm) {
  parts <- strsplit(nm, ",\\s*")
  sex <- vapply(parts, function(p) { s <- p[p %in% c("Male", "Female")]; if (length(s)) s[1] else "Total" }, "")
  age <- vapply(parts, function(p) { a <- p[!p %in% c("Male", "Female")]; if (length(a)) paste(a, collapse = ", ") else "All ages" }, "")
  age[age %in% c("default", "")] <- "All ages"
  list(sex = sex, age = trimws(age))
}
age_days <- function(a) {
  n <- suppressWarnings(as.numeric(sub("^[^0-9]*([0-9]+(\\.[0-9]+)?).*$", "\\1", a)))
  n[grepl("^\\s*<", a)] <- 0
  mult <- ifelse(grepl("day", a, ignore.case = TRUE) & !grepl("yr|year|mth|month", sub("^[^-]*-", "", a), ignore.case = TRUE) &
                   grepl("^[^-]*day", a, ignore.case = TRUE), 1,
          ifelse(grepl("^[^-]*(mth|month)", a, ignore.case = TRUE), 30, 365))
  out <- n * mult; out[is.na(out)] <- 1e6; out
}
pc <- parse_coc(raw$coc_name); raw[, `:=`(sex = pc$sex, age = pc$age)]
raw[, item := names(items)[vapply(de, function(d) which(vapply(items, function(v) d %in% v, TRUE))[1], 1L)]]
raw[, year := as.integer(substr(period, 1, 4))]
f_par <- ou[level == 6, .(uid, l5 = uid_l5, l4 = uid_l4, l3 = uid_l3)]
b <- merge(raw[, .(value = sum(value)), by = .(item, uid, year, age, sex)], f_par, by = "uid")
brk <- rbind(b[, .(uid, item, year, age, sex, value)],
             b[, .(value = sum(value)), by = .(uid = l5, item, year, age, sex)],
             b[, .(value = sum(value)), by = .(uid = l4, item, year, age, sex)],
             b[, .(value = sum(value)), by = .(uid = l3, item, year, age, sex)],
             b[, .(value = sum(value), uid = cfg$region_uid), by = .(item, year, age, sex)])
brk[, age_order := age_days(age)]
saveRDS(brk, "app/data/breakdown.rds", compress = "xz")
log_msg("breakdown rows: %s; items: %s", format(nrow(brk), big.mark = ","), paste(unique(brk$item), collapse = "; "))
