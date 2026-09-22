# 06_validate.R
# Check our computed values against DHIS2's own indicator results: every official
# (dhis2_uid) indicator, every district plus Busoga, for the last complete calendar year.
# Writes data/meta/validation.csv; the summary goes into app/data/meta.json (07_app_data.R).

source("R/00_config.R")
defs <- fread("data/meta/indicator_defs.csv")
im   <- readRDS("app/data/ind_month.rds"); im[, `:=`(level = as.character(level), code = as.character(code))]
yr   <- as.integer(format(cfg$end_month, "%Y")) - 1L

off <- defs[!is.na(dhis2_uid) & nzchar(dhis2_uid)]
d2 <- rbindlist(lapply(chunk(off$dhis2_uid, 20), function(ch)
  d2_analytics(c(paste0("dx:", paste(ch, collapse = ";")), paste0("pe:", yr),
                 paste0("ou:", cfg$region_uid, ";LEVEL-3")))))
setnames(d2, c("dx", "pe", "ou", "value"), c("dhis2_uid", "year", "uid", "dhis2"))
d2 <- merge(d2, off[, .(dhis2_uid, code)], by = "dhis2_uid")

x <- im[period %/% 100L == yr & level %in% c("district", "region")]
x <- x[, .(num = sum(num, na.rm = TRUE), den_sum = sum(den, na.rm = TRUE), den_mean = mean(den, na.rm = TRUE),
           n = uniqueN(period)), by = .(code, uid)]
x <- merge(x, defs[, .(code, factor, annualized, unit, area_only)], by = "code")
# a full calendar year: annualisation factor is 1; population denominators are averaged, not summed
x[, ours := fifelse(unit == "count", num, num / fifelse(area_only, den_mean, den_sum) * factor)]
v <- merge(d2[, .(code, uid, dhis2)], x[, .(code, uid, ours)], by = c("code", "uid"))
v[, rel_diff := abs(ours - dhis2) / pmax(abs(dhis2), 1e-9)]
# DHIS2 returns some indicators rounded to whole numbers (e.g. dropout rates)
v[, match := rel_diff <= 0.005 | abs(ours - dhis2) < 0.05 | (dhis2 == round(dhis2) & abs(ours - dhis2) <= 0.5)]
fwrite(v, "data/meta/validation.csv")
by_ind <- v[, .(n = .N, matched = sum(match), worst = max(rel_diff)), by = code][order(matched / n)]
log_msg("validation %d: %d of %d indicator-area values match within 0.5%%", yr, sum(v$match), nrow(v))
print(by_ind[matched < n])
saveRDS(list(year = yr, n = uniqueN(v$code), n_values = nrow(v), n_match = sum(v$match),
             ind_all_match = by_ind[matched == n, .N], median_diff = sprintf("%.2f%%", 100 * median(v$rel_diff))),
        "data/meta/validation_summary.rds")
