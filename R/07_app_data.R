# 07_app_data.R
# Final small files the app reads: indicators.csv, orgunits.csv (with facility groups), meta.json.

source("R/00_config.R")
defs <- fread("data/meta/indicator_defs.csv")
im   <- readRDS("app/data/ind_month.rds")
has  <- unique(as.character(im$code))
drop <- setdiff(defs$code, has)
if (length(drop)) log_msg("indicators with no data in Busoga, left out of the app: %s", paste(drop, collapse = ", "))
# Series that stopped (e.g. data elements retired when the HMIS 105 form was revised in July 2025):
# the last month in which Busoga's numerator was at least 20% of its 2024 monthly median. Months
# after it are not shown, so a handful of late entries is never read as a collapse.
rg <- im[level == "region", .(n = sum(num, na.rm = TRUE)), by = .(code = as.character(code), period)]
last_m <- max(rg$period)
ended <- rg[, {
  base <- median(n[period %/% 100L == 2024L]); ok <- period[n >= 0.2 * base]
  .(ended = if (is.na(base) || base <= 0 || !length(ok)) NA_integer_ else if (max(ok) <= last_m - 3L) max(ok) else NA_integer_)
}, by = code]
defs <- merge(defs, ended, by = "code", all.x = TRUE, sort = FALSE)
if (any(!is.na(defs$ended))) log_msg("series that ended: %s", paste(sprintf("%s (%d)", defs[!is.na(ended), code], defs[!is.na(ended), ended]), collapse = ", "))
fwrite(defs[code %in% has, .(code, theme, label, dhis2_uid, dhis2_name, direction, note, factor, annualized,
                             area_only, unit, num_desc, den_desc, ended)], "app/data/indicators.csv")

ou  <- fread("data/meta/orgunits.csv", na.strings = "")
grp <- fread("data/meta/facility_groups.csv")
w <- dcast(grp, uid ~ set, value.var = "group")
setnames(w, setdiff(names(w), "uid"), paste0("grp_", setdiff(names(w), "uid")))
ou <- merge(ou, w, by = "uid", all.x = TRUE)
ou[level == 2, `:=`(name = "Busoga")]
fwrite(ou[, .(uid, name, level, level_name, uid_l3, uid_l4, uid_l5, district, dlg, subcounty,
              opening_date, closed_date, lon, lat, grp_ownership, grp_level, grp_authority)], "app/data/orgunits.csv")

val <- if (file.exists("data/meta/validation_summary.rds")) readRDS("data/meta/validation_summary.rds")
meta <- list(region_uid = cfg$region_uid, extracted = format(Sys.Date(), "%d %B %Y"),
             datasets = as.list(cfg$datasets), validation = val)
jsonlite::write_json(meta, "app/data/meta.json", auto_unbox = TRUE, pretty = TRUE)
log_msg("app data ready: %d indicators, %d org units", length(has), nrow(ou))

# ---- readable formulas and data-element lists for the definitions page ---------------------
ops <- fread("data/meta/operands.csv", na.strings = "")
coc_tab <- fread("data/meta/coc.csv")
ops[, coc_name := coc_tab$coc_name[match(ops$coc, coc_tab$coc)]]
ops[, label := fifelse(is.na(coc_name), de_name, sprintf("%s (%s)", de_name, coc_name))]
tok_re <- "#[{][^}]+[}]"
tokens_in <- function(e) unique(regmatches(e, gregexpr(tok_re, e))[[1]])
readable <- function(e) {
  toks <- tokens_in(e)
  if (length(toks) > 10) {                       # long sums: name the data elements, count the categories
    d <- ops[token %in% toks, .(n = .N), by = de_name]
    return(paste0("Sum of ", paste(sprintf("%s%s", d$de_name, ifelse(d$n > 1, sprintf(" [%d categories]", d$n), "")), collapse = " + ")))
  }
  for (t in toks) { lab <- ops$label[match(t, ops$token)]; e <- gsub(t, sprintf("[%s]", if (is.na(lab)) t else lab), e, fixed = TRUE) }
  gsub("[[:space:]]+", " ", e)
}
defs_app <- defs[code %in% has]
form <- defs_app[, .(code, num_readable = vapply(num, readable, ""), den_readable = vapply(den, readable, ""))]
fwrite(form, "app/data/indicator_formula.csv")
el <- rbindlist(lapply(seq_len(nrow(defs_app)), function(i) {
  d <- defs_app[i]
  rbind(data.table(code = d$code, part = "Numerator", token = tokens_in(d$num)),
        data.table(code = d$code, part = "Denominator", token = tokens_in(d$den)))
}))
el <- merge(el, ops[, .(token, de, de_name, category = fifelse(is.na(coc_name), "All categories", coc_name), period_type)], by = "token")
fwrite(unique(el[, .(code, part, de, de_name, category, period_type)]), "app/data/indicator_elements.csv")
log_msg("definitions: %d readable formulas, %d data-element rows", nrow(form), nrow(el))

# ---- UBOS subnational population (2023 projections, census base; via OCHA HDX, CC BY-IGO) --------
if (file.exists("data/ubos/uga_admpop_adm2_2023.csv")) {
  ub <- fread("data/ubos/uga_admpop_adm2_2023.csv")
  busoga <- c("Bugiri", "Bugweri", "Buyende", "Iganga", "Jinja", "Kaliro", "Kamuli", "Luuka", "Mayuge", "Namayingo", "Namutumba")
  ub <- ub[ADM2_EN %in% busoga]
  age_cols <- grep("^[FM]_[0-9]", names(ub), value = TRUE)
  long <- melt(ub, id.vars = "ADM2_EN", measure.vars = age_cols, variable.name = "col", value.name = "pop")
  long[, `:=`(sex = fifelse(substr(col, 1, 1) == "F", "Female", "Male"),
              age_start = as.integer(substr(col, 3, 4)))]              # F_00_04 -> 0, M_80Plus -> 80
  long[, age := fifelse(grepl("Plus", col), sprintf("%d+", age_start), sprintf("%d-%d", age_start, age_start + 4L))]
  saveRDS(long[, .(district = ADM2_EN, sex, age_start, age, pop = as.numeric(pop))], "app/data/ubos_pop.rds", compress = "xz")
  log_msg("UBOS population: %d districts, total %s", uniqueN(long$ADM2_EN), format(sum(long$pop), big.mark = ","))
}
