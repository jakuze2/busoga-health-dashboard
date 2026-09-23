# 03c_extract_supplies.R
# Medicines, supplies and facility management from the HMIS 105:06-09 monthly report
# (dataset VDhwrW9DiC1), per facility and month:
#   * 61 items (66 data elements): days out of stock (SSxxb), in both the 2019 and the revised form; items are
#     matched across form versions in data/meta/supply_items.csv. (Stock on hand, SSxxc, is not
#     open to analytics for this account, so a stock-out means one or more days out of stock.)
#   * support supervision (SV01-05) and management meetings (MT02-05): planned and conducted
#   * whether the facility submitted the report that month (the stock-out denominator)
# Writes app/data/supplies.rds. Raw pulls are kept in data/raw/supplies/ (one file per year and
# chunk), so a monthly refresh re-pulls only the last 12 months.
#
#   Rscript R/03c_extract_supplies.R            # pull what is missing
#   Rscript R/03c_extract_supplies.R --refresh  # re-pull the most recent 12 months

source("R/00_config.R")
refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
DS <- "VDhwrW9DiC1"
dir.create("data/raw/supplies", recursive = TRUE, showWarnings = FALSE)

# ---- metadata ---------------------------------------------------------------------------------
de <- as.data.table(d2_get("dataElements", filter = paste0("dataSetElements.dataSet.id:eq:", DS),
                           fields = "id,name", paging = "false")$dataElements)
ss <- de[grepl("^105-SS\\d+b", name)]
ss[, `:=`(code = sub("^105-(SS\\d+).*", "\\1", name), form = fifelse(grepl("_2019", name), 2019L, 2025L))]
items <- fread("data/meta/supply_items.csv")
ss <- merge(ss, items, by = c("code", "form"))
items <- unique(items[, .(item, group, tracer)], by = "item")
mgmt <- de[grepl("^105-(SV0[1-5]|MT0[2-5])[ab]\\.", name)]
MG_LAB <- c(SV01 = "Supervision by the Ministry of Health", SV02 = "Supervision by regional teams",
            SV03 = "Supervision by the local government", SV04 = "Supervision by the health sub-district",
            SV05 = "Other support supervision", MT02 = "Quality improvement meetings",
            MT03 = "Maternal and perinatal death reviews", MT04 = "Health unit management committee / board meetings",
            MT05 = "Community accountability and client feedback meetings")
mgmt[, `:=`(what = MG_LAB[sub("^105-([A-Z]+\\d+).*", "\\1", name)],
            kind = fifelse(grepl("^105-[A-Z]+\\d+a", name), "planned", "conducted"))]
log_msg("supplies: %d stock elements for %d items; %d management elements", nrow(ss), nrow(items), nrow(mgmt))

# ---- data, facility x month ---------------------------------------------------------------------
months <- months_between(); years <- unique(substr(months, 1, 4)); recent <- tail(months, 12)
ou_fac <- paste0("ou:", cfg$region_uid, ";LEVEL-", cfg$levels[["facility"]])
# elements this account may not read return 401 for the whole request: split and skip them
pull <- function(ids, pe) {
  r <- tryCatch(d2_analytics(c(paste0("dx:", paste(ids, collapse = ";")), paste0("pe:", paste(pe, collapse = ";")), ou_fac)),
                error = function(e) if (grepl("401|Unauthori", conditionMessage(e))) NULL else stop(e))
  if (!is.null(r)) return(r)
  if (length(ids) == 1) { log_msg("  not readable, skipped: %s", ids); return(data.table()) }
  h <- ceiling(length(ids) / 2)
  rbind(pull(ids[1:h], pe), pull(ids[-(1:h)], pe), fill = TRUE)
}
chunks <- c(chunk(c(ss$id, mgmt$id), 20), list(paste0(DS, ".ACTUAL_REPORTS")))
for (y in years) {
  pe <- months[substr(months, 1, 4) == y]
  if (refresh && !any(pe %in% recent)) next
  for (k in seq_along(chunks)) {
    f <- sprintf("data/raw/supplies/sup_%s_%02d.rds", y, k)
    if (file.exists(f) && !refresh) next
    dt <- pull(chunks[[k]], pe)
    if (nrow(dt)) setnames(dt, c("dx", "pe", "ou"), c("de", "period", "uid"))
    saveRDS(dt, f, compress = "xz")
    log_msg("%s: %s rows", basename(f), format(nrow(dt), big.mark = ","))
  }
}
raw <- rbindlist(lapply(list.files("data/raw/supplies", pattern = "^sup_", full.names = TRUE), readRDS), fill = TRUE)
raw <- unique(raw[!is.na(value)], by = c("de", "uid", "period"))
raw[, period := as.integer(period)]

reports <- raw[de == paste0(DS, ".ACTUAL_REPORTS") & value > 0, .(uid, period)]

# days out of stock per facility, item and month (both form versions in one month: the larger)
st <- merge(raw, ss[, .(de = id, item)], by = "de")
st <- st[, .(days = as.integer(round(min(31, max(0, value))))), by = .(uid, period, item)]
# Many facilities report 30 days out of stock, month after month, for items they never hold. An
# item counts for a facility in a month only if the facility had it in stock (fewer than 28 days
# out) in at least one of the three months before; stock-out rates use those facility-months.
st[, mi := (period %/% 100L) * 12L + period %% 100L]
had <- st[days < 28, .(uid, item, mi)]
st[, managed := FALSE]
for (k in 1:3) st[copy(had)[, mi := mi + k], on = .(uid, item, mi), managed := TRUE]
log_msg("supplies: %s of %s facility-item-months are for items the facility stocks", format(sum(st$managed), big.mark = ","), format(nrow(st), big.mark = ","))
st <- st[managed == TRUE, .(uid, period, item, days)]
st[, item := factor(item, levels = items$item)]

mg <- merge(raw, mgmt[, .(de = id, what, kind)], by = "de")
mg <- dcast(mg, uid + period + what ~ kind, value.var = "value", fun.aggregate = sum, fill = 0)
for (v in c("planned", "conducted")) if (!v %in% names(mg)) mg[, (v) := 0]

saveRDS(list(items = items, stock = st, reports = reports, mgmt = mg, built = format(Sys.Date(), "%d %B %Y")),
        "app/data/supplies.rds", compress = "xz")
log_msg("supplies written: %s facility-item-months, %s reports, %s management rows",
        format(nrow(st), big.mark = ","), format(nrow(reports), big.mark = ","), format(nrow(mg), big.mark = ","))
