# 02_indicators.R
# Resolve every row of data/meta/indicator_catalogue.csv into a numerator and
# denominator expression over DHIS2 data elements, using the live MoH definitions.
#   official rows: dhis2_uid -> that indicator's numerator, denominator, factor, annualised flag
#   BHF-defined rows: N{uid} = numerator of MoH indicator uid, G{uid} = sum of the
#                     numerators of every indicator in indicator group uid
# Writes data/meta/indicator_defs.csv and data/meta/operands.csv.

source("R/00_config.R")

`%||%` <- function(a, b) if (is.null(a)) b else a
cat_ <- fread("data/meta/indicator_catalogue.csv", na.strings = "")
cat_[, `:=`(num = as.character(num), den = as.character(den))]

get_ind <- function(uid) d2_get(paste0("indicators/", uid),
  fields = "id,name,numerator,denominator,numeratorDescription,denominatorDescription,annualized,indicatorType[factor]")

defs <- rbindlist(lapply(seq_len(nrow(cat_)), function(i) {
  r <- cat_[i]
  if (!is.na(r$dhis2_uid)) {
    x <- get_ind(r$dhis2_uid)
    data.table(code = r$code, dhis2_name = x$name, num = x$numerator, den = x$denominator,
               factor = x$indicatorType$factor, annualized = isTRUE(x$annualized),
               num_desc = x$numeratorDescription %||% "", den_desc = x$denominatorDescription %||% "")
  } else {
    data.table(code = r$code, dhis2_name = NA_character_, num = r$num, den = r$den,
               factor = r$factor, annualized = isTRUE(as.logical(r$annualized)),
               num_desc = "", den_desc = "")
  }
}))


# expand N{uid} and G{uid}
expand <- function(e) {
  e <- gsub("\\s+", " ", e)
  for (u in unique(regmatches(e, gregexpr("(?<=N\\{)[A-Za-z0-9]{11}(?=\\})", e, perl = TRUE))[[1]]))
    e <- gsub(paste0("N{", u, "}"), paste0("(", get_ind(u)$numerator, ")"), e, fixed = TRUE)
  for (u in unique(regmatches(e, gregexpr("(?<=G\\{)[A-Za-z0-9]{11}(?=\\})", e, perl = TRUE))[[1]])) {
    g <- d2_get(paste0("indicatorGroups/", u), fields = "indicators[numerator]")$indicators$numerator
    e <- gsub(paste0("G{", u, "}"), paste0("(", paste0("(", g, ")", collapse = " + "), ")"), e, fixed = TRUE)
  }
  e
}
defs[, `:=`(num = vapply(num, expand, ""), den = vapply(den, expand, ""))]

# reject anything we cannot evaluate exactly (only #{..}, numbers, + - * / ( ) allowed)
check <- function(e) {
  s <- gsub("#\\{[^}]+\\}", "", e)
  !grepl("[A-Za-z{}]", s)
}
bad <- defs[!(vapply(num, check, TRUE) & vapply(den, check, TRUE))]
if (nrow(bad)) {
  log_msg("dropping %d indicators with unsupported expressions: %s", nrow(bad), paste(bad$code, collapse = ", "))
  defs <- defs[!code %in% bad$code]
}

# operands: #{de}, #{de.coc}, #{de.coc.aoc}, #{de.*.aoc}; attribute option combos are ignored (total)
ops <- unique(unlist(regmatches(paste(defs$num, defs$den), gregexpr("#\\{[^}]+\\}", paste(defs$num, defs$den)))))
op_dt <- data.table(token = ops)
op_dt[, inner := gsub("^#\\{|\\}$", "", token)]
op_dt[, c("de", "coc") := tstrsplit(inner, ".", fixed = TRUE, keep = 1:2)]
op_dt[coc %in% c("*", ""), coc := NA]

# data element metadata: aggregation type and dataset period types
des <- unique(op_dt$de)
meta <- rbindlist(lapply(chunk(des, 100), function(ch) {
  x <- d2_get("dataElements", filter = paste0("id:in:[", paste(ch, collapse = ","), "]"),
              fields = "id,name,code,aggregationType,dataSetElements[dataSet[periodType]]", paging = "false")$dataElements
  data.table(de = x$id, de_name = x$name, aggregation = x$aggregationType,
             period_type = vapply(x$dataSetElements, function(d)
               if (is.null(d) || !length(d$dataSet$periodType)) NA_character_ else {
                 pt <- unique(d$dataSet$periodType); if ("Monthly" %in% pt) "Monthly" else pt[1] }, ""))
}))
op_dt <- merge(op_dt, meta, by = "de", all.x = TRUE)
missing <- op_dt[is.na(de_name), unique(de)]
if (length(missing)) log_msg("WARNING %d operand data elements not found: %s", length(missing), paste(missing, collapse = ","))
op_dt[, operand_kind := fifelse(period_type %in% "Yearly", "population", "routine")]

area_des <- op_dt[operand_kind == "population", unique(de)]
defs[, area_only := vapply(paste(num, den), function(e) any(vapply(area_des, grepl, TRUE, x = e, fixed = TRUE)), TRUE)]
defs <- merge(cat_[, .(code, theme, label, dhis2_uid, direction, note)], defs, by = "code", sort = FALSE)
defs[, unit := fcase(factor == 100, "%", factor == 1000, "per 1,000", factor == 100000, "per 100,000",
                     factor == 1 & den %in% c("1", "(1)"), "count", default = "ratio")]

fwrite(defs, "data/meta/indicator_defs.csv")
fwrite(op_dt[, .(token, de, coc, de_name, aggregation, period_type, operand_kind)], "data/meta/operands.csv")
log_msg("indicators resolved: %d (area-only: %d); operands: %d over %d data elements (%d routine, %d population)",
        nrow(defs), sum(defs$area_only), nrow(op_dt), uniqueN(op_dt$de),
        op_dt[operand_kind == "routine", uniqueN(de)], op_dt[operand_kind == "population", uniqueN(de)])
print(op_dt[, .N, by = .(aggregation, period_type)])
