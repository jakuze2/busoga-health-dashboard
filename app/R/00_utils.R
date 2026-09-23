# Shared data, colours and calculations. Shiny sources every file in R/ before app.R.

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(data.table); library(plotly)
  library(leaflet); library(sf); library(reactable); library(htmltools); library(DT)
})
sf::sf_use_s2(FALSE)

# ---- brand and colour system -------------------------------------------------------
BRAND <- list(navy = "#201B6D", maroon = "#75002C", navy_soft = "#E9E8F4", maroon_soft = "#F6E6EC",
              ink = "#0b0b0b", ink2 = "#52514e", muted = "#898781", grid = "#e1e0d9",
              base = "#c3c2b7", surface = "#fcfcfb", page = "#f7f6f3")

# one fixed hue per programme theme (validated categorical palette, fixed order)
THEMES <- data.table(
  theme = c("Antenatal care", "Delivery & newborn", "Postnatal & family planning", "Immunisation",
            "Child health & nutrition", "Malaria", "HIV & PMTCT", "Services & mortality"),
  # brand-led palette (navy, maroon, teal, gold, plum, forest, indigo, terracotta); passes the
  # lightness, chroma, colour-blind and normal-vision separation checks on white
  color = c("#3949AB", "#A3214A", "#00897B", "#C17D11", "#8E44AD", "#2E7D32", "#5C6BC0", "#C0582B"),
  icon  = c("person-pregnant", "baby", "people-roof", "syringe", "child-reaching", "mosquito",
            "ribbon", "hospital"))
theme_col  <- function(th) THEMES$color[match(th, THEMES$theme)]
theme_icon <- function(th) THEMES$icon[match(th, THEMES$theme)]

# status scale (fixed, always shown with icon + label)
STATUS <- list(good = "#0ca30c", warning = "#fab219", serious = "#ec835a", critical = "#d03b3b",
               none = "#e1e0d9")
STATUS_ICON  <- c(good = "▲", warning = "●", critical = "▼", none = "–")
STATUS_LABEL <- c(good = "Better than Busoga", warning = "Close to Busoga",
                  critical = "Worse than Busoga", none = "No data")

SEQ_BLUE <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
SEQ_RED  <- c("#fde0dd", "#f8b4ad", "#f08880", "#e34948", "#c23433", "#962323", "#6b1414")
# two sequential contexts per indicator direction: "high is good" -> blue, "low is good" -> red
seq_ramp <- function(direction) if (identical(direction, "low")) SEQ_RED else SEQ_BLUE

LEVEL_LABEL <- c(region = "Region", district = "District / City", dlg = "DLG / Municipality",
                 subcounty = "Sub-county / Division", facility = "Health facility")
LEVEL_PLURAL <- c(region = "regions", district = "districts", dlg = "DLGs", subcounty = "sub-counties",
                  facility = "facilities")

# ---- data -----------------------------------------------------------------------
DATA <- "data"
IND  <- fread(file.path(DATA, "indicators.csv"))
IND[, theme := factor(theme, THEMES$theme)]
setorder(IND, theme, code)
OU   <- fread(file.path(DATA, "orgunits.csv"), na.strings = "")
IM   <- readRDS(file.path(DATA, "ind_month.rds"))
REP  <- readRDS(file.path(DATA, "reporting.rds"))
POP  <- readRDS(file.path(DATA, "population.rds"))
DQF  <- if (file.exists(file.path(DATA, "dq_facility.rds"))) readRDS(file.path(DATA, "dq_facility.rds"))
DQC  <- if (file.exists(file.path(DATA, "dq_checks.rds"))) readRDS(file.path(DATA, "dq_checks.rds"))
DQO  <- if (file.exists(file.path(DATA, "dq_outliers.rds"))) readRDS(file.path(DATA, "dq_outliers.rds"))
BRK  <- if (file.exists(file.path(DATA, "breakdown.rds"))) readRDS(file.path(DATA, "breakdown.rds"))
META <- jsonlite::read_json(file.path(DATA, "meta.json"))
IM[, level := as.character(level)]; IM[, code := as.character(code)]
setkey(IM, code, level)
# series that ended when DHIS2 data elements were retired (HMIS 105 revision, July 2025)
END_OF <- setNames(IND$ended, IND$code)
ended_note <- function(code) { e <- END_OF[code]
  ifelse(is.na(e), "", sprintf("Series ended %s (DHIS2 form revised; no successor data element)", format(as.Date(sprintf("%d-%02d-01", e %/% 100L, e %% 100L)), "%b %Y"))) }

MONTHS    <- sort(unique(IM$period))
MONTH_MIN <- min(MONTHS); MONTH_MAX <- max(MONTHS)
ym_date   <- function(p) as.Date(sprintf("%d-%02d-01", p %/% 100L, p %% 100L))
date_ym   <- function(d) as.integer(format(as.Date(d), "%Y%m"))
fmt_month <- function(p) format(ym_date(p), "%b %Y")
month_seq <- function(from, to) date_ym(seq(ym_date(from), ym_date(to), by = "month"))

# default window: the last 12 complete months
DEFAULT_TO   <- MONTH_MAX
DEFAULT_FROM <- date_ym(seq(ym_date(MONTH_MAX), by = "-11 months", length.out = 2)[2])

ou_name  <- setNames(OU$name, OU$uid)
ou_level <- setNames(OU$level_name, OU$uid)
units_at <- function(lvl, within = NULL) {
  x <- OU[level_name == lvl]
  if (length(within) && !is.null(within) && nzchar(within) && within != META$region_uid)
    x <- x[uid_l3 == within | uid_l4 == within | uid_l5 == within]
  setNames(x$uid, x$name)[order(x$name)]
}
parent_chain <- function(uid) {
  i <- match(uid, OU$uid)                            # outside OU[...] so `uid` is the argument
  r <- OU[i[!is.na(i)]]
  if (!nrow(r)) return(character())
  ids <- c(META$region_uid, r$uid_l3, r$uid_l4, r$uid_l5, r$uid)
  ids <- unique(ids[!is.na(ids)])
  ids[seq_len(match(uid, ids))]
}

# ---- indicator values --------------------------------------------------------------
# Value for a set of months, DHIS2-consistent:
#   count indicators      -> sum of numerators
#   rate / ratio          -> sum(num) / sum(den) x factor
#   annualised (population denominators) -> sum(num) / mean(den) x factor x 12 / months
#   population denominators are yearly stocks: averaged over the months, never summed
ind_value <- function(code, num, den_sum, den_mean, n_months) {
  idx <- match(code, IND$code)                 # outside IND[...] so `code` is the argument
  d <- IND[idx]
  den <- ifelse(d$area_only, den_mean, den_sum)
  out <- ifelse(d$unit == "count", num,
                num / den * d$factor * ifelse(d$annualized, 12 / n_months, 1))
  out[!is.finite(out)] <- NA_real_
  out
}

# Coverages above 100% (usually a denominator that is too small, e.g. an under-estimated
# projected population) are shown as 100%. The uncapped value is kept as `value_raw` and is what
# the CSV download contains.
cap_pct <- function(v, code) {
  u <- IND$unit[match(code, IND$code)]
  ifelse(!is.na(v) & !is.na(u) & u == "%" & v > 100, 100, v)
}
add_cap <- function(s) {
  if (!nrow(s)) { s[, `:=`(value_raw = numeric(), capped = logical())]; return(s) }
  s[, value_raw := value][, value := cap_pct(value_raw, code)][, capped := !is.na(value_raw) & value_raw > value]
  s
}
CAP_NOTE <- "Coverages above 100% are shown as 100% (marked *); this usually means the population denominator is under-estimated. The uncapped values are in the CSV download."

bucket_of <- function(period, by) switch(by,
  none    = rep(0L, length(period)),
  month   = period,
  quarter = (period %/% 100L) * 10L + ((period %% 100L) - 1L) %/% 3L + 1L,
  year    = period %/% 100L)
bucket_label <- function(b, by) switch(by,
  none = rep("Selected period", length(b)), month = fmt_month(b),
  quarter = sprintf("%d Q%d", b %/% 10L, b %% 10L), year = as.character(b))
bucket_date <- function(b, by) switch(by,
  none = rep(as.Date(NA), length(b)), month = ym_date(b),
  quarter = as.Date(sprintf("%d-%02d-15", b %/% 10L, (b %% 10L - 1L) * 3L + 2L)),
  year = as.Date(sprintf("%d-07-01", b)))

# Summarise indicators for units at a level over a month window.
#   codes, level: which indicators and which org unit level
#   uids: restrict to these units (NULL = all at that level)
#   by: "none", "month", "quarter", "year"
summarise_ind <- function(codes, level, from = DEFAULT_FROM, to = DEFAULT_TO, uids = NULL, by = "none") {
  keys <- CJ(code = codes, level = level)          # built outside IM[...] so `level` is the argument
  x <- IM[keys, on = .(code, level), nomatch = NULL]
  x <- x[period >= from & period <= to]
  x <- x[is.na(END_OF[code]) | period <= END_OF[code]]          # never show months after a series ended
  if (!is.null(uids)) { u <- uids; x <- x[uid %in% u] }
  if (!nrow(x)) return(data.table(code = character(), uid = character(), bucket = integer(), value = numeric()))
  x[, bucket := bucket_of(period, by)]
  months <- month_seq(from, to)
  nm <- data.table(bucket = bucket_of(months, by))[, .(n_months = .N), by = bucket]
  s <- x[, .(num = if (all(is.na(num))) NA_real_ else sum(num, na.rm = TRUE),
             den_sum = if (all(is.na(den))) NA_real_ else sum(den, na.rm = TRUE),
             den_mean = mean(den, na.rm = TRUE), n_rep = sum(!is.na(num))),
         by = .(code, uid, bucket)]
  s <- merge(s, nm, by = "bucket")
  s[, value := ind_value(code, num, den_sum, den_mean, n_months)]
  # an incomplete last quarter or year (e.g. Q3 with only July and August) would look like a collapse
  if (by %in% c("quarter", "year")) s <- s[n_months >= (if (by == "quarter") 3L else 12L) | bucket != max(bucket_of(months, by))]
  add_cap(s)[]
}

# Aggregate facilities into custom groups (ownership, level, authority) inside an area.
summarise_groups <- function(codes, set, within = NULL, from = DEFAULT_FROM, to = DEFAULT_TO, by = "none") {
  f <- OU[level_name == "facility" & !is.na(get(paste0("grp_", set)))]
  if (!is.null(within) && within != META$region_uid)
    f <- f[uid_l3 == within | uid_l4 == within | uid_l5 == within]
  x <- IM[CJ(code = codes, level = "facility"), on = .(code, level), nomatch = NULL][
    period >= from & period <= to & uid %in% f$uid]
  if (!nrow(x)) return(data.table())
  x[, grp := f[[paste0("grp_", set)]][match(uid, f$uid)]]
  x[, bucket := bucket_of(period, by)]
  nm <- data.table(bucket = bucket_of(month_seq(from, to), by))[, .(n_months = .N), by = bucket]
  s <- x[, .(num = sum(num, na.rm = TRUE), den_sum = sum(den, na.rm = TRUE),
             den_mean = mean(den, na.rm = TRUE), n_fac = uniqueN(uid)), by = .(code, grp, bucket)]
  s <- merge(s, nm, by = "bucket")
  s[, value := ind_value(code, num, den_sum, den_mean, n_months)]
  add_cap(s)[]
}

# Busoga-wide reference value for the same window
region_value <- function(codes, from, to) {
  s <- summarise_ind(codes, "region", from, to)
  setNames(s$value, s$code)
}

# first month with data for each indicator (series added by the July 2025 form revision start late)
SERIES_START <- IM[, .(start = min(period)), by = code][, setNames(start, code)]

# ---- targets ------------------------------------------------------------------------
# targets.csv (code, not data): the Uganda national target where one exists, otherwise a global
# one. ">=56" means at least 56, "<=4" at most 4, "5-15" an acceptable range.
TGT <- local({
  t <- fread("targets.csv", na.strings = "", encoding = "UTF-8")
  t <- t[code %in% IND$code]
  use_nat <- !is.na(t$national)
  t[, `:=`(spec = fifelse(use_nat, national, global), source = fifelse(use_nat, national_source, global_source),
           basis = fifelse(use_nat, "National", "Global"))]
  t[, op := fifelse(grepl("^>=", spec), ">=", fifelse(grepl("^<=", spec), "<=", "range"))]
  num <- function(x) suppressWarnings(as.numeric(x))
  t[, lo := fifelse(op == "range", num(sub("-.*", "", spec)), fifelse(op == ">=", num(sub(">=", "", spec)), NA_real_))]
  t[, hi := fifelse(op == "range", num(sub(".*-", "", spec)), fifelse(op == "<=", num(sub("<=", "", spec)), NA_real_))]
  t[]
})
has_target <- function(code) code %in% TGT$code
target_text <- function(code) {
  i <- match(code, TGT$code); t <- TGT[i]
  u <- IND$unit[match(code, IND$code)]; suf <- ifelse(u == "%", "%", "")
  n <- function(x) as.character(x)                  # no padding (format() pads to a common width)
  out <- ifelse(is.na(i), NA_character_,
    ifelse(t$op == ">=", sprintf("\u2265 %s%s", n(t$lo), suf),
    ifelse(t$op == "<=", sprintf("\u2264 %s%s", n(t$hi), suf), sprintf("%s\u2013%s%s", n(t$lo), n(t$hi), suf))))
  out
}
# Population-based rates that are not annualised in DHIS2 (e.g. malaria incidence per 1,000) add
# up over the months selected; their targets are per year, so compare the annualised value.
PER_YEAR_TARGET <- IND[area_only == TRUE & annualized == FALSE & unit != "%" & unit != "count", code]
target_basis_value <- function(v, code, n_months = 12) ifelse(code %in% PER_YEAR_TARGET, v * 12 / n_months, v)
# "met", "below" (worse than target, for either direction) or NA when there is no target or value
target_status <- function(v, code, n_months = 12) {
  v <- target_basis_value(v, code, n_months)
  i <- match(code, TGT$code); t <- TGT[i]
  out <- rep(NA_character_, length(v)); ok <- !is.na(i) & !is.na(v)
  met <- ifelse(t$op == ">=", v >= t$lo - 1e-9, ifelse(t$op == "<=", v <= t$hi + 1e-9, v >= t$lo & v <= t$hi))
  out[ok] <- ifelse(met[ok], "met", "below")
  out
}
target_value_line <- function(code, monthly = FALSE) {   # a reference value for charts (NA for ranges)
  i <- match(code, TGT$code); t <- TGT[i]
  out <- ifelse(is.na(i), NA_real_, ifelse(t$op == ">=", t$lo, ifelse(t$op == "<=", t$hi, NA_real_)))
  if (monthly) out[code %in% PER_YEAR_TARGET] <- NA_real_       # a yearly target cannot be drawn on monthly values
  out
}
target_chip <- function(v, code, n_months = 12) {
  if (!has_target(code)) return(NULL)
  st <- target_status(v, code, n_months); ti <- match(code, TGT$code); t <- TGT[ti]   # index outside [ ] so `code` is the argument
  cls <- if (is.na(st)) "tgt-chip none" else if (st == "met") "tgt-chip met" else "tgt-chip below"
  lab <- if (is.na(st)) "" else if (st == "met") "Target met" else if (t$op == "range") "Outside range" else "Below target"
  span(class = cls, title = sprintf("%s target %s. Source: %s%s", t$basis, target_text(code), t$source,
                                    if (!is.na(t$note)) paste0(". ", t$note) else ""),
       sprintf("%s target %s", t$basis, target_text(code)), if (nzchar(lab)) tags$b(paste0(" \u00b7 ", lab)))
}
TARGET_NOTE <- "Targets: the Uganda national target (MoH Strategic Plan 2020/21-2024/25 / Annual Health Sector Performance Report 2024/25) where one exists, otherwise a global target (WHO, UNAIDS, Immunization Agenda 2030, ENAP/EPMM). Hover a target for its source."

# ---- formatting --------------------------------------------------------------------
fmt_val <- function(v, code = NULL, unit = NULL) {
  if (is.null(unit)) unit <- IND$unit[match(code, IND$code)]
  unit <- rep_len(unit, length(v))                   # one indicator code may format many values
  out <- rep("–", length(v)); ok <- !is.na(v)
  whole <- ok & (unit == "count" | abs(v) >= 1000)
  out[whole] <- formatC(round(v[whole]), format = "d", big.mark = ",")
  frac <- ok & !whole
  out[frac] <- formatC(v[frac], format = "f", digits = 1, big.mark = ",")
  out[ok & unit == "%"] <- paste0(out[ok & unit == "%"], "%")
  out
}
unit_label <- function(code) {
  u <- IND$unit[match(code, IND$code)]
  ifelse(u == "count", "number", ifelse(u == "ratio", "per person per year", u))
}
ind_label <- function(code) IND$label[match(code, IND$code)]

# status of a unit relative to Busoga for a direction-aware indicator
status_vs <- function(v, ref, direction, band = 0.10) {
  s <- rep("none", length(v))
  ok <- !is.na(v) & !is.na(ref) & ref != 0
  rel <- (v - ref) / abs(ref)
  better <- ifelse(direction == "low", -rel, rel)
  s[ok & direction == "neutral"] <- "none"
  k <- ok & direction != "neutral"
  s[k] <- ifelse(better[k] > band, "good", ifelse(better[k] < -band, "critical", "warning"))
  s
}

# ---- chart helpers -----------------------------------------------------------------
plotly_base <- function(p, ytitle = NULL, xtitle = NULL, legend = TRUE) {
  p |> layout(
    font = list(family = "Arial, Helvetica, sans-serif", size = 12, color = BRAND$ink2),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    xaxis = list(title = if (is.null(xtitle)) "" else xtitle, gridcolor = "rgba(0,0,0,0)", linecolor = BRAND$base, zeroline = FALSE,
                 tickfont = list(color = BRAND$muted)),
    yaxis = list(title = if (is.null(ytitle)) "" else ytitle, gridcolor = BRAND$grid, zeroline = FALSE, tickfont = list(color = BRAND$muted),
                 rangemode = "tozero"),
    hoverlabel = list(bgcolor = "white", bordercolor = BRAND$grid, font = list(color = BRAND$ink)),
    legend = list(orientation = "h", y = -0.18, font = list(size = 11)),
    showlegend = legend, margin = list(l = 10, r = 10, t = 10, b = 10)
  ) |> config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toImage"),
              modeBarButtonsToAdd = list(DL_BUTTON))
}
# modebar button: save the whole card (title, chart, area, period and source) as a PNG
DL_BUTTON <- list(name = "Download as image (with title and source)", icon = htmlwidgets::JS("Plotly.Icons.camera"),
                  click = htmlwidgets::JS("function(gd){ var c = gd.closest('.card') || gd.parentElement; window.bhfCapture && window.bhfCapture(c); }"))

empty_plot <- function(msg = "No data for this selection") {
  plot_ly() |> layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                      annotations = list(text = msg, showarrow = FALSE, font = list(color = BRAND$muted, size = 14)),
                      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
    config(displayModeBar = FALSE)
}

# categorical palette for up to N compared units (fixed order, never cycled)
SERIES <- c("#3949AB", "#A3214A", "#00897B", "#C17D11", "#8E44AD", "#2E7D32", "#5C6BC0", "#C0582B")

# ---- shared UI bits ----------------------------------------------------------------
indicator_choices <- function(filter_area_only = FALSE, facility = FALSE) {
  x <- IND
  if (facility) x <- x[area_only == FALSE]
  split(setNames(x$code, x$label), as.character(x$theme))[intersect(THEMES$theme, unique(as.character(x$theme)))]
}

theme_chip <- function(th) span(class = "theme-chip", style = sprintf("--chip:%s", theme_col(th)),
                                fontawesome::fa(theme_icon(th), fill = theme_col(th), height = "0.9em"), th)

period_caption <- function(from, to) sprintf("%s to %s", fmt_month(from), fmt_month(to))

info_note <- function(...) div(class = "info-note", fontawesome::fa("circle-info", fill = BRAND$muted), span(...))
