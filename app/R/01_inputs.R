# Shared inputs: period picker and cascading area picker (district > DLG > sub-county).

period_presets <- function() {
  yrs <- rev(unique(MONTHS %/% 100L))
  last <- MONTH_MAX
  ytd_lab <- sprintf("%d year to date (Jan to %s)", last %/% 100L, format(ym_date(last), "%b"))
  c(setNames("last12", sprintf("Last 12 months (%s to %s)", fmt_month(DEFAULT_FROM), fmt_month(last))),
    setNames("last3", "Last 3 months"),
    setNames(paste0("y", yrs[1]), ytd_lab),
    setNames(paste0("y", yrs[-1]), paste("Calendar year", yrs[-1])),
    setNames("all", sprintf("All data (%s to %s)", fmt_month(MONTH_MIN), fmt_month(last))),
    setNames("custom", "Custom range..."))
}
resolve_period <- function(preset, from = NULL, to = NULL) {
  last <- MONTH_MAX
  switch(substr(preset, 1, 1),
    l = if (preset == "last12") c(DEFAULT_FROM, last) else
          c(date_ym(seq(ym_date(last), by = "-2 months", length.out = 2)[2]), last),
    y = { y <- as.integer(substring(preset, 2)); c(max(y * 100L + 1L, MONTH_MIN), min(y * 100L + 12L, last)) },
    a = c(MONTH_MIN, last),
    c = c(as.integer(from %||% DEFAULT_FROM), as.integer(to %||% last)))
}
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

month_choices <- setNames(rev(MONTHS), fmt_month(rev(MONTHS)))

# Period slicer: one row of buttons (the latest 12 months by default); earlier years and any
# other range through "Custom".
period_slicer_choices <- function() {
  yrs <- rev(unique(MONTHS %/% 100L)); last <- MONTH_MAX
  c(setNames("last3", "Last 3 months"), setNames("last12", "Last 12 months"),
    setNames(paste0("y", yrs[1]), sprintf("%d so far", yrs[1])),
    setNames(paste0("y", yrs[2:min(4, length(yrs))]), yrs[2:min(4, length(yrs))]),
    setNames("all", "All years"), setNames("custom", "Custom"))
}
period_ui <- function(id, label = "Period", selected = "last12") {
  ns <- NS(id)
  div(class = "period-slicer",
    radioButtons(ns("preset"), label, period_slicer_choices(), selected = selected, inline = TRUE),
    conditionalPanel(sprintf("input['%s'] == 'custom'", ns("preset")), class = "period-custom",
      selectInput(ns("from"), "From", month_choices, selected = DEFAULT_FROM, width = "130px"),
      selectInput(ns("to"), "To", month_choices, selected = MONTH_MAX, width = "130px")))
}
period_server <- function(id) moduleServer(id, function(input, output, session) {
  reactive({
    p <- resolve_period(input$preset, input$from, input$to)
    if (p[1] > p[2]) p <- rev(p)
    list(from = p[1], to = p[2], label = period_caption(p[1], p[2]),
         n = length(month_seq(p[1], p[2])))
  })
})

# previous window of the same length (for change indicators)
previous_window <- function(from, to) {
  n <- length(month_seq(from, to))
  pto <- date_ym(seq(ym_date(from), by = "-1 month", length.out = 2)[2])
  pfrom <- date_ym(seq(ym_date(pto), by = sprintf("-%d months", n - 1), length.out = 2)[2])
  if (n == 1) pfrom <- pto
  if (pfrom < MONTH_MIN) return(NULL)
  c(pfrom, pto)
}

area_ui <- function(id, depth = 3, width = "200px") {
  ns <- NS(id)
  d <- units_at("district")
  tagList(
    selectInput(ns("district"), "District / City", c("All Busoga" = META$region_uid, d), width = width),
    if (depth >= 2) selectInput(ns("dlg"), "DLG / Municipality", c("All" = ""), width = width),
    if (depth >= 3) selectInput(ns("subcounty"), "Sub-county / Division", c("All" = ""), width = width),
    if (depth >= 4) selectizeInput(ns("facility"), "Health facility", c("All" = ""), width = "260px")
  )
}
area_server <- function(id) moduleServer(id, function(input, output, session) {
  observeEvent(input$district, {
    ch <- if (input$district == META$region_uid) character() else units_at("dlg", input$district)
    updateSelectInput(session, "dlg", choices = c("All" = "", ch))
    updateSelectInput(session, "subcounty", choices = c("All" = ""))
    updateSelectizeInput(session, "facility", choices = c("All" = ""))
  })
  observeEvent(input$dlg, {
    ch <- if (!nzchar(input$dlg %||% "")) character() else units_at("subcounty", input$dlg)
    updateSelectInput(session, "subcounty", choices = c("All" = "", ch))
  }, ignoreInit = TRUE)
  observeEvent(input$subcounty, {
    ch <- if (!nzchar(input$subcounty %||% "")) character() else units_at("facility", input$subcounty)
    updateSelectizeInput(session, "facility", choices = c("All" = "", ch))
  }, ignoreInit = TRUE)
  reactive({
    u <- if (nzchar(input$facility %||% "")) input$facility else
         if (nzchar(input$subcounty %||% "")) input$subcounty else
         if (nzchar(input$dlg %||% "")) input$dlg else input$district %||% META$region_uid
    list(uid = u, level = if (u == META$region_uid) "region" else ou_level[[u]], name = ou_name[[u]])
  })
})

# the child level below a given level
child_level <- function(lvl) switch(lvl, region = "district", district = "dlg", dlg = "subcounty",
                                    subcounty = "facility", facility = NA_character_)

# children of a unit (uids)
children_of <- function(uid) {
  lvl <- if (uid == META$region_uid) "region" else ou_level[[uid]]
  cl <- child_level(lvl); if (is.na(cl)) return(character())
  x <- OU[level_name == cl]
  if (lvl != "region") x <- x[uid_l3 == uid | uid_l4 == uid | uid_l5 == uid]
  setNames(x$uid, x$name)
}

filter_bar <- function(...) div(class = "filter-bar", ...)
page_head <- function(eyebrow, title, text = NULL, right = NULL)
  div(class = "page-head", div(class = "page-head-text", div(class = "eyebrow", eyebrow), h2(title), if (!is.null(text)) p(text)),
      if (is.null(right)) data_status() else right)

# "what data am I looking at" panel shown at the right of every page header
data_status <- function() {
  nfac <- nrow(OU[level_name == "facility"]); nd <- nrow(OU[level_name == "district"])
  item <- function(icon, lab, val) div(class = "ds-item", span(class = "ds-icon", fontawesome::fa(icon, fill = "#201B6D", height = "1em")),
                                       div(div(class = "ds-lab", lab), div(class = "ds-val", val)))
  div(class = "data-status",
      item("calendar-check", "Latest month of data", format(ym_date(MONTH_MAX), "%B %Y")),
      item("rotate", "Last updated", META$extracted),
      item("hospital", "Coverage", sprintf("%s facilities, %d districts and cities", format(nfac, big.mark = ","), nd)),
      item("database", "Source", "Ministry of Health DHIS2 (HMIS)"))
}

# Mini area chart for KPI tiles: gradient fill, y-axis min/max labels, first/last month on the
# x-axis, the latest point marked, optional dotted reference (e.g. Busoga). Inline SVG, so a page
# with dozens of tiles stays fast.
mini_chart <- function(v, periods, color, code, ref = NA, w = 260, h = 92, target = NA) {
  ok <- is.finite(v); v <- v[ok]; periods <- periods[ok]
  if (length(v) < 2) return(div(class = "mini-empty", "not enough data for a trend"))
  gid <- paste0("g", substr(gsub("[^0-9a-z]", "", tolower(paste0(code, color, length(v), sum(v)))), 1, 14))
  pl <- 34; pr <- 8; pt <- 8; pb <- 18                  # padding: left axis labels, bottom month labels
  lo <- min(c(v, if (is.finite(ref)) ref, if (is.finite(target)) target))
  hi <- max(c(v, if (is.finite(ref)) ref, if (is.finite(target)) target))
  if (hi == lo) { lo <- lo - 1; hi <- hi + 1 }
  lo <- max(0, lo - 0.08 * (hi - lo)); hi <- hi + 0.08 * (hi - lo)
  x <- pl + (seq_along(v) - 1) / (length(v) - 1) * (w - pl - pr)
  y <- pt + (hi - v) / (hi - lo) * (h - pt - pb)
  line <- paste(sprintf("%.1f,%.1f", x, y), collapse = " ")
  area <- sprintf("%.1f,%.1f %s %.1f,%.1f", x[1], h - pb, line, tail(x, 1), h - pb)
  lab <- function(z) fmt_val(z, code)
  refl <- if (is.finite(ref)) { yr <- pt + (hi - ref) / (hi - lo) * (h - pt - pb)
    sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="#898781" stroke-width="1" stroke-dasharray="3 3"/>', pl, w - pr, yr, yr) } else ""
  if (is.finite(target)) { yt <- pt + (hi - target) / (hi - lo) * (h - pt - pb)
    refl <- paste0(refl, sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="#b3261e" stroke-width="1.2" stroke-dasharray="6 3"><title>Target %s</title></line>',
                                 pl, w - pr, yt, yt, htmlEscape(lab(target)))) }
  HTML(sprintf(
    '<svg viewBox="0 0 %d %d" width="100%%" height="%d" role="img" aria-label="trend">
      <defs><linearGradient id="%s" x1="0" x2="0" y1="0" y2="1">
        <stop offset="0%%" stop-color="%s" stop-opacity=".32"/><stop offset="100%%" stop-color="%s" stop-opacity="0"/></linearGradient></defs>
      <line x1="%d" x2="%d" y1="%d" y2="%d" stroke="#e1e0d9"/><line x1="%d" x2="%d" y1="%d" y2="%d" stroke="#e1e0d9"/>
      <text x="%d" y="%d" class="ax" text-anchor="end">%s</text><text x="%d" y="%d" class="ax" text-anchor="end">%s</text>
      <text x="%d" y="%d" class="ax">%s</text><text x="%d" y="%d" class="ax" text-anchor="end">%s</text>
      %s<polygon points="%s" fill="url(#%s)"/>
      <polyline points="%s" fill="none" stroke="%s" stroke-width="2.2" stroke-linejoin="round" stroke-linecap="round"/>
      <circle cx="%.1f" cy="%.1f" r="3.6" fill="#fff" stroke="%s" stroke-width="2"/></svg>',
    w, h, h, gid, color, color,
    pl, w - pr, pt, pt, pl, w - pr, h - pb, h - pb,
    pl - 4, pt + 8, htmlEscape(lab(max(c(v, ref, target), na.rm = TRUE))), pl - 4, h - pb, htmlEscape(lab(min(c(v, ref, target), na.rm = TRUE))),
    pl, h - 4, format(ym_date(periods[1]), "%b %y"), w - pr, h - 4, format(ym_date(tail(periods, 1)), "%b %y"),
    refl, area, gid, line, color, tail(x, 1), tail(y, 1), color))
}

# inline SVG sparkline (fast; no widget per tile)
spark_svg <- function(v, color, w = 200, h = 36) {
  v <- v[is.finite(v)]
  if (length(v) < 2) return(HTML(""))
  r <- range(v); if (diff(r) == 0) r <- r + c(-1, 1)
  x <- seq(2, w - 2, length.out = length(v)); y <- h - 3 - (v - r[1]) / diff(r) * (h - 6)
  pts <- paste(sprintf("%.1f,%.1f", x, y), collapse = " ")
  HTML(sprintf('<svg viewBox="0 0 %d %d" width="100%%" height="%d" preserveAspectRatio="none" aria-hidden="true">
    <polyline points="%s" fill="none" stroke="%s" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>
    <circle cx="%.1f" cy="%.1f" r="2.8" fill="%s"/></svg>', w, h, h, pts, color, tail(x, 1), tail(y, 1), color))
}
