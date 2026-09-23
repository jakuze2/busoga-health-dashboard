# Indicators & methods: the dictionary and how every number is made.

about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Indicators & methods", "What every number means",
              "Definitions come straight from the Ministry of Health indicator set in DHIS2. BHF-defined indicators are built only from named DHIS2 data elements and are marked as such.", key = "about"),
    div(class = "st-sec", div(class = "st-kicker", "From register to dashboard"), h3(class = "st-h", "How every number is made")),
    div(class = "mt-flow", lapply(seq_along(METHOD_STEPS), function(i) { m <- METHOD_STEPS[[i]]
      div(class = "mt-step", style = sprintf("--mc:%s", m$col), span(class = "mt-n", i), span(class = "mt-icon", fontawesome::fa(m$icon, fill = "#fff", height = "1.2em")),
          div(class = "mt-t", m$t), div(class = "mt-d", m$d)) })),
    div(class = "st-sec", div(class = "st-kicker", "Open and official data"), h3(class = "st-h", "Where the data come from"),
        p(class = "st-lead", "Every source, what it adds, how often it is refreshed and its licence.")),
    div(class = "mt-sources", lapply(DATA_SOURCES, function(d)
      div(class = "mt-src", style = sprintf("--mc:%s", d$col),
          div(class = "mt-src-h", span(class = "mt-src-icon", fontawesome::fa(d$icon, fill = d$col, height = "1.1em")), div(div(class = "mt-src-n", d$n), div(class = "mt-src-o", d$o))),
          div(class = "mt-src-w", d$w), div(class = "mt-src-f", span(fontawesome::fa("rotate", fill = "#8a8799", height = ".8em"), " ", d$f), span(d$l))))),
    div(class = "st-sec", div(class = "st-kicker", "The details"), h3(class = "st-h", "Indicator dictionary and methods")),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE, card_header("Indicator dictionary", span(class = "sub", sprintf("%d indicators in %d themes", nrow(IND), uniqueN(IND$theme)))),
           reactableOutput(ns("dict"))),
      accordion(open = "How values are computed", class = "mt-acc",
        accordion_panel("Source", icon = fontawesome::fa("database"),
          p(sprintf("Uganda national DHIS2 (hmis.health.go.ug), analytics API, extracted %s. Every Busoga facility (level 6 under region Busoga), %s to %s.",
                    META$extracted, fmt_month(MONTH_MIN), fmt_month(MONTH_MAX)))),
        accordion_panel("How values are computed", icon = fontawesome::fa("calculator"),
          tags$ul(
            tags$li("Data elements are pulled per facility and month with their full category breakdown."),
            tags$li("Each numerator and denominator is evaluated exactly as its DHIS2 expression; missing parts count as zero unless all parts are missing."),
            tags$li("Numerators and denominators are summed from facility to sub-county, DLG, district and Busoga, then divided: rates are pooled, never averaged."),
            tags$li("Population-based indicators use the 107a projected population (sub-county, yearly) and are annualised as in DHIS2, so they exist for areas only. When the current year's projection is not yet entered, the previous year is used."),
            tags$li("Values were checked against DHIS2's own indicator results (see Validation)."))),
        accordion_panel("Scorecard colours", icon = fontawesome::fa("palette"),
          p("A unit is rated against the Busoga value for the same indicator and period, taking the indicator's direction into account. National targets are shown where the Ministry of Health has set one.")),
        accordion_panel("Data quality", icon = fontawesome::fa("clipboard-check"),
          p("Completeness and timeliness are DHIS2 reporting rates for the monthly datasets. A facility is a regular reporter when it sent at least 75% of expected 105:01 reports in the last 12 months; 'stopped' when it sent none in the last 6 months after reporting before; 'never reported' when it was expected but never sent one.")),
        accordion_panel("Validation", icon = fontawesome::fa("circle-check"), uiOutput(ns("valid"))),
        accordion_panel("Population projections", icon = fontawesome::fa("chart-line"),
          p("Busoga's population is projected from the 1980-2024 censuses with three growth scenarios; every assumption is listed on the Place page (Busoga profile).")),
        accordion_panel("Caveats", icon = fontawesome::fa("triangle-exclamation"),
          tags$ul(tags$li("The latest month may still be receiving late reports."),
                  tags$li("Facility-reported data are only as good as the registers behind them; see the Data quality page."),
                  tags$li("Coverage indicators depend on the population projection; they can exceed 100% where people cross district borders for care."))))))
}

METHOD_STEPS <- list(
  list(icon = "download", col = "#201B6D", t = "Extract", d = "Every Busoga facility's monthly and weekly reports are pulled from the national DHIS2 through its analytics API."),
  list(icon = "calculator", col = "#3949AB", t = "Compute", d = "Numerators and denominators are rebuilt from the DHIS2 data elements, exactly as the Ministry of Health defines them."),
  list(icon = "sitemap", col = "#5C6BC0", t = "Aggregate", d = "Facility figures are summed to sub-county, DLG, district and Busoga before dividing, so rates are pooled, never averaged."),
  list(icon = "clipboard-check", col = "#00897B", t = "Check", d = "Completeness, timeliness, consistency and outliers are scored for every facility; extreme values are flagged."),
  list(icon = "circle-check", col = "#2E7D32", t = "Validate", d = "Every official indicator is recomputed for each district and compared with DHIS2's own result."),
  list(icon = "earth-africa", col = "#C17D11", t = "Add context", d = "Census, population, climate, access and market data are joined by place and month."),
  list(icon = "rotate", col = "#B0306A", t = "Refresh", d = "An automatic job refreshes weekly surveillance every Monday and the monthly data on the 16th."))
DATA_SOURCES <- list(
  list(n = "HMIS / DHIS2", o = "Ministry of Health", icon = "hospital", col = "#201B6D", w = "Monthly and weekly reports from every health facility: services, medicines, management, surveillance.", f = "weekly and monthly", l = "MoH data"),
  list(n = "Census 2024 and earlier", o = "Uganda Bureau of Statistics", icon = "people-group", col = "#B0306A", w = "Population counts 1980-2024, households, living conditions, birth registration, mortality.", f = "each census", l = "UBOS"),
  list(n = "WorldPop", o = "University of Southampton", icon = "chart-simple", col = "#8E44AD", w = "Age and sex structure on a 1 km grid, summed within every boundary.", f = "yearly", l = "CC BY 4.0"),
  list(n = "CHIRPS", o = "Climate Hazards Center", icon = "cloud-rain", col = "#0277BD", w = "Rainfall and drought (SPI) for every sub-county.", f = "monthly", l = "CC BY 4.0"),
  list(n = "ERA5-Land and CAMS", o = "Copernicus / ECMWF", icon = "temperature-high", col = "#EF6C00", w = "Temperature, hot days and air quality (PM2.5).", f = "monthly", l = "Copernicus licence"),
  list(n = "OpenStreetMap", o = "OSM contributors", icon = "map", col = "#2E7D32", w = "Roads, rivers, towns, schools, markets and commerce.", f = "on refresh", l = "ODbL"),
  list(n = "HeiGIT", o = "Heidelberg Institute for Geoinformation Technology", icon = "route", col = "#00897B", w = "Access to services, rural road access, flood exposure and vulnerability.", f = "yearly", l = "CC BY-SA"),
  list(n = "WFP market prices", o = "World Food Programme", icon = "wheat-awn", col = "#C17D11", w = "Food prices in Iganga and Jinja markets.", f = "monthly", l = "CC BY-IGO"),
  list(n = "World Bank and WHO GHO", o = "World Bank, World Health Organization", icon = "globe", col = "#37474F", w = "National health spending, workforce, UHC index and governance.", f = "yearly", l = "CC BY 4.0"))

about_server <- function(id) moduleServer(id, function(input, output, session) {
  output$dict <- renderReactable({
    x <- IND[, .(theme = as.character(theme), label, unit = unit_label(code), direction,
                 source = fifelse(is.na(dhis2_uid) | dhis2_uid == "", "BHF-defined", "MoH DHIS2"),
                 definition = fifelse(nzchar(num_desc %||% ""), sprintf("Numerator: %s. Denominator: %s.", num_desc, den_desc), note),
                 dhis2_uid, area_only)]
    reactable(x, groupBy = "theme", defaultExpanded = TRUE, searchable = TRUE, pagination = FALSE, height = 760, compact = TRUE,
      columns = list(theme = colDef(name = "Theme", minWidth = 150, style = function(v) list(borderLeft = sprintf("4px solid %s", theme_col(v)))),
                     label = colDef(name = "Indicator", minWidth = 220, style = list(fontWeight = 600)),
                     unit = colDef(name = "Unit", width = 90),
                     direction = colDef(name = "Better when", width = 95, cell = function(v) c(high = "higher", low = "lower", neutral = "–")[[v]]),
                     source = colDef(name = "Source", width = 100),
                     definition = colDef(name = "Definition", minWidth = 320, style = list(fontSize = ".8rem", color = BRAND$ink2)),
                     dhis2_uid = colDef(name = "DHIS2 UID", width = 110, style = list(fontFamily = "monospace", fontSize = ".78rem")),
                     area_only = colDef(name = "Areas only", width = 80, cell = function(v) if (isTRUE(v)) "yes" else "")))
  })
  output$valid <- renderUI({
    v <- META$validation
    if (is.null(v)) return(p(class = "muted", "Validation not run."))
    p(sprintf("Every official indicator (%s) was recomputed for each district and for Busoga for %s and compared with DHIS2's own result: %s of %s values match (within 0.5%%, allowing for DHIS2's rounding).",
              v$n, v$year, format(v$n_match, big.mark = ","), format(v$n_values, big.mark = ",")))
  })
})
