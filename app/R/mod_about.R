# Indicators & methods: the dictionary and how every number is made.

about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Indicators & methods", "What every number means",
              "Definitions come straight from the Ministry of Health indicator set in DHIS2. BHF-defined indicators are built only from named DHIS2 data elements and are marked as such."),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE, card_header("Indicator dictionary", span(class = "sub", sprintf("%d indicators in %d themes", nrow(IND), uniqueN(IND$theme)))),
           reactableOutput(ns("dict"))),
      card(card_header("Methods"), card_body(
        tags$h6("Source"),
        p(sprintf("Uganda national DHIS2 (hmis.health.go.ug), analytics API, extracted %s. Every Busoga facility (level 6 under region Busoga), %s to %s.",
                  META$extracted, fmt_month(MONTH_MIN), fmt_month(MONTH_MAX))),
        tags$h6("How values are computed"),
        tags$ul(
          tags$li("Data elements are pulled per facility and month with their full category breakdown."),
          tags$li("Each numerator and denominator is evaluated exactly as its DHIS2 expression; missing parts count as zero unless all parts are missing."),
          tags$li("Numerators and denominators are summed from facility to sub-county, DLG, district and Busoga, then divided: rates are pooled, never averaged."),
          tags$li("Population-based indicators use the 107a projected population (sub-county, yearly) and are annualised as in DHIS2, so they exist for areas only. When the current year's projection is not yet entered, the previous year is used."),
          tags$li("Values were checked against DHIS2's own indicator results (see the validation note below).")),
        tags$h6("Scorecard colours"),
        p("A unit is rated against the Busoga value for the same indicator and period, taking the indicator's direction into account. No national targets are assumed."),
        tags$h6("Data quality"),
        p("Completeness and timeliness are DHIS2 reporting rates for the monthly datasets. A facility is a regular reporter when it sent at least 75% of expected 105:01 reports in the last 12 months; 'stopped' when it sent none in the last 6 months after reporting before; 'never reported' when it was expected but never sent one."),
        tags$h6("Validation"),
        uiOutput(ns("valid")),
        tags$h6("Map data"),
        p("Boundaries and facility coordinates: DHIS2 (Ministry of Health). Roads, water, schools, markets, towns and OSM health sites: © OpenStreetMap contributors, ODbL. Rainfall: CHIRPS v2.0. Temperature: ERA5-Land. Air quality: CAMS. Seasonal outlook: ECMWF SEAS5."),
        tags$h6("Caveats"),
        tags$ul(tags$li("The latest month may still be receiving late reports."),
                tags$li("Facility-reported data are only as good as the registers behind them; see the Data quality page."),
                tags$li("Coverage indicators depend on the population projection; they can exceed 100% where people cross district borders for care.")))))
  )
}

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
