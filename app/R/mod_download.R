# Download: district-level (and Busoga) aggregates only. No facility, sub-county or DLG rows
# are ever written, so no facility can be identified from the export.

download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Download", "Download district-level data",
              "Aggregated to district / city and Busoga totals only. The file contains no facility, sub-county or DLG rows, so no individual facility is linked to any figure."),
    layout_columns(col_widths = c(4, 8),
      card(card_header("Choose what to export"),
           selectInput(ns("themes"), "Programme themes", THEMES$theme, selected = THEMES$theme, multiple = TRUE),
           uiOutput(ns("ind_pick")),
           radioButtons(ns("by"), "Time step", c(Month = "month", Quarter = "quarter", Year = "year"), selected = "year", inline = TRUE),
           period_ui(ns("period"), selected = "all"),
           checkboxInput(ns("nd"), "Include numerators and denominators", TRUE),
           checkboxInput(ns("region"), "Include Busoga totals", TRUE),
           downloadButton(ns("dl"), "Download CSV", class = "btn-primary w-100 mt-2"),
           downloadButton(ns("dict"), "Download indicator dictionary", class = "btn-outline-secondary w-100 mt-2"),
           info_note("Values follow DHIS2 rules: rates are sum(numerator) / sum(denominator); population-based indicators are annualised. ",
                     "Numerators below 5 are shown as they are in DHIS2; take care when publishing very small counts. ",
                     "Coverages above 100% are exported uncapped (the dashboard shows them as 100%); the column above_100_shown_capped marks them.")),
      card(full_screen = TRUE, card_header("Preview", span(class = "sub", textOutput(ns("n"), inline = TRUE))),
           DTOutput(ns("preview"))))
  )
}

download_server <- function(id) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  per <- period_server("period")
  output$ind_pick <- renderUI({
    x <- IND[theme %in% input$themes]
    selectizeInput(ns("codes"), "Indicators", split(setNames(x$code, x$label), as.character(x$theme))[unique(as.character(x$theme))],
                   selected = x$code, multiple = TRUE, options = list(plugins = list("remove_button")))
  })
  export <- reactive({
    req(input$codes); p <- per()
    lv <- c("district", if (isTRUE(input$region)) "region")
    s <- rbindlist(lapply(lv, function(l) summarise_ind(input$codes, l, p$from, p$to, by = input$by)[, level := l]))
    stopifnot(all(s$level %in% c("district", "region")))          # guarantee: nothing below district
    s[, `:=`(area = fifelse(level == "region", "Busoga", ou_name[uid]),
             period = bucket_label(bucket, input$by))]
    d <- IND[match(s$code, IND$code)]
    s[, `:=`(indicator = d$label, theme = as.character(d$theme), unit = unit_label(code),
             value = round(value_raw, 2), numerator = round(num, 2),
             denominator = fifelse(d$unit == "count", NA_real_, round(fifelse(d$annualized, den_mean, den_sum), 2)),
             months_in_period = n_months, facility_months_reporting = n_rep)]
    out <- s[, .(area_level = fifelse(level == "region", "Region", "District / City"), area, period, theme,
                 indicator_code = code, indicator, unit, value, numerator, denominator, months_in_period,
                 facility_months_reporting, above_100_shown_capped = capped)]
    if (!isTRUE(input$nd)) out[, c("numerator", "denominator") := NULL]
    setorder(out, area_level, area, indicator_code, period)
    out
  })
  output$n <- renderText(sprintf("%s rows · %d areas · %s", format(nrow(export()), big.mark = ","),
                                 uniqueN(export()$area), per()$label))
  output$preview <- renderDT(datatable(head(export(), 500), rownames = FALSE,
                                       options = list(pageLength = 15, scrollX = TRUE, dom = "tip")))
  output$dl <- downloadHandler(
    filename = function() sprintf("busoga_district_hmis_%s_%s.csv", input$by, format(Sys.Date(), "%Y%m%d")),
    content = function(f) {
      x <- export()
      writeLines(c(sprintf("# Busoga Health Forum - district-level HMIS aggregates (no facility-level data). Source: DHIS2 hmis.health.go.ug, extracted %s.", META$extracted),
                   sprintf("# Period: %s; time step: %s.", per()$label, input$by)), f)
      fwrite(x, f, append = TRUE, col.names = TRUE)
    })
  output$dict <- downloadHandler(
    filename = function() "busoga_indicator_dictionary.csv",
    content = function(f) fwrite(IND[, .(indicator_code = code, theme, indicator = label, unit = unit_label(code), direction,
                                         source = fifelse(is.na(dhis2_uid) | dhis2_uid == "", "BHF-defined from DHIS2 data elements", "MoH DHIS2 indicator"),
                                         dhis2_uid, dhis2_name, numerator_description = num_desc,
                                         denominator_description = den_desc, factor, annualised = annualized,
                                         population_based = area_only, note)], f))
})
