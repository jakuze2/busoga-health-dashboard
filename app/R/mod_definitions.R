# Indicator definitions: an interactive, searchable reference for every indicator.

FORM <- fread(file.path(DATA, "indicator_formula.csv"))
ELEM <- fread(file.path(DATA, "indicator_elements.csv"))
OTHER <- fread(file.path(DATA, "measures_other.csv"))

definitions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Reference", "Indicator definitions",
              sprintf("Every measure in the dashboard. The %d health service indicators come first: what each measures, the exact formula with the DHIS2 data elements it uses, and how it looks in Busoga today. Pick a theme or search, then click an indicator. The %d other measures (data quality, epidemic alerts, climate, forecasts, population, education and Busoga in numbers) follow below, grouped by the page they appear on.",
                      nrow(IND), nrow(OTHER))),
    layout_sidebar(
      sidebar = sidebar(width = 340, open = "always",
        textInput(ns("q"), NULL, placeholder = "Search indicators, e.g. malaria, ANC, HIV"),
        div(class = "theme-filter", radioButtons(ns("theme"), NULL, c("All themes", THEMES$theme), selected = "All themes")),
        uiOutput(ns("list"))),
      uiOutput(ns("detail")),
      layout_columns(col_widths = c(7, 5),
        card(full_screen = TRUE, card_header("Busoga, by quarter"), plotlyOutput(ns("trend"), height = 300)),
        card(full_screen = TRUE, card_header("Districts, last 12 months"), plotlyOutput(ns("dist"), height = 300))),
      card(card_header("Data elements used", span(class = "sub", "from the DHIS2 HMIS reporting forms")), DTOutput(ns("elems")))
    ),
    card(full_screen = TRUE,
         card_header("Other measures in the dashboard", span(class = "sub", sprintf("%d measures, by page", nrow(OTHER)))),
         reactableOutput(ns("other")))
  )
}

definitions_server <- function(id) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  sel <- reactiveVal("ANC03")
  observeEvent(input$pick, sel(input$pick))

  output$list <- renderUI({
    x <- IND
    if (input$theme != "All themes") x <- x[theme == input$theme]
    q <- trimws(input$q %||% "")
    if (nzchar(q)) x <- x[grepl(q, label, ignore.case = TRUE) | grepl(q, num_desc, ignore.case = TRUE) |
                            grepl(q, den_desc, ignore.case = TRUE) | grepl(q, dhis2_name, ignore.case = TRUE)]
    if (!nrow(x)) return(p(class = "muted", "No indicator matches."))
    div(class = "def-list",
        lapply(seq_len(nrow(x)), function(i) {
          r <- x[i]; col <- theme_col(as.character(r$theme)); on <- identical(r$code, sel())
          tags$a(href = "#", class = paste("def-item", if (on) "active"), style = sprintf("--th:%s", col),
                 onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'}); return false;", ns("pick"), r$code),
                 span(class = "def-dot"), span(r$label), span(class = "def-unit", unit_label(r$code)))
        }))
  })

  output$detail <- renderUI({
    cd <- sel(); r <- IND[code == cd]; f <- FORM[code == cd]; col <- theme_col(as.character(r$theme))
    official <- !is.na(r$dhis2_uid) && nzchar(r$dhis2_uid)
    ref <- region_value(cd, DEFAULT_FROM, DEFAULT_TO)[[cd]] %||% NA
    fac_line <- if (r$annualized) sprintf(" × %s, annualised", format(r$factor, big.mark = ",")) else
      if (r$unit == "count") "" else sprintf(" × %s", format(r$factor, big.mark = ","))
    card(class = "themed", style = sprintf("--th:%s", col), card_body(
      div(style = "display:flex;justify-content:space-between;gap:1rem;flex-wrap:wrap;align-items:flex-start",
          div(theme_chip(as.character(r$theme)), h3(style = sprintf("margin:.5rem 0 .2rem;color:%s;font-weight:700", BRAND$navy), r$label),
              div(class = "muted", if (official) sprintf("Ministry of Health DHIS2 indicator: %s (UID %s)", r$dhis2_name, r$dhis2_uid)
                                   else "BHF-defined from named DHIS2 data elements")),
          div(class = "def-value", div(class = "muted", sprintf("Busoga, %s", period_caption(DEFAULT_FROM, DEFAULT_TO))),
              div(style = sprintf("font-size:2rem;font-weight:700;color:%s", col), fmt_val(ref, cd)), div(class = "muted", unit_label(cd)))),
      hr(),
      layout_columns(col_widths = c(6, 6),
        div(h6("What it measures"),
            p(if (official && nzchar(r$num_desc %||% "")) sprintf("Numerator: %s.", r$num_desc) else ""),
            p(if (official && nzchar(r$den_desc %||% "") && r$unit != "count") sprintf("Denominator: %s.", r$den_desc) else ""),
            if (nzchar(r$note %||% "")) p(class = "muted", r$note),
            if (!is.na(r$ended)) div(class = "ended-note", fontawesome::fa("triangle-exclamation", fill = "#8a5a00"), ended_note(cd))),
        div(h6("Reading it"),
            tags$ul(tags$li(tags$b("Better when: "), c(high = "higher", low = "lower", neutral = "no fixed direction (context)")[[r$direction]]),
                    tags$li(tags$b("Unit: "), unit_label(cd)),
                    tags$li(tags$b("Levels: "), if (r$area_only) "sub-county, DLG, district and Busoga (population-based, so not for single facilities)"
                                                 else "health facility, sub-county, DLG, district and Busoga"),
                    tags$li(tags$b("Aggregation: "), if (r$unit == "count") "summed across facilities and months"
                                                      else "numerators and denominators summed, then divided (pooled)")))),
      h6("Formula"),
      div(class = "formula",
          div(class = "f-num", f$num_readable),
          if (r$unit != "count") tagList(div(class = "f-bar"), div(class = "f-den", f$den_readable)),
          if (nzchar(fac_line)) div(class = "f-fac", fac_line))))
  })

  output$trend <- renderPlotly({
    cd <- sel(); s <- summarise_ind(cd, "region", MONTH_MIN, MONTH_MAX, by = "quarter")[order(bucket)]
    if (!nrow(s)) return(empty_plot())
    plot_ly(s, x = ~bucket_date(bucket, "quarter"), y = ~value, type = "scatter", mode = "lines+markers",
            line = list(color = theme_col(as.character(IND[code == cd, theme])), width = 2.5), marker = list(size = 6),
            hovertemplate = "%{x|%b %Y}: %{y:,.1f}<extra></extra>") |> plotly_base(ytitle = unit_label(cd), legend = FALSE)
  })
  output$dist <- renderPlotly({
    cd <- sel(); s <- summarise_ind(cd, "district", DEFAULT_FROM, DEFAULT_TO)[!is.na(value)]
    if (!nrow(s)) return(empty_plot())
    s[, name := ou_name[uid]]; s <- s[order(value)]
    plot_ly(s, x = ~value, y = ~factor(name, levels = name), type = "bar", orientation = "h",
            marker = list(color = theme_col(as.character(IND[code == cd, theme]))), hovertemplate = "%{y}: %{x:,.1f}<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(yaxis = list(title = NULL), xaxis = list(title = unit_label(cd)))
  })
  output$elems <- renderDT({
    x <- ELEM[code == sel(), .(Part = part, `Data element` = de_name, Category = category, `Reporting form` = period_type, UID = de)]
    datatable(x, rownames = FALSE, options = list(pageLength = 8, dom = "tip", order = list(list(0, "desc"))))
  })
  output$other <- renderReactable({
    reactable(OTHER, groupBy = "page", defaultExpanded = TRUE, searchable = TRUE, pagination = FALSE, compact = TRUE,
      columns = list(page = colDef(name = "Page", minWidth = 150),
                     measure = colDef(name = "Measure", minWidth = 200, style = list(fontWeight = 600)),
                     definition = colDef(name = "Definition", minWidth = 420, style = list(fontSize = ".8rem", color = BRAND$ink2)),
                     unit = colDef(name = "Unit", width = 110),
                     levels = colDef(name = "Levels", minWidth = 150, style = list(fontSize = ".8rem")),
                     source = colDef(name = "Source", minWidth = 180, style = list(fontSize = ".8rem"))))
  })
})
