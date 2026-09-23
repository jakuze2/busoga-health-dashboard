# Epidemics: weekly 033B surveillance, epidemic thresholds, alerts and 4-week predictions.

EPI  <- if (file.exists(file.path(DATA, "epi_week.rds"))) readRDS(file.path(DATA, "epi_week.rds"))
EPIF <- if (file.exists(file.path(DATA, "epi_forecast.rds"))) readRDS(file.path(DATA, "epi_forecast.rds"))
EPIR <- if (file.exists(file.path(DATA, "epi_reporting.rds"))) readRDS(file.path(DATA, "epi_reporting.rds"))
if (!is.null(EPI)) setkey(EPI, level, disease, uid)
EPI_STATUS <- c("Epidemic threshold" = STATUS$critical, "Alert threshold" = STATUS$serious,
                "Notifiable case" = "#7a1fa2", "Normal" = "#cfe8cf", "No baseline" = "#e8e7e2")
EPI_ICON <- c("Epidemic threshold" = "▲▲", "Alert threshold" = "▲", "Notifiable case" = "◆",
              "Normal" = "–", "No baseline" = "–")
fmt_week <- function(d) sprintf("W%s %s (from %s)", format(d, "%V"), format(d, "%G"), format(d, "%d %b"))

epidemic_ui <- function(id) {
  ns <- NS(id)
  if (is.null(EPI)) return(p("Weekly surveillance data are not available."))
  dz <- unique(EPI[, .(disease, kind)])[order(kind == "notifiable", disease)]
  tagList(
    page_head("Early warning", "Epidemic alerts and predictions",
              "Weekly surveillance (HMIS 033B). Endemic diseases are compared with their normal channel, the usual level for the same weeks in previous years (WHO method). For immediately notifiable diseases any case is an alert. Predictions look four weeks ahead."),
    filter_bar(
      selectInput(ns("disease"), "Disease", split(dz$disease, ifelse(dz$kind == "seasonal", "Endemic / seasonal", "Immediately notifiable")),
                  selected = "Malaria (confirmed)", width = "290px"),
      selectInput(ns("level"), "Level", c("Districts / cities" = "district", "DLGs / municipalities" = "dlg",
                                          "Sub-counties / divisions" = "subcounty", "Health facilities" = "facility"),
                  selected = "district", width = "200px"),
      selectizeInput(ns("unit"), "Area / facility for the chart", choices = NULL, width = "320px"),
      selectInput(ns("week"), "Week", NULL, width = "240px")),
    uiOutput(ns("hero")),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE, card_header(textOutput(ns("ch_title"), inline = TRUE)), plotlyOutput(ns("channel"), height = 440),
           info_note("Grey band: median to alert threshold (3rd quartile). Red dashed: epidemic threshold (mean + 2 SD). Coloured dashed: 4-week prediction with 80% interval.")),
      card(full_screen = TRUE, card_header("Map for the selected week"), leafletOutput(ns("map"), height = 480))),
    navset_card_underline(full_screen = TRUE,
      nav_panel("Alert board", reactableOutput(ns("board"))),
      nav_panel("Next 4 weeks: risk of crossing the threshold", reactableOutput(ns("risk")),
                info_note("Probability that weekly cases exceed the alert threshold, from the forecast model. Treat above 50% as a prompt to check stocks, staffing and case management; it is not a declaration of an epidemic.")),
      nav_panel("Weeks x areas", plotlyOutput(ns("heat"), height = 520)),
      nav_panel("Reporting", plotlyOutput(ns("rep"), height = 360),
                info_note("A drop in weekly reporting can hide a rise in cases; read alerts together with completeness.")))
  )
}

epidemic_server <- function(id) moduleServer(id, function(input, output, session) {
  if (is.null(EPI)) return()
  ns <- session$ns
  weeks <- sort(unique(EPI$week_start), decreasing = TRUE)
  updateSelectInput(session, "week", choices = setNames(as.character(weeks), fmt_week(weeks)), selected = as.character(weeks[1]))
  wk <- reactive(as.Date(input$week %||% as.character(weeks[1])))

  observeEvent(list(input$level, input$disease), {
    u <- unique(EPI[.(input$level, input$disease), uid, nomatch = NULL])
    ch <- setNames(u, ou_name[u]); ch <- ch[order(names(ch))]
    lat <- EPI[.(input$level, input$disease), nomatch = NULL][week_start == wk()]
    top <- if (nrow(lat)) lat[order(-(status %in% c("Epidemic threshold", "Alert threshold", "Notifiable case")), -cases), uid[1]] else u[1]
    updateSelectizeInput(session, "unit", choices = c(setNames(META$region_uid, "Busoga (all)"), ch), selected = top, server = TRUE)
  })

  output$hero <- renderUI({
    w <- wk(); x <- EPI[level == input$level & week_start == w]
    tile <- function(lab, v, sub) div(div(class = "h-lab", lab), div(class = "h-val", v), div(class = "h-sub", sub))
    r <- EPIR[level == "region" & week_start == w]
    div(class = "hero",
        tile("Week", format(w, "W%V %G"), sprintf("%s to %s", format(w, "%d %b"), format(w + 6, "%d %b %Y"))),
        tile("At epidemic threshold", x[status == "Epidemic threshold", uniqueN(paste(uid, disease))], sprintf("%s-disease pairs", LEVEL_PLURAL[[input$level]])),
        tile("At alert threshold", x[status == "Alert threshold", uniqueN(paste(uid, disease))], "above the 3rd quartile"),
        tile("Notifiable disease cases", format(x[kind == "notifiable", sum(cases)], big.mark = ","),
             paste(unique(x[kind == "notifiable" & cases > 0, disease]), collapse = ", ") %||% ""),
        tile("Malaria cases, Busoga", format(EPI[level == "region" & disease == "Malaria (confirmed)" & week_start == w, sum(cases)], big.mark = ","), "confirmed, this week"),
        tile("033B reporting", if (nrow(r) && r$expected > 0) sprintf("%.0f%%", 100 * r$actual / r$expected) else "–", "weekly reports received"))
  })

  output$ch_title <- renderText(sprintf("%s · %s", input$disease, ou_name[[input$unit %||% META$region_uid]] %||% "Busoga"))
  output$channel <- renderPlotly({
    req(input$unit); u <- input$unit
    lvl <- if (u == META$region_uid) "region" else input$level
    d <- EPI[.(lvl, input$disease, u), nomatch = NULL][week_start > wk() - 7 * 104 & week_start <= wk()]
    if (!nrow(d)) return(empty_plot())
    col <- "#2a78d6"
    p <- plot_ly(d, x = ~week_start)
    if (any(!is.na(d$alert_thr)))
      p <- p |> add_ribbons(ymin = ~base_median, ymax = ~alert_thr, name = "Normal channel", fillcolor = "rgba(137,135,129,.22)",
                            line = list(width = 0), hoverinfo = "skip") |>
        add_lines(y = ~epi_thr, name = "Epidemic threshold", line = list(color = STATUS$critical, dash = "dash", width = 1.5),
                  hovertemplate = "Epidemic threshold: %{y:,.0f}<extra></extra>")
    p <- p |> add_lines(y = ~cases, name = "Cases", line = list(color = BRAND$ink, width = 2),
                        hovertemplate = "%{x|W%V %G}: %{y:,.0f} cases<extra></extra>")
    fl <- d[status %in% c("Alert threshold", "Epidemic threshold", "Notifiable case")]
    if (nrow(fl)) p <- p |> add_markers(data = fl, x = ~week_start, y = ~cases, name = "Flagged week",
                                        marker = list(color = unname(EPI_STATUS[fl$status]), size = 9, line = list(color = "white", width = 1)),
                                        text = ~status, hovertemplate = "%{x|W%V %G}: %{y:,.0f} (%{text})<extra></extra>")
    f <- if (!is.null(EPIF)) EPIF[level == lvl & uid == u & disease == input$disease] else NULL
    if (!is.null(f) && nrow(f) && wk() == weeks[1])
      p <- p |> add_ribbons(data = f, x = ~week_start, ymin = ~lo80, ymax = ~hi80, name = "Prediction 80%", fillcolor = paste0(col, "33"),
                            line = list(width = 0), hoverinfo = "skip") |>
        add_lines(data = f, x = ~week_start, y = ~mean, name = "Prediction", line = list(color = col, dash = "dash", width = 2.5),
                  text = ~sprintf("%.0f%%", 100 * p_exceed),
                  hovertemplate = "%{x|W%V %G}: %{y:,.0f} predicted, P(alert) %{text}<extra></extra>")
    p |> plotly_base(ytitle = "Cases per week") |> layout(hovermode = "x unified")
  })

  output$map <- renderLeaflet({
    w <- wk(); x <- EPI[level == "subcounty" & disease == input$disease & week_start == w]
    g <- merge(GEO$subcounty, x[, .(uid, cases, status)], by = "uid", all.x = TRUE)
    g$status[is.na(g$status)] <- "Normal"; g$cases[is.na(g$cases)] <- 0
    m <- base_map() |>
      addPolygons(data = g, fillColor = unname(EPI_STATUS[g$status]), fillOpacity = .85, color = "white", weight = .7,
                  label = lapply(sprintf("<b>%s</b><br>%s: %s cases<br>%s", htmlEscape(g$name), htmlEscape(input$disease),
                                         format(g$cases, big.mark = ","), g$status), HTML)) |>
      addPolylines(data = GEO$district, color = BRAND$navy, weight = 1.5)
    fx <- EPI[level == "facility" & disease == input$disease & week_start == w & cases > 0]
    fx <- merge(fx, FAC_PTS[, .(uid, name, lat, lon)], by = "uid")
    if (nrow(fx)) m <- m |> addCircleMarkers(data = fx, lng = ~lon, lat = ~lat, radius = ~pmin(3 + sqrt(cases), 14), color = "white",
                                            weight = 1, fillColor = BRAND$ink, fillOpacity = .55,
                                            label = ~sprintf("%s: %s cases", name, format(cases, big.mark = ",")))
    m |> addLegend("bottomright", colors = unname(EPI_STATUS[1:4]), labels = names(EPI_STATUS)[1:4], opacity = .9,
                   title = "Sub-county status")
  })

  output$board <- renderReactable({
    w <- wk()
    x <- EPI[level == input$level & week_start <= w & week_start > w - 28 &
               status %in% c("Epidemic threshold", "Alert threshold", "Notifiable case")]
    validate(need(nrow(x), "No alerts in the last four weeks at this level."))
    x <- x[, .(weeks_flagged = .N, latest_status = status[which.max(week_start)], latest_week = max(week_start),
               cases_4w = sum(cases), deaths_4w = sum(deaths), run = run[which.max(week_start)]), by = .(uid, disease)]
    x[, `:=`(Area = ou_name[uid], District = OU$district[match(uid, OU$uid)])]
    x[, sev := match(latest_status, names(EPI_STATUS))]
    x <- x[order(sev, -cases_4w)]
    reactable(x[, .(Area, District, disease, latest_status, latest_week, weeks_flagged, run, cases_4w, deaths_4w)],
      compact = TRUE, searchable = TRUE, pagination = TRUE, defaultPageSize = 15, highlight = TRUE,
      columns = list(disease = colDef(name = "Disease", minWidth = 170),
        latest_status = colDef(name = "Status", minWidth = 160, cell = function(v) span(
          style = sprintf("color:%s;font-weight:700", EPI_STATUS[[v]]), EPI_ICON[[v]], " ", v)),
        latest_week = colDef(name = "Latest flagged week", cell = function(v) format(as.Date(v), "W%V %G")),
        weeks_flagged = colDef(name = "Weeks flagged (of 4)", align = "right"),
        run = colDef(name = "Consecutive weeks", align = "right"),
        cases_4w = colDef(name = "Cases, 4 weeks", align = "right", format = colFormat(separators = TRUE)),
        deaths_4w = colDef(name = "Deaths, 4 weeks", align = "right")))
  })

  output$risk <- renderReactable({
    validate(need(!is.null(EPIF) && nrow(EPIF), "No predictions available."))
    x <- EPIF[level == input$level & disease == input$disease]
    validate(need(nrow(x), "No predictions for this disease at this level (too few cases for a reliable model)."))
    x <- x[, .(max_p = max(p_exceed), peak_week = week_start[which.max(p_exceed)], cases_next4 = sum(mean),
               lo = sum(lo80), hi = sum(hi80)), by = uid][order(-max_p)]
    x[, `:=`(Area = ou_name[uid], District = OU$district[match(uid, OU$uid)])]
    reactable(x[, .(Area, District, max_p, peak_week, cases_next4, lo, hi)], compact = TRUE, searchable = TRUE,
      pagination = TRUE, defaultPageSize = 15,
      columns = list(max_p = colDef(name = "Highest weekly P(alert)", minWidth = 170, cell = function(v) {
          col <- if (v >= .7) STATUS$critical else if (v >= .5) STATUS$serious else if (v >= .3) STATUS$warning else STATUS$good
          div(style = "display:flex;gap:.4rem;align-items:center",
              div(style = "flex:1;height:8px;background:#f0efec;border-radius:4px",
                  div(style = sprintf("width:%.0f%%;height:8px;background:%s;border-radius:4px", 100 * v, col))),
              span(style = "min-width:2.8em;text-align:right", sprintf("%.0f%%", 100 * v))) }),
        peak_week = colDef(name = "Riskiest week", cell = function(v) format(as.Date(v), "W%V %G")),
        cases_next4 = colDef(name = "Predicted cases, 4 weeks", align = "right", cell = function(v) format(round(v), big.mark = ",")),
        lo = colDef(name = "80% low", align = "right", cell = function(v) format(round(v), big.mark = ",")),
        hi = colDef(name = "80% high", align = "right", cell = function(v) format(round(v), big.mark = ","))))
  })

  output$heat <- renderPlotly({
    w <- wk(); lvl <- if (input$level == "facility") "subcounty" else input$level
    x <- EPI[level == lvl & disease == input$disease & week_start > w - 7 * 52 & week_start <= w]
    if (!nrow(x)) return(empty_plot())
    x[, ratio := fifelse(!is.na(alert_thr) & alert_thr > 0, cases / alert_thr, NA_real_)]
    x[, name := ou_name[uid]]
    z_col <- if (x[, all(is.na(ratio))]) "cases" else "ratio"
    wdt <- dcast(x, name ~ week_start, value.var = z_col)
    z <- as.matrix(wdt[, -1])
    plot_ly(x = as.Date(colnames(z)), y = wdt$name, z = z, type = "heatmap", xgap = 1, ygap = 1,
            zmin = 0, zmax = if (z_col == "ratio") 2 else NULL,
            colorscale = if (z_col == "ratio") list(c(0, "#f4f3ef"), c(.5, "#fab219"), c(1, "#d03b3b")) else
              list(c(0, "#f4f3ef"), c(1, "#256abf")),
            colorbar = list(title = if (z_col == "ratio") "Cases / alert<br>threshold" else "Cases"),
            hovertemplate = paste0("%{y}<br>%{x|W%V %G}: %{z:.2f}", if (z_col == "ratio") "× threshold" else " cases", "<extra></extra>")) |>
      plotly_base(legend = FALSE) |> layout(yaxis = list(autorange = "reversed", title = "", tickfont = list(size = 9)))
  })

  output$rep <- renderPlotly({
    u <- input$unit %||% META$region_uid; lvl <- if (u == META$region_uid) "region" else input$level
    r <- EPIR[level == lvl & uid == u & week_start > wk() - 7 * 104][order(week_start)]
    if (!nrow(r)) return(empty_plot())
    plot_ly(r, x = ~week_start, y = ~100 * actual / pmax(expected, 1), type = "bar", marker = list(color = BRAND$navy),
            hovertemplate = "%{x|W%V %G}: %{y:.0f}% of weekly reports<extra></extra>") |>
      plotly_base(ytitle = "% of expected 033B reports", legend = FALSE) |> layout(yaxis = list(range = c(0, 105)))
  })
})
