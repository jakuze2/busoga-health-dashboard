# Climate & environment: rainfall, drought (SPI-3), heat, air quality, seasonal outlook, climate x health.

CLIM    <- if (file.exists(file.path(DATA, "climate.rds"))) readRDS(file.path(DATA, "climate.rds"))
OUTLOOK <- if (file.exists(file.path(DATA, "outlook.rds"))) readRDS(file.path(DATA, "outlook.rds"))
SPI_COL <- c("Extremely dry" = "#962323", "Severely dry" = "#e34948", "Moderately dry" = "#f8b4ad",
             "Near normal" = "#f0efec", "Moderately wet" = "#9ec5f4", "Very wet" = "#256abf")

climate_ui <- function(id) {
  ns <- NS(id)
  if (is.null(CLIM)) return(p("Climate data are not available."))
  cm <- sort(unique(CLIM$period), decreasing = TRUE)
  tagList(
    page_head("Early warning", "Climate & environment",
              "Rainfall and drought for every sub-county (CHIRPS satellite rainfall, 5 km, since 1981), heat (ERA5-Land) and air pollution (CAMS) by district, and the ECMWF seasonal outlook. Use it alongside the health data: rainfall usually drives malaria one to two months later."),
    filter_bar(area_ui(ns("area"), depth = 3),
               selectInput(ns("month"), "Month (for the map)", setNames(cm, fmt_month(cm)), width = "170px")),
    uiOutput(ns("hero")),
    navset_card_underline(full_screen = TRUE,
      nav_panel("Rainfall & drought", plotlyOutput(ns("rain"), height = 520),
                info_note("SPI-3 (Standardised Precipitation Index): the last 3 months of rainfall compared with the same 3 months in 1991-2020 for this area. Below -1 is a moderate, below -1.5 a severe and below -2 an extreme drought signal. The latest CHIRPS month can be preliminary.")),
      nav_panel("Drought map", layout_columns(col_widths = c(8, 4), leafletOutput(ns("map"), height = 540), plotlyOutput(ns("spi_rank"), height = 540))),
      nav_panel("Heat", plotlyOutput(ns("heat"), height = 460),
                info_note("ERA5-Land at district level (sub-counties show their district). Heat days: days hotter than the district's 90th percentile of daily maximum temperature in 2011-2020.")),
      nav_panel("Air quality", plotlyOutput(ns("pm"), height = 460),
                info_note("CAMS model estimates of fine particulate matter (PM2.5), district level, from September 2022. WHO guidelines: 5 µg/m³ annual mean, 15 µg/m³ over 24 hours.")),
      nav_panel("Seasonal outlook", layout_columns(col_widths = c(7, 5), plotlyOutput(ns("outlook"), height = 460), reactableOutput(ns("outlook_tbl"))),
                info_note("ECMWF SEAS5 ensemble forecast. The band is the 10th to 90th percentile of ensemble members; the normal is CHIRPS 1991-2020. 'Chance of a dry month' is the share of members below the lower tercile of that normal (one in three is the usual chance). Model rainfall is not bias-corrected, so read it as indicative.")),
      nav_panel("Climate & health", plotlyOutput(ns("ch"), height = 560),
                info_note("Top: confirmed malaria cases (monthly, HMIS). Bottom: rainfall. The bar chart shows how strongly malaria follows rainfall 0 to 3 months earlier (correlation of deviations from each series' own seasonal pattern)."))),
    div(class = "footer-note", "Rainfall: CHIRPS v2.0 (Climate Hazards Center, UC Santa Barbara), via the IRI Data Library. Temperature and air quality: contains modified Copernicus Climate Change Service and Copernicus Atmosphere Monitoring Service information; seasonal outlook: ECMWF SEAS5; served by Open-Meteo (CC BY 4.0).")
  )
}

climate_server <- function(id) moduleServer(id, function(input, output, session) {
  if (is.null(CLIM)) return()
  ns <- session$ns
  area <- area_server("area")
  cl <- reactive({ a <- area(); CLIM[level == a$level & uid == a$uid][order(period)] })

  output$hero <- renderUI({
    x <- cl(); req(nrow(x)); last <- tail(x, 1)
    tile <- function(lab, v, sub) div(div(class = "h-lab", lab), div(class = "h-val", v), div(class = "h-sub", sub))
    ol <- if (!is.null(OUTLOOK)) { a <- area(); u <- if (a$level %in% c("region", "district")) a$uid else OU$uid_l3[OU$uid == a$uid]
      OUTLOOK[uid == u][order(period)] } else NULL
    div(class = "hero",
        tile(fmt_month(last$period), sprintf("%.0f mm", last$rain_mm), sprintf("rainfall, %+.0f%% vs normal", last$rain_anom)),
        tile("Drought index (SPI-3)", sprintf("%+.1f", last$spi3), last$spi_class %||% ""),
        tile("Heat days", sprintf("%.0f", last$heat_days), sprintf("mean daily max %.1f°C", last$tmax_c)),
        tile("PM2.5", if (is.na(last$pm25)) "–" else sprintf("%.0f µg/m³", last$pm25),
             if (is.na(last$pm25_days)) "" else sprintf("%.0f days above WHO 24-h guideline", last$pm25_days)),
        tile("Next 3 months", if (!is.null(ol) && nrow(ol)) sprintf("%+.0f%%", mean(head(ol$rain_anom, 3))) else "–",
             if (!is.null(ol) && nrow(ol)) sprintf("rainfall vs normal; %.0f%% chance of a dry month", 100 * mean(head(ol$p_dry, 3))) else "seasonal outlook"))
  })

  output$rain <- renderPlotly({
    x <- cl(); if (!nrow(x)) return(empty_plot())
    x[, d := ym_date(period)]
    x[, normal := rain_mm / (1 + rain_anom / 100)]  # the unit's 1991-2020 mean for that month
    p1 <- plot_ly(x, x = ~d) |>
      add_bars(y = ~rain_mm, name = "Rainfall", marker = list(color = "#3987e5"), hovertemplate = "%{x|%b %Y}: %{y:.0f} mm<extra></extra>") |>
      add_lines(y = ~normal, name = "1991-2020 normal", line = list(color = BRAND$ink2, dash = "dot", width = 1.5),
                hovertemplate = "Normal: %{y:.0f} mm<extra></extra>") |>
      layout(yaxis = list(title = "mm per month", gridcolor = BRAND$grid))
    p2 <- plot_ly(x, x = ~d, y = ~spi3, type = "bar", name = "SPI-3",
                  marker = list(color = unname(SPI_COL[x$spi_class %||% "Near normal"])),
                  text = ~spi_class, hovertemplate = "%{x|%b %Y}: SPI-3 %{y:.2f} (%{text})<extra></extra>") |>
      layout(yaxis = list(title = "SPI-3", gridcolor = BRAND$grid, zeroline = TRUE, zerolinecolor = BRAND$base),
             shapes = lapply(c(-1, -1.5, -2), function(h) list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = h, y1 = h,
                                                               line = list(color = "#e34948", dash = "dot", width = .8))))
    subplot(p1, p2, nrows = 2, shareX = TRUE, heights = c(.6, .4), titleY = TRUE, margin = .04) |>
      layout(showlegend = TRUE, legend = list(orientation = "h", y = -0.1), hovermode = "x unified",
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", font = list(family = "Arial, Helvetica, sans-serif")) |>
      config(displaylogo = FALSE)
  })

  spi_month <- reactive(CLIM[level == "subcounty" & period == as.integer(input$month)])
  output$map <- renderLeaflet({
    x <- spi_month()
    g <- merge(GEO$subcounty, x[, .(uid, spi3, spi_class, rain_mm, rain_anom)], by = "uid", all.x = TRUE)
    pal <- colorBin(c("#962323", "#e34948", "#f8b4ad", "#f0efec", "#9ec5f4", "#256abf"), bins = c(-Inf, -2, -1.5, -1, 1, 1.5, Inf),
                    na.color = "#e8e7e2")
    base_map() |>
      addPolygons(data = g, fillColor = ~pal(spi3), fillOpacity = .9, color = "white", weight = .7,
                  label = lapply(sprintf("<b>%s</b><br>SPI-3 %+.2f (%s)<br>Rainfall %.0f mm (%+.0f%% vs normal)",
                                         htmlEscape(g$name), g$spi3, g$spi_class, g$rain_mm, g$rain_anom), HTML)) |>
      addPolylines(data = GEO$district, color = BRAND$navy, weight = 1.5) |>
      addLegend("bottomright", colors = unname(SPI_COL), labels = names(SPI_COL), title = sprintf("SPI-3, %s", fmt_month(as.integer(input$month))))
  })
  output$spi_rank <- renderPlotly({
    x <- spi_month()[order(spi3)]; if (!nrow(x)) return(empty_plot())
    x <- head(x, 25); x[, name := ou_name[uid]]
    plot_ly(x, x = ~spi3, y = ~factor(name, levels = rev(name)), type = "bar", orientation = "h",
            marker = list(color = unname(SPI_COL[x$spi_class])), hovertemplate = "%{y}: %{x:.2f}<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(title = list(text = "<b>Driest 25 sub-counties</b>", font = list(size = 12), x = 0),
                                            yaxis = list(title = NULL, tickfont = list(size = 9)), xaxis = list(title = "SPI-3"), margin = list(t = 30))
  })

  output$heat <- renderPlotly({
    x <- cl(); if (!nrow(x)) return(empty_plot()); x[, d := ym_date(period)]
    p1 <- plot_ly(x, x = ~d, y = ~tmax_c, type = "scatter", mode = "lines", name = "Mean daily maximum (°C)",
                  line = list(color = "#eb6834", width = 2), hovertemplate = "%{x|%b %Y}: %{y:.1f}°C<extra></extra>") |>
      layout(yaxis = list(title = "°C", gridcolor = BRAND$grid))
    p2 <- plot_ly(x, x = ~d, y = ~heat_days, type = "bar", name = "Heat days", marker = list(color = "#e34948"),
                  hovertemplate = "%{x|%b %Y}: %{y:.0f} heat days<extra></extra>") |> layout(yaxis = list(title = "days", gridcolor = BRAND$grid))
    subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE, margin = .05) |>
      layout(hovermode = "x unified", legend = list(orientation = "h", y = -0.12), paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)") |> config(displaylogo = FALSE)
  })

  output$pm <- renderPlotly({
    a <- area(); u <- if (a$level %in% c("region", "district")) a$uid else OU$uid_l3[OU$uid == a$uid]
    x <- CLIM[uid == u & !is.na(pm25)][order(period)]
    if (!nrow(x)) return(empty_plot("PM2.5 is available from September 2022, district level"))
    plot_ly(x, x = ~ym_date(period), y = ~pm25, type = "scatter", mode = "lines+markers", name = sprintf("PM2.5, %s", ou_name[[u]]),
            line = list(color = "#4a3aa7", width = 2), marker = list(size = 5),
            hovertemplate = "%{x|%b %Y}: %{y:.0f} µg/m³<extra></extra>") |>
      plotly_base(ytitle = "µg/m³ (monthly mean)") |>
      layout(shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 15, y1 = 15, line = list(color = STATUS$critical, dash = "dash")),
                           list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 5, y1 = 5, line = list(color = STATUS$serious, dash = "dot"))),
             annotations = list(list(xref = "paper", x = 1, y = 15, text = "WHO 24-hour guideline", showarrow = FALSE, xanchor = "right", yanchor = "bottom", font = list(size = 10)),
                                list(xref = "paper", x = 1, y = 5, text = "WHO annual guideline", showarrow = FALSE, xanchor = "right", yanchor = "bottom", font = list(size = 10))))
  })

  outlook_u <- reactive({ a <- area(); u <- if (a$level %in% c("region", "district")) a$uid else OU$uid_l3[OU$uid == a$uid]
    if (is.null(OUTLOOK)) NULL else OUTLOOK[uid == u][order(period)] })
  output$outlook <- renderPlotly({
    o <- outlook_u(); if (is.null(o) || !nrow(o)) return(empty_plot("No outlook available"))
    o[, d := ym_date(period)]
    plot_ly(o, x = ~d) |>
      add_ribbons(ymin = ~rain_lo, ymax = ~rain_hi, name = "Ensemble 10-90%", fillcolor = "rgba(57,135,229,.2)", line = list(width = 0), hoverinfo = "skip") |>
      add_lines(y = ~rain_mm, name = "Forecast rainfall", line = list(color = "#2a78d6", width = 2.5),
                hovertemplate = "%{x|%b %Y}: %{y:.0f} mm forecast<extra></extra>") |>
      add_lines(y = ~rain_normal, name = "1991-2020 normal", line = list(color = BRAND$ink2, dash = "dot"),
                hovertemplate = "Normal: %{y:.0f} mm<extra></extra>") |>
      plotly_base(ytitle = "mm per month") |> layout(hovermode = "x unified")
  })
  output$outlook_tbl <- renderReactable({
    req(OUTLOOK); m <- sort(unique(OUTLOOK$period))
    x <- OUTLOOK[level == "district", .(District = ou_name[uid], period, rain_anom, p_dry)]
    w <- dcast(x, District ~ period, value.var = "p_dry")
    cols <- lapply(as.character(m), function(k) colDef(name = format(ym_date(as.integer(k)), "%b %Y"), align = "center",
      cell = function(v) { if (is.na(v)) return("–")
        col <- if (v >= .6) STATUS$critical else if (v >= .45) STATUS$serious else if (v >= .33) STATUS$warning else STATUS$good
        div(class = "sc-cell", style = sprintf("background:%s33;justify-content:center", col), sprintf("%.0f%%", 100 * v)) }))
    names(cols) <- as.character(m)
    reactable(w, columns = c(list(District = colDef(minWidth = 120, style = list(fontWeight = 600))), cols), compact = TRUE,
              pagination = FALSE, theme = reactableTheme(headerStyle = list(fontSize = ".75rem")))
  })

  output$ch <- renderPlotly({
    a <- area(); x <- cl(); if (!nrow(x)) return(empty_plot())
    lvl <- a$level
    mal <- summarise_ind("MAL09", lvl, MONTH_MIN, MONTH_MAX, uids = a$uid, by = "month")[order(bucket)]
    if (!nrow(mal)) return(empty_plot("No malaria data for this area"))
    j <- merge(mal[, .(period = bucket, cases = value)], x[, .(period, rain_mm)], by = "period")
    j[, mo := period %% 100L]
    j[, `:=`(c_dev = log1p(cases) - mean(log1p(cases)), r_dev = rain_mm - mean(rain_mm)), by = mo]
    lags <- 0:3
    cors <- vapply(lags, function(L) { r <- shift(j$r_dev, L); suppressWarnings(cor(j$c_dev, r, use = "complete.obs")) }, 0)
    p1 <- plot_ly(j, x = ~ym_date(period), y = ~cases, type = "scatter", mode = "lines", name = "Confirmed malaria",
                  line = list(color = "#008300", width = 2), hovertemplate = "%{x|%b %Y}: %{y:,.0f} cases<extra></extra>") |>
      layout(yaxis = list(title = "cases", gridcolor = BRAND$grid))
    p2 <- plot_ly(j, x = ~ym_date(period), y = ~rain_mm, type = "bar", name = "Rainfall", marker = list(color = "#3987e5"),
                  hovertemplate = "%{x|%b %Y}: %{y:.0f} mm<extra></extra>") |> layout(yaxis = list(title = "mm", gridcolor = BRAND$grid))
    p3 <- plot_ly(x = paste0(lags, " mo"), y = cors, type = "bar", name = "Correlation with rainfall N months earlier",
                  marker = list(color = BRAND$navy), hovertemplate = "Rainfall %{x} earlier: r = %{y:.2f}<extra></extra>") |>
      layout(yaxis = list(title = "r", range = c(-1, 1), gridcolor = BRAND$grid), xaxis = list(title = "rainfall lead"))
    subplot(subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE, margin = .04), p3, widths = c(.72, .28), titleY = TRUE, titleX = TRUE, margin = .05) |>
      layout(legend = list(orientation = "h", y = -0.12), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
      config(displaylogo = FALSE)
  })
})
