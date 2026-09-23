# Busoga in numbers: census history, access to services, hazards, agriculture & food, commerce.

opt_read <- function(f) { p <- file.path(DATA, f); if (file.exists(p)) readRDS(p) }
CEN <- opt_read("open_census.rds"); HEI <- opt_read("open_heigit.rds"); PRICES <- opt_read("open_prices.rds")
COM <- opt_read("open_commerce.rds"); LAND <- opt_read("open_land.rds")
COM_COL <- c("Banks & microfinance" = "#3949AB", "ATMs" = "#5C6BC0", "Mobile money & forex" = "#00897B",
             "Fuel stations" = "#C0582B", "Markets" = "#C17D11", "Agro-input shops" = "#2E7D32", "Shops & wholesale" = "#8E44AD")

region_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Busoga in numbers", "Busoga beyond the clinic",
              "Open data about the region: how the population has grown since the census, how close people live to services, flood and drought exposure, food prices and farming, and where commerce happens."),
    navset_card_underline(full_screen = TRUE,
      nav_panel("Census & population history",
        layout_columns(col_widths = c(7, 5), plotlyOutput(ns("cen_trend"), height = 440), plotlyOutput(ns("cen_dist"), height = 440)),
        info_note("UBOS Population and Housing Census 2002 and UBOS projections on the 2002 census base (districts of that time: Bugiri, Iganga, Jinja, Kamuli, Kaliro, Mayuge), UBOS 2023 subnational projections (via OCHA HDX), and the Ministry of Health 107a projections used in DHIS2. District-level 2014 and 2024 census tables are not yet published as open data; they will be added when they are.")),
      nav_panel("Access to services",
        layout_columns(col_widths = c(6, 6), plotlyOutput(ns("acc_health"), height = 440), plotlyOutput(ns("acc_edu"), height = 440)),
        info_note("HeiGIT accessibility indicators (2026), built with openrouteservice on OpenStreetMap roads and facilities: share of each district's population that can reach a hospital or primary health care within 30 minutes, 1 hour or 2 hours, and a school within 5 km. CC BY-SA.")),
      nav_panel("Floods & vulnerability",
        layout_columns(col_widths = c(6, 6), plotlyOutput(ns("flood"), height = 440), reactableOutput(ns("vul"))),
        info_note("HeiGIT risk assessment indicators (2026): people and cropland exposed to 30 cm or more of flooding in a 1-in-100-year flood, and vulnerability (children, elderly, women of reproductive age, rural share). CC BY-SA.")),
      nav_panel("Agriculture & food",
        layout_columns(col_widths = c(7, 5),
          div(selectInput(ns("commodity"), "Food price", NULL, width = "260px"), plotlyOutput(ns("prices"), height = 400)),
          plotlyOutput(ns("seasons"), height = 460)),
        layout_columns(col_widths = c(6, 6), plotlyOutput(ns("crop_flood"), height = 320), plotlyOutput(ns("land"), height = 320)),
        info_note("Food prices: WFP / FAO / UBOS market monitoring for Iganga and Jinja markets (CC BY-IGO). Rainfall seasons: CHIRPS 1991-2020 normal against the latest year. Cropland exposed to flooding: HeiGIT 2026. Forest reserves, rangeland and water: UBOS land-use layer (2006).")),
      nav_panel("Commerce & markets",
        layout_columns(col_widths = c(7, 5), leafletOutput(ns("com_map"), height = 520), plotlyOutput(ns("com_bar"), height = 520)),
        info_note("Banks, ATMs, mobile money, fuel stations, markets, agro-input shops and supermarkets mapped in OpenStreetMap (ODbL). OSM coverage is incomplete, especially for small businesses; counts are a minimum."))),
    div(class = "footer-note", "Open data used under their licences: UBOS, OCHA HDX, HeiGIT (CC BY-SA), WFP (CC BY-IGO), CHIRPS, OpenStreetMap (ODbL).")
  )
}

region_server <- function(id) moduleServer(id, function(input, output, session) {
  output$cen_trend <- renderPlotly({
    req(CEN); x <- CEN$region[order(year)]
    cols <- c("UBOS Census 2002" = BRAND$maroon, "UBOS projection (2002 census base)" = "#9b9a94",
              "UBOS projection 2023 (HDX)" = "#C17D11", "DHIS2 107a projection (MoH)" = BRAND$navy)
    p <- plot_ly()
    for (s in names(cols)) { z <- x[source == s]; if (!nrow(z)) next
      p <- p |> add_trace(data = z, x = ~year, y = ~pop, type = "scatter", mode = if (nrow(z) > 1) "lines+markers" else "markers",
                          name = s, line = list(color = cols[[s]], width = 2.5), marker = list(color = cols[[s]], size = if (s == "UBOS Census 2002") 14 else 7,
                          symbol = if (s == "UBOS Census 2002") "diamond" else "circle"),
                          hovertemplate = paste0(s, " %{x}: %{y:,.0f}<extra></extra>")) }
    p |> plotly_base(ytitle = "People") |> layout(title = list(text = "<b>Busoga population, census and projections</b>", x = 0, font = list(size = 13)),
                                                   margin = list(t = 36), yaxis = list(tickformat = ",.0f"))
  })
  output$cen_dist <- renderPlotly({
    req(CEN); x <- CEN$by_old_district[year %in% c(2002, 2017)]
    plot_ly(x, y = ~district_2002, x = ~pop, color = ~factor(year), colors = c(BRAND$maroon, BRAND$navy), type = "bar", orientation = "h",
            hovertemplate = "%{y} %{fullData.name}: %{x:,.0f}<extra></extra>") |>
      plotly_base() |> layout(barmode = "group", title = list(text = "<b>Districts of 2002: census 2002 and 2017 projection</b>", x = 0, font = list(size = 13)),
                              yaxis = list(title = ""), xaxis = list(title = "People", tickformat = ",.0f"), margin = list(t = 36))
  })
  acc_long <- reactive({ req(HEI); melt(HEI$access[, !c("ADM2_PCODE", "ADM_PCODE")], id.vars = "district", variable.name = "measure", value.name = "pct") })
  output$acc_health <- renderPlotly({
    x <- acc_long()[grepl("hospitals|primary_healthcare", measure)]
    x[, service := fifelse(grepl("hospitals", measure), "Hospital", "Primary health care")]
    x[, reach := sub(".*_", "", measure)]
    x <- x[reach == "1h"]
    ord <- x[service == "Primary health care"][order(pct)]$district
    plot_ly(x, y = ~factor(district, levels = ord), x = ~pct, color = ~service, colors = c(Hospital = BRAND$maroon, `Primary health care` = "#00897B"),
            type = "bar", orientation = "h", hovertemplate = "%{y}: %{x:.0f}% within 1 hour<extra>%{fullData.name}</extra>") |>
      plotly_base() |> layout(barmode = "group", xaxis = list(title = "% of people within 1 hour", range = c(0, 100)), yaxis = list(title = ""),
                              title = list(text = "<b>Reach a health facility within 1 hour</b>", x = 0, font = list(size = 13)), margin = list(t = 36))
  })
  output$acc_edu <- renderPlotly({
    x <- acc_long()[grepl("education", measure)][, reach := sub("access_pop_education_", "", measure)]
    ord <- x[reach == "5km"][order(pct)]$district
    plot_ly(x, y = ~factor(district, levels = ord), x = ~pct, color = ~reach, colors = c("#cde2fb", "#6da7ec", "#3949AB"),
            type = "bar", orientation = "h", hovertemplate = "%{y}: %{x:.0f}% within %{fullData.name}<extra></extra>") |>
      plotly_base() |> layout(barmode = "group", xaxis = list(title = "% of people", range = c(0, 100)), yaxis = list(title = ""),
                              title = list(text = "<b>Live within reach of a school</b>", x = 0, font = list(size = 13)), margin = list(t = 36))
  })
  output$flood <- renderPlotly({
    req(HEI); f <- HEI$flood; cols <- grep("^RP100_", names(f), value = TRUE)
    pc <- intersect(c("RP100_crops_30cm_croppct", "RP100_primary_healthcare_30cm_pct", "RP100_education_30cm_pct"), cols)
    if (!length(pc)) return(empty_plot("No flood indicators"))
    x <- melt(f[, c("district", pc), with = FALSE], id.vars = "district")
    x[, what := c(RP100_crops_30cm_croppct = "Cropland", RP100_primary_healthcare_30cm_pct = "Health facilities",
                  RP100_education_30cm_pct = "Schools")[as.character(variable)]]
    plot_ly(x, y = ~district, x = ~value, color = ~what, colors = c(Cropland = "#2E7D32", `Health facilities` = BRAND$maroon, Schools = "#3949AB"),
            type = "bar", orientation = "h", hovertemplate = "%{y}: %{x:.1f}%<extra>%{fullData.name}</extra>") |>
      plotly_base() |> layout(barmode = "group", xaxis = list(title = "% exposed, 1-in-100-year flood (30 cm+)"), yaxis = list(title = ""),
                              title = list(text = "<b>Flood exposure</b>", x = 0, font = list(size = 13)), margin = list(t = 36))
  })
  output$vul <- renderReactable({
    req(HEI); v <- HEI$vulnerability
    reactable(v[, .(District = district, Population = total_pop, `Children <5` = children_u5, `Women 15-49` = wra_pop, `Aged 65+` = elderly,
                    `Dependency ratio` = dependency_ratio, `Rural %` = rural_pop_perc)][order(-Population)], compact = TRUE, pagination = FALSE,
              columns = list(Population = colDef(format = colFormat(separators = TRUE)), `Children <5` = colDef(format = colFormat(separators = TRUE)),
                             `Women 15-49` = colDef(format = colFormat(separators = TRUE)), `Aged 65+` = colDef(format = colFormat(separators = TRUE))))
  })
  if (!is.null(PRICES)) updateSelectInput(session, "commodity", choices = sort(unique(PRICES$commodity)), selected = "Maize (white)")
  output$prices <- renderPlotly({
    req(PRICES, input$commodity); x <- PRICES[commodity == input$commodity][order(date)]
    p <- plot_ly()
    for (k in seq_along(unique(x$market))) { m <- unique(x$market)[k]; z <- x[market == m]
      p <- p |> add_lines(data = z, x = ~date, y = ~price, name = sprintf("%s (%s)", m, z$pricetype[1]), line = list(color = SERIES[k], width = 2),
                          hovertemplate = paste0(m, " %{x|%b %Y}: UGX %{y:,.0f}<extra></extra>")) }
    p |> plotly_base(ytitle = sprintf("UGX per %s", x$unit[1])) |> layout(hovermode = "x unified")
  })
  output$seasons <- renderPlotly({
    req(exists("CLIM") && !is.null(CLIM)); x <- CLIM[level == "region"]
    x[, `:=`(y = period %/% 100L, m = period %% 100L)]
    last <- max(x$y); cur <- x[y == last]; prev <- x[y == last - 1]
    nrm <- x[, .(normal = mean(rain_mm / (1 + rain_anom / 100))), by = m]
    plot_ly() |>
      add_bars(data = nrm, x = ~month.abb[m], y = ~normal, name = "1991-2020 normal", marker = list(color = "#d9d7ee")) |>
      add_lines(data = prev, x = ~month.abb[m], y = ~rain_mm, name = as.character(last - 1), line = list(color = "#9b9a94", width = 2)) |>
      add_lines(data = cur, x = ~month.abb[m], y = ~rain_mm, name = as.character(last), line = list(color = BRAND$navy, width = 3)) |>
      plotly_base(ytitle = "mm per month") |>
      layout(xaxis = list(categoryorder = "array", categoryarray = month.abb), title = list(text = "<b>Farming seasons: Busoga rainfall</b>", x = 0, font = list(size = 13)),
             margin = list(t = 36))
  })
  output$crop_flood <- renderPlotly({
    req(HEI); f <- HEI$flood
    if (!"RP100_crops_30cm_km2" %in% names(f)) return(empty_plot())
    plot_ly(f[order(RP100_crops_30cm_km2)], y = ~factor(district, levels = district), x = ~RP100_crops_30cm_km2, type = "bar", orientation = "h",
            marker = list(color = "#2E7D32"), hovertemplate = "%{y}: %{x:,.1f} km²<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(xaxis = list(title = "Cropland exposed to a 1-in-100-year flood (km²)"), yaxis = list(title = ""))
  })
  output$land <- renderPlotly({
    req(LAND); x <- LAND$summary
    plot_ly(x, y = ~district, x = ~pct, color = ~type, type = "bar", orientation = "h",
            colors = c("Forest Reserve" = "#2E7D32", "Water" = "#3987e5", "Rangeland" = "#C17D11", "National Park" = "#00897B", "Game Reserve" = "#8E44AD"),
            hovertemplate = "%{y}: %{x:.1f}% of land<extra>%{fullData.name}</extra>") |>
      plotly_base() |> layout(barmode = "stack", xaxis = list(title = "% of district area (UBOS 2006)"), yaxis = list(title = ""))
  })
  output$com_map <- renderLeaflet({
    req(COM); m <- base_map() |> addPolylines(data = GEO$district, color = BRAND$navy, weight = 1.4)
    for (k in names(COM_COL)) { z <- COM$points[type == k]; if (!nrow(z)) next
      m <- m |> addCircleMarkers(data = z, lng = ~lon, lat = ~lat, radius = 5, color = "white", weight = 1, fillColor = COM_COL[[k]],
                                 fillOpacity = .9, group = k, label = ~sprintf("%s (%s)", ifelse(is.na(name), k, name), k)) }
    m |> addLayersControl(overlayGroups = names(COM_COL), options = layersControlOptions(collapsed = TRUE)) |>
      addLegend("bottomright", colors = unname(COM_COL), labels = names(COM_COL), opacity = 1)
  })
  output$com_bar <- renderPlotly({
    req(COM); x <- COM$counts[level == "district"][, district := ou_name[uid]]
    ord <- x[, sum(N), by = district][order(V1)]$district
    p <- plot_ly()
    for (k in names(COM_COL)) { z <- x[type == k]; if (!nrow(z)) next
      p <- p |> add_bars(data = z, y = ~factor(district, levels = ord), x = ~N, name = k, orientation = "h", marker = list(color = COM_COL[[k]])) }
    p |> plotly_base() |> layout(barmode = "stack", xaxis = list(title = "Places mapped"), yaxis = list(title = ""))
  })
})
