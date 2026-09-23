# Choropleth maps from DHIS2 boundary polygons, with a ranked bar chart and click-through trend.

GEO <- lapply(c(region = "region", district = "district", dlg = "dlg", subcounty = "subcounty"), function(l)
  sf::st_read(file.path(DATA, "geo", paste0(l, ".geojson")), quiet = TRUE))
FAC_PTS <- OU[level_name == "facility" & !is.na(lat)]

class_breaks <- function(v, n = 5) {
  v <- v[is.finite(v)]
  if (length(unique(v)) < 2) return(NULL)
  b <- unique(quantile(v, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE, names = FALSE))
  if (length(b) < 3) b <- pretty(range(v), n = 4)
  b
}

# Esri light grey canvas (no API key needed; CARTO tiles now require one)
ESRI_GREY_LABELS <- "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"
add_grey_labels <- function(m, group, pane = "overlayPane")
  addTiles(m, urlTemplate = ESRI_GREY_LABELS, group = group,
           options = tileOptions(pane = pane, maxNativeZoom = 16), attribution = "Esri")

base_map <- function() {
  leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5)) |>
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") |>
    add_grey_labels("Place names") |>
    fitBounds(32.82, -0.4, 33.99, 1.48)
}

maps_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Maps", "Where is performance strongest and weakest?",
              "Colour-coded maps from the official DHIS2 district and sub-county boundaries. Blue maps: higher is better. Red maps: lower is better. Classes are quintiles of the areas shown."),
    filter_bar(
      selectInput(ns("ind"), "Indicator", indicator_choices(), selected = "ANC03", width = "330px"),
      selectInput(ns("level"), "Map level", c("Districts / cities" = "district", "DLGs / municipalities" = "dlg",
                                              "Sub-counties / divisions" = "subcounty", "Health facilities (points)" = "facility"),
                  selected = "subcounty", width = "220px"),
      period_ui(ns("period")),
      checkboxInput(ns("show_fac"), "Overlay facility locations", FALSE)),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE, card_header(textOutput(ns("map_title"), inline = TRUE)),
           leafletOutput(ns("map"), height = 620),
           div(class = "map-legend-note", "Boundaries: DHIS2 (Ministry of Health). Basemap © Esri.")),
      card(full_screen = TRUE, card_header("Ranking", span(class = "sub", "click a bar or an area to see its trend")),
           plotlyOutput(ns("rank"), height = 360),
           plotlyOutput(ns("trend"), height = 250)))
  )
}

maps_server <- function(id) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  per <- period_server("period")
  selected <- reactiveVal(NULL)

  observeEvent(input$ind, {
    if (IND[code == input$ind, area_only] && input$level == "facility")
      updateSelectInput(session, "level", selected = "subcounty")
  })
  vals <- reactive({
    p <- per(); lvl <- input$level
    if (lvl == "facility" && IND[code == input$ind, area_only]) lvl <- "subcounty"
    s <- summarise_ind(input$ind, lvl, p$from, p$to)
    s[, name := ou_name[uid]]
    list(s = s, lvl = lvl, ref = region_value(input$ind, p$from, p$to)[[input$ind]] %||% NA, p = p)
  })
  output$map_title <- renderText({
    v <- vals(); sprintf("%s · %s · %s", ind_label(input$ind), LEVEL_PLURAL[[v$lvl]], v$p$label)
  })
  output$map <- renderLeaflet({
    base_map() |>
      addPolylines(data = GEO$district, color = BRAND$navy, weight = 1.6, opacity = .9, group = "District lines") |>
      addLayersControl(overlayGroups = c("Place names", "District lines"),
                       options = layersControlOptions(collapsed = TRUE))
  })
  observe({
    req(input$map_zoom)            # wait until the map exists (it is not drawn while its tab is hidden)
    v <- vals(); cd <- input$ind; dirn <- IND[code == cd, direction]
    ramp <- seq_ramp(dirn)[2:6]
    m <- leafletProxy(ns("map")) |> clearGroup("data") |> clearControls() |> clearMarkers()
    br <- class_breaks(v$s$value)
    pal <- if (is.null(br)) colorNumeric(ramp, v$s$value, na.color = "#e8e7e2") else
      colorBin(ramp, bins = br, na.color = "#e8e7e2", right = FALSE)
    lab_of <- function(nm, val) sprintf("<b>%s</b><br>%s: <b>%s</b><br><span style='color:#898781'>Busoga: %s</span>",
                                        htmlEscape(nm), htmlEscape(ind_label(cd)), fmt_val(val, cd), fmt_val(v$ref, cd))
    if (v$lvl == "facility") {
      pts <- merge(FAC_PTS[, .(uid, name, lat, lon)], v$s[, .(uid, value)], by = "uid")
      m <- m |> addPolygons(data = GEO$subcounty, fill = FALSE, color = "#9b9a94", weight = .6, group = "data") |>
        addCircleMarkers(data = pts, lng = ~lon, lat = ~lat, radius = 6, stroke = TRUE, color = "white", weight = 1,
                         fillColor = ~pal(value), fillOpacity = .95, layerId = ~uid, group = "data",
                         label = lapply(lab_of(pts$name, pts$value), HTML))
    } else {
      g <- merge(GEO[[v$lvl]], v$s[, .(uid, value)], by = "uid", all.x = TRUE)
      m <- m |> addPolygons(data = g, fillColor = ~pal(value), fillOpacity = .88, color = "white", weight = .8,
                            layerId = ~uid, group = "data", label = lapply(lab_of(g$name, g$value), HTML),
                            highlightOptions = highlightOptions(weight = 2.5, color = BRAND$ink, bringToFront = TRUE))
    }
    if (isTRUE(input$show_fac))
      m <- m |> addCircleMarkers(data = FAC_PTS, lng = ~lon, lat = ~lat, radius = 2.5, stroke = FALSE,
                                 fillColor = BRAND$ink, fillOpacity = .6, group = "data", label = ~name)
    m |> addLegend("bottomright", pal = pal, values = v$s$value, opacity = .9, na.label = "No data",
                   title = HTML(sprintf("%s<br><span style='font-weight:400'>%s</span>", htmlEscape(ind_label(cd)), unit_label(cd))))
  })
  observeEvent(input$map_shape_click, selected(input$map_shape_click$id))
  observeEvent(input$map_marker_click, selected(input$map_marker_click$id))
  observeEvent(event_data("plotly_click", source = ns("rank")), {
    e <- event_data("plotly_click", source = ns("rank")); if (!is.null(e$customdata)) selected(e$customdata)
  })

  output$rank <- renderPlotly({
    v <- vals(); s <- v$s[!is.na(value)]
    if (!nrow(s)) return(empty_plot())
    dirn <- IND[code == input$ind, direction]
    s <- s[order(if (dirn == "low") -value else value)]
    if (nrow(s) > 40) s <- rbind(head(s, 20), tail(s, 20))
    col <- theme_col(IND[code == input$ind, theme])
    plot_ly(s, x = ~value, y = ~factor(name, levels = name), type = "bar", orientation = "h", source = ns("rank"),
            customdata = ~uid, marker = list(color = col, line = list(color = "white", width = 1)),
            hovertemplate = paste0("%{y}<br>", ind_label(input$ind), ": %{x:,.1f}<extra></extra>")) |>
      plotly_base(legend = FALSE) |>
      layout(yaxis = list(title = "", tickfont = list(size = 10)), xaxis = list(title = unit_label(input$ind)),
             shapes = if (!is.na(v$ref)) list(list(type = "line", x0 = v$ref, x1 = v$ref, y0 = 0, y1 = 1, yref = "paper",
                                                   line = list(color = BRAND$maroon, dash = "dash", width = 1.5))),
             annotations = if (!is.na(v$ref)) list(list(x = v$ref, y = 1.02, yref = "paper", text = "Busoga", showarrow = FALSE,
                                                        font = list(color = BRAND$maroon, size = 10))),
             margin = list(t = 18)) |>
      event_register("plotly_click")
  })
  output$trend <- renderPlotly({
    u <- selected(); req(u)
    lvl <- if (u == META$region_uid) "region" else ou_level[[u]]
    s <- summarise_ind(input$ind, lvl, MONTH_MIN, MONTH_MAX, uids = u, by = "quarter")
    r <- summarise_ind(input$ind, "region", MONTH_MIN, MONTH_MAX, by = "quarter")
    if (!nrow(s)) return(empty_plot())
    col <- theme_col(IND[code == input$ind, theme])
    plot_ly() |>
      add_lines(data = r, x = ~bucket_date(bucket, "quarter"), y = ~value, name = "Busoga",
                line = list(color = BRAND$muted, width = 2, dash = "dot"), hovertemplate = "Busoga %{x|%b %Y}: %{y:,.1f}<extra></extra>") |>
      add_lines(data = s, x = ~bucket_date(bucket, "quarter"), y = ~value, name = ou_name[[u]],
                line = list(color = col, width = 2.5), hovertemplate = paste0(ou_name[[u]], " %{x|%b %Y}: %{y:,.1f}<extra></extra>")) |>
      plotly_base(ytitle = unit_label(input$ind)) |>
      layout(title = list(text = sprintf("<b>%s</b>, quarterly", htmlEscape(ou_name[[u]])), font = list(size = 12), x = 0))
  })
})
