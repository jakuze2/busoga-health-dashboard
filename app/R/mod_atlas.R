# Atlas: interactive map of Busoga with every DHIS2 facility and OpenStreetMap context layers.

read_osm <- function(n) {
  f <- file.path(DATA, "geo", paste0("osm_", n, ".geojson"))
  if (file.exists(f)) tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) NULL)
}
OSM <- lapply(c(roads = "roads", water = "water", rivers = "rivers", schools = "schools",
                markets = "markets", towns = "towns", health = "health"), read_osm)
pts_xy <- function(s) if (is.null(s) || !nrow(s)) NULL else
  cbind(sf::st_drop_geometry(s), sf::st_coordinates(sf::st_geometry(s))[, 1:2, drop = FALSE])

REPORT_STATUS <- c("Regular reporter" = STATUS$good, "Intermittent" = STATUS$warning,
                   "Stopped reporting" = STATUS$serious, "Never reported" = STATUS$critical,
                   "Not expected to report" = "#9b9a94")
LEVEL_RADIUS <- function(lv) fcase(lv %in% c("RRH", "General Hospital"), 9, lv == "HC IV", 7.5,
                                   lv == "HC III", 6, lv == "HC II", 4.5, default = 4)

atlas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Atlas", "Map of Busoga: facilities and context",
              "Every health facility registered in DHIS2, coloured by reporting status and sized by level, with roads, water, schools, markets and towns from OpenStreetMap. Switch layers in the map control (top right).", key = "atlas"),
    layout_sidebar(
      sidebar = sidebar(width = 300, open = "desktop",
        selectizeInput(ns("find"), "Find a facility", choices = NULL, options = list(placeholder = "Type a name...")),
        checkboxGroupInput(ns("status"), "Reporting status", names(REPORT_STATUS), selected = names(REPORT_STATUS)),
        checkboxGroupInput(ns("flevel"), "Facility level", c("Hospital", "HC IV", "HC III", "HC II", "Clinic / other"),
                           selected = c("Hospital", "HC IV", "HC III", "HC II", "Clinic / other")),
        checkboxGroupInput(ns("own"), "Ownership", c("GOV", "PNFP", "PFP"), selected = c("GOV", "PNFP", "PFP"), inline = TRUE),
        hr(), uiOutput(ns("counts")),
        info_note(textOutput(ns("nocoord"), inline = TRUE))),
      card(full_screen = TRUE, class = "border-0",
           leafletOutput(ns("map"), height = 760),
           div(class = "map-legend-note",
               "Facilities and boundaries: DHIS2, Ministry of Health. Roads, water, schools, markets, towns, OSM health sites: © OpenStreetMap contributors (ODbL). Basemaps © Esri, OpenStreetMap contributors, OpenTopoMap."))
    ),
    card(card_header("Facilities without coordinates in DHIS2", span(class = "sub", "these cannot be mapped; updating their GPS in DHIS2 fixes this")),
         DTOutput(ns("nocoord_tbl")))
  )
}

atlas_server <- function(id) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  fac <- copy(OU[level_name == "facility"])
  fac[, flevel := fcase(grp_level %in% c("RRH", "General Hospital"), "Hospital", grp_level == "HC IV", "HC IV",
                        grp_level == "HC III", "HC III", grp_level == "HC II", "HC II", default = "Clinic / other")]
  st <- if (!is.null(DQF)) DQF[, .(uid, report_status, last_report, completeness_12m)] else
    data.table(uid = fac$uid, report_status = NA_character_, last_report = NA_integer_, completeness_12m = NA_real_)
  fac <- merge(fac, st, by = "uid", all.x = TRUE)
  fac[is.na(report_status), report_status := "Not expected to report"]
  updateSelectizeInput(session, "find", choices = setNames(fac[!is.na(lat), uid], fac[!is.na(lat), sprintf("%s (%s)", name, subcounty)]),
                       server = TRUE, selected = character())

  shown <- reactive({
    fac[!is.na(lat) & report_status %in% input$status & flevel %in% input$flevel &
          (grp_ownership %in% input$own | is.na(grp_ownership))]
  })
  popup_of <- function(x) sprintf(
    "<div style='min-width:230px'><div style='font-size:11px;color:%s;text-transform:uppercase;letter-spacing:.06em;font-weight:700'>%s · %s</div>
     <b style='font-size:14px;color:%s'>%s</b><br>%s, %s<br>%s<hr style='margin:5px 0'>
     Reporting: <b style='color:%s'>%s</b><br>Last 105:01 report: %s<br>Completeness, last 12 months: %s</div>",
    BRAND$maroon, htmlEscape(x$grp_level %||% ""), htmlEscape(x$grp_ownership %||% ""), BRAND$navy, htmlEscape(x$name),
    htmlEscape(x$subcounty), htmlEscape(x$dlg), htmlEscape(x$district),
    REPORT_STATUS[x$report_status], x$report_status,
    ifelse(is.na(x$last_report), "none", fmt_month(x$last_report)),
    ifelse(is.na(x$completeness_12m), "–", sprintf("%.0f%%", x$completeness_12m)))

  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE, zoomSnap = 0.25)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") |>
      add_grey_labels("Light", pane = "tilePane") |>
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Streets (OpenStreetMap)") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addProviderTiles(providers$OpenTopoMap, group = "Topographic") |>
      fitBounds(32.82, -0.4, 33.99, 1.48) |>
      addMapPane("bnd", zIndex = 410) |> addMapPane("ctx", zIndex = 420) |> addMapPane("fac", zIndex = 450)
    if (!is.null(OSM$water))
      m <- m |> addPolygons(data = OSM$water[sf::st_geometry_type(OSM$water) %in% c("POLYGON", "MULTIPOLYGON"), ],
                            fillColor = "#9cc3e6", fillOpacity = .75, color = "#6ea6d8", weight = .5, group = "Water bodies",
                            label = ~name, options = pathOptions(pane = "ctx"))
    if (!is.null(OSM$rivers))
      m <- m |> addPolylines(data = OSM$rivers, color = "#4a8fd3", weight = 1.4, opacity = .8, group = "Rivers",
                             label = ~name, options = pathOptions(pane = "ctx"))
    if (!is.null(OSM$roads)) {
      rw <- fcase(OSM$roads$highway == "trunk", 3.2, OSM$roads$highway == "primary", 2.6,
                  OSM$roads$highway == "secondary", 1.8, default = 1.1)
      m <- m |> addPolylines(data = OSM$roads, color = "#b5651d", weight = rw, opacity = .85, group = "Main roads",
                             label = ~paste0(ifelse(is.na(name), "", name), " (", highway, ")"), options = pathOptions(pane = "ctx"))
    }
    m <- m |>
      addPolylines(data = GEO$subcounty, color = "#7d7b74", weight = .6, opacity = .8, dashArray = "3 3",
                   group = "Sub-county boundaries", label = ~name, options = pathOptions(pane = "bnd")) |>
      addPolylines(data = GEO$dlg, color = BRAND$maroon, weight = 1.2, opacity = .8, group = "DLG boundaries",
                   options = pathOptions(pane = "bnd")) |>
      addPolylines(data = GEO$district, color = BRAND$navy, weight = 2.2, opacity = .95, group = "District boundaries",
                   label = ~name, options = pathOptions(pane = "bnd"))
    add_pts <- function(m, s, grp, col, r, lab_col = "name") {
      d <- pts_xy(s); if (is.null(d)) return(m)
      m |> addCircleMarkers(data = d, lng = ~X, lat = ~Y, radius = r, stroke = TRUE, weight = .8, color = "white",
                            fillColor = col, fillOpacity = .9, group = grp,
                            label = if (lab_col %in% names(d)) ~ifelse(is.na(get(lab_col)), grp, get(lab_col)) else grp,
                            options = pathOptions(pane = "ctx"))
    }
    m <- m |> add_pts(OSM$schools, "Schools", "#7a5cc4", 3) |>
      add_pts(OSM$markets, "Markets", "#d9822b", 4.5) |>
      add_pts(OSM$health, "Health sites (OpenStreetMap)", "#0f9d9a", 3.5)
    if (!is.null(OSM$towns) && nrow(OSM$towns)) {
      d <- pts_xy(OSM$towns)
      m <- m |> addLabelOnlyMarkers(data = d, lng = ~X, lat = ~Y, label = ~name, group = "Towns",
                                    labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                                                                style = list("font-weight" = "700", "color" = "#2b2b2b",
                                                                             "text-shadow" = "0 0 3px #fff, 0 0 3px #fff")))
    }
    over <- c("DHIS2 health facilities", "District boundaries", "DLG boundaries", "Sub-county boundaries",
              "Main roads", "Rivers", "Water bodies", "Towns", "Markets", "Schools", "Health sites (OpenStreetMap)")
    m |> addLayersControl(baseGroups = c("Light", "Streets (OpenStreetMap)", "Satellite", "Topographic"),
                          overlayGroups = over, options = layersControlOptions(collapsed = FALSE)) |>
      hideGroup(c("DLG boundaries", "Sub-county boundaries", "Schools", "Health sites (OpenStreetMap)", "Markets")) |>
      addLegend("bottomleft", colors = unname(REPORT_STATUS), labels = names(REPORT_STATUS), opacity = 1,
                title = "Facility reporting (105:01)") |>
      addScaleBar("bottomright", options = scaleBarOptions(imperial = FALSE))
  })
  observe({
    req(input$map_zoom)            # wait until the map exists (it is not drawn while its tab is hidden)
    s <- shown()
    leafletProxy(ns("map")) |> clearGroup("DHIS2 health facilities") |>
      addCircleMarkers(data = s, lng = ~lon, lat = ~lat, radius = LEVEL_RADIUS(s$grp_level), layerId = ~uid,
                       stroke = TRUE, color = "white", weight = 1.2, fillColor = unname(REPORT_STATUS[s$report_status]),
                       fillOpacity = .95, group = "DHIS2 health facilities", label = ~name,
                       popup = popup_of(s), options = pathOptions(pane = "fac"))
  })
  observeEvent(input$find, {
    req(nzchar(input$find)); x <- fac[uid == input$find]
    leafletProxy(ns("map")) |> flyTo(x$lon, x$lat, zoom = 14) |>
      addPopups(x$lon, x$lat, popup_of(x), layerId = "find")
  })
  output$counts <- renderUI({
    s <- fac[flevel %in% input$flevel]
    tb <- s[, .N, by = report_status][order(match(report_status, names(REPORT_STATUS)))]
    tagList(tags$b(sprintf("%d facilities in DHIS2", nrow(fac))),
            tags$ul(style = "padding-left:1rem;font-size:.85rem;margin:.3rem 0",
                    lapply(seq_len(nrow(tb)), function(i) tags$li(
                      span(class = "status-dot", style = sprintf("background:%s;margin-right:.35rem", REPORT_STATUS[tb$report_status[i]])),
                      sprintf("%s: %d", tb$report_status[i], tb$N[i])))),
            if (!is.null(OSM$schools)) div(class = "muted", style = "font-size:.8rem",
              sprintf("Context: %s schools, %s markets, %s mapped road segments.",
                      format(nrow(OSM$schools), big.mark = ","), format(NROW(OSM$markets), big.mark = ","),
                      format(nrow(OSM$roads), big.mark = ","))))
  })
  output$nocoord <- renderText(sprintf("%d facilities have no coordinates in DHIS2 and are not on the map (list below).",
                                       fac[is.na(lat), .N]))
  output$nocoord_tbl <- renderDT({
    x <- fac[is.na(lat), .(Facility = name, Level = grp_level, Ownership = grp_ownership, `Sub-county` = subcounty,
                           DLG = dlg, District = district, `Reporting status` = report_status)][order(District, Facility)]
    datatable(x, rownames = FALSE, extensions = "Buttons", options = list(pageLength = 10, dom = "Bfrtip", buttons = c("csv")))
  })
})
