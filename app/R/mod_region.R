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
              "Open data about the region: every census since 1980 and where the population may be heading, how Busoga compares with Uganda's other regions, how close people live to services, flood and drought exposure, food prices and farming, and where commerce happens.", key = "region"),
    navset_card_underline(full_screen = TRUE,
      nav_panel("Census history & projections",
        layout_columns(col_widths = c(8, 4),
          div(plotlyOutput(ns("cen_trend"), height = 470)),
          div(class = "pj-controls",
              div(class = "pj-h", fontawesome::fa("sliders", fill = "#201B6D", height = ".95em"), " Try your own assumptions"),
              selectInput(ns("pj_to"), "Project to", c(2030, 2035, 2040, 2045, 2050), selected = 2040, width = "100%"),
              sliderInput(ns("pj_r0"), "Growth rate at the 2024 census (% a year)", min = 0.5, max = 3.5, value = round(proj_defaults()$r0, 2), step = 0.05, width = "100%"),
              sliderInput(ns("pj_slope"), "Past pace of slowing (points a year)", min = 0, max = 0.12, value = round(-proj_defaults()$slope, 3), step = 0.005, width = "100%"),
              div(class = "pj-note", "Defaults come from the censuses: the 2014-24 growth rate and the fall in the growth rate since the 1990s. The central line slows at half this pace, the low line at the full pace, the high line not at all."),
              actionLink(ns("pj_reset"), "Back to the census-based defaults"))),
        layout_columns(col_widths = c(6, 6),
          card(class = "xai-card", card_header("Projected population", span(class = "sub", "1 July of each year; children and households use the 2024 census shares")), tableOutput(ns("pj_table"))),
          card(class = "xai-card", card_header(textOutput(ns("pj_dist_title"), inline = TRUE), span(class = "sub", "census counts and the central projection")), plotlyOutput(ns("cen_dist"), height = 420))),
        card(class = "xai-card pa-card", card_header(div(fontawesome::fa("clipboard-list", fill = "#201B6D", height = ".95em"), " Assumptions behind the projections")), uiOutput(ns("pj_assump"))),
        info_note("Census counts: UBOS 2002 Census Analytical Report (1980 by district), UBOS National Population and Housing Census 2014 Main Report (1991, 2002 and 2014 on 2014 boundaries) and NPHC 2024 Final Report. Growth rates are calculated here from the census-night dates. ",
                  "Reference line: Ministry of Health 107a projections used as denominators in DHIS2.")),
      nav_panel("Busoga among Uganda's regions",
        uiOutput(ns("rg_facts")),
        layout_columns(col_widths = c(7, 5),
          card(class = "xai-card", card_header(selectInput(ns("rg_measure"), NULL, width = "100%", c(
                 "Population, 2024" = "pop", "Share of Uganda's population (%)" = "share", "Households" = "households", "Average household size" = "hh_size",
                 "Crude birth rate (births per 1,000 people)" = "cbr", "Infant mortality (per 1,000 live births)" = "imr", "Under-five mortality (per 1,000 live births)" = "u5mr",
                 "Net migration (% of population)" = "net_migration_pct", "People with a birth certificate (%)" = "birth_cert_pct", "Households owning a mosquito net (%)" = "net_pct"))),
               plotlyOutput(ns("rg_bar"), height = 520)),
          card(class = "xai-card", card_header("Growth since 1948: Busoga and Uganda", span(class = "sub", "average annual growth between censuses")), plotlyOutput(ns("rg_growth"), height = 520))),
        info_note("UBOS National Population and Housing Census 2024 Final Report: Tables 2.1 (population by sub-region), 2.6 (households), 3.4 (birth registration), 6.1 (mosquito nets), 7.1 (crude birth rate), 8.2 (childhood mortality; Bukedi, Karamoja and Madi withheld by UBOS pending investigation) and 9.1 (migration). Uganda growth rates: Table 2.2.")),
      nav_panel("Access to services",
        uiOutput(ns("acc_tiles")),
        layout_columns(col_widths = c(7, 5),
          card(full_screen = TRUE, class = "xai-card",
               card_header("How far is the nearest hospital or HC IV?", span(class = "sub", "straight-line distance from the centre of each sub-county; circles = 5 km around every HC III, HC IV and hospital")),
               leafletOutput(ns("acc_map"), height = 540)),
          div(card(class = "xai-card", card_header("Health facilities per 10,000 people", span(class = "sub", "by level; DHIS2 register and census 2024")), plotlyOutput(ns("acc_dens"), height = 300)),
              card(class = "xai-card", card_header("Rural people near an all-season road", span(class = "sub", "Rural Access Index: % of rural people within 2 km (HeiGIT)")), plotlyOutput(ns("acc_rai"), height = 250)))),
        card(class = "xai-card", card_header("Sub-counties furthest from a hospital or HC IV", span(class = "sub", "where outreach and referral transport matter most")), plotlyOutput(ns("acc_far"), height = 360)),
        info_note("Facilities and levels: DHIS2 organisation-unit register (facilities with map coordinates). Distances are straight lines from the centre of each sub-county to the nearest HC IV or hospital, so real travel distances are longer. ",
                  "Population: UBOS census 2024 (districts) and the Ministry of Health 107a figures (sub-counties). Rural Access Index: HeiGIT (2026), CC BY-SA.")),
      nav_panel("Floods & vulnerability",
        div(class = "fl-pick", radioButtons(ns("rp"), "Flood size", inline = TRUE, selected = "RP100",
                                            c("1-in-10-year" = "RP10", "1-in-50-year" = "RP50", "1-in-100-year" = "RP100", "1-in-500-year" = "RP500"))),
        uiOutput(ns("fl_tiles")),
        layout_columns(col_widths = c(7, 5),
          card(class = "xai-card", card_header("Who lives where the water would reach 30 cm or more", span(class = "sub", "people in the flood zone, by district")), plotlyOutput(ns("flood"), height = 420)),
          card(class = "xai-card", card_header("Time to reach safety", span(class = "sub", "average and longest walk out of the flood zone, minutes")), plotlyOutput(ns("fl_evac"), height = 420))),
        card(class = "xai-card", card_header("Who is most vulnerable", span(class = "sub", "bubble size = population; dependency ratio = children and older people per 100 of working age")), plotlyOutput(ns("vul"), height = 420)),
        info_note("HeiGIT flood exposure and risk indicators (2026, CC BY-SA): people, cropland, schools and health facilities in areas a flood of each size would cover to 30 cm or more, evacuation times on foot, and vulnerability (children, older people, women of reproductive age, rural share). Busoga lies along Lake Victoria, the Nile and Lake Kyoga, so flood exposure is concentrated near the shores.")),
      nav_panel("Agriculture & food",
        layout_columns(col_widths = c(7, 5),
          div(selectInput(ns("commodity"), "Food price", NULL, width = "260px"), plotlyOutput(ns("prices"), height = 400)),
          plotlyOutput(ns("seasons"), height = 460)),
        layout_columns(col_widths = c(6, 6), plotlyOutput(ns("crop_flood"), height = 320), plotlyOutput(ns("land"), height = 320)),
        info_note("Food prices: WFP / FAO / UBOS market monitoring for Iganga and Jinja markets (CC BY-IGO). Rainfall seasons: CHIRPS 1991-2020 normal against the latest year. Cropland exposed to flooding: HeiGIT 2026. Forest reserves, rangeland and water: UBOS land-use layer (2006).")),
      nav_panel("Commerce & markets",
        uiOutput(ns("com_tiles")),
        layout_columns(col_widths = c(7, 5),
          card(full_screen = TRUE, class = "xai-card", card_header("Where business happens", span(class = "sub", "zoom in to separate the clusters; switch layers on and off at the top right")),
               leafletOutput(ns("com_map"), height = 540)),
          div(card(class = "xai-card", card_header("Money services per 100,000 people", span(class = "sub", "banks, microfinance, ATMs and mobile money; census 2024")), plotlyOutput(ns("com_fin"), height = 280)),
              card(class = "xai-card", card_header("Places mapped, by district", span(class = "sub", "OpenStreetMap")), plotlyOutput(ns("com_bar"), height = 260)))),
        info_note("Banks, ATMs, mobile money, fuel stations, markets, agro-input shops and supermarkets mapped in OpenStreetMap (ODbL). OSM coverage is incomplete, especially for small businesses in rural areas, so counts are a minimum and are best read as where mapped commerce is concentrated."))),
    div(class = "footer-note", "Open data used under their licences: UBOS, OCHA HDX, HeiGIT (CC BY-SA), WFP (CC BY-IGO), CHIRPS, OpenStreetMap (ODbL).")
  )
}

region_server <- function(id) moduleServer(id, function(input, output, session) {
  pj <- reactive(list(to = as.integer(input$pj_to %||% 2040), r0 = input$pj_r0 %||% proj_defaults()$r0, slope = -(input$pj_slope %||% -proj_defaults()$slope)))
  observeEvent(input$pj_reset, { updateSliderInput(session, "pj_r0", value = round(proj_defaults()$r0, 2)); updateSliderInput(session, "pj_slope", value = round(-proj_defaults()$slope, 3)) })
  output$cen_trend <- renderPlotly({
    req(CH); x <- pj()
    pop_history_plot(x$to, x$r0, x$slope) |>
      layout(title = list(text = "<b>Busoga: every census since 1980, and where the population may be heading</b>", x = 0, font = list(size = 13)), margin = list(t = 40))
  })
  output$pj_table <- renderTable({
    req(CH); x <- pj(); pr <- busoga_projection(x$to, x$r0, x$slope)
    yrs <- intersect(c(2025, 2030, 2035, 2040, 2045, 2050), 2025:x$to)
    w <- dcast(pr[year %in% yrs], year ~ scenario, value.var = "pop")
    c24 <- function(t, c) story_cen(META$region_uid, t, c)
    u5 <- c24("Age Groups", "Age 0-4") / c24("Population by Sex", "Total"); u18 <- c24("Age Groups", "Age 0-17") / c24("Population by Sex", "Total")
    hs <- c24("Household Size", "Average Household Size")
    cen <- w[[names(PROJ_COLS)[2]]]
    f <- function(v) format(round(v, -3), big.mark = ",")
    data.table(Year = as.character(w$year), Low = f(w[[names(PROJ_COLS)[3]]]), Central = f(cen), High = f(w[[names(PROJ_COLS)[1]]]),
               `Under 5 (central)` = f(cen * u5), `Under 18 (central)` = f(cen * u18), `Households (central)` = f(cen / hs))
  }, striped = TRUE, spacing = "s", width = "100%", align = "lrrrrrr")
  output$pj_assump <- renderUI({ x <- pj(); proj_assumptions(x$r0, x$slope) })
  output$pj_dist_title <- renderText(sprintf("Districts and cities: 1991 to %s", pj()$to))
  output$cen_dist <- renderPlotly({
    req(CH); x <- pj()
    h <- CH[level == "district"]
    # before the 2018 and 2020 splits, Bugweri was part of Iganga and Jinja City part of Jinja
    h[, grp := fifelse(area == "Bugweri", "Iganga", fifelse(area == "Jinja City", "Jinja", area))]
    hh <- h[, .(pop = sum(pop)), by = .(grp, year)]
    dp <- district_projection(x$to, x$r0, x$slope)[, grp := fifelse(area == "Bugweri", "Iganga", fifelse(area == "Jinja City", "Jinja", area))][, .(pop = sum(proj)), by = grp][, year := x$to]
    all <- rbind(hh, dp)[, lab := fifelse(year == x$to, paste(year, "(projected)"), as.character(year))]
    ord <- hh[year == 2024][order(pop), grp]
    labs <- unique(all[order(year), lab])
    plot_ly(all, y = ~factor(grp, levels = ord), x = ~pop, color = ~factor(lab, levels = labs), type = "bar", orientation = "h",
            colors = c("#d7d3e8", "#aaa2d4", "#6f64b6", "#201B6D", "#B0306A")[seq_along(labs)],
            hovertemplate = "%{y}, %{fullData.name}: %{x:,.0f}<extra></extra>") |>
      plotly_base() |> layout(barmode = "group", bargap = .25, yaxis = list(title = ""), xaxis = list(title = "People", tickformat = ",.0f"),
                              legend = list(orientation = "h", y = -0.12), margin = list(t = 10),
                              annotations = list(list(x = 1, y = 1.04, xref = "paper", yref = "paper", xanchor = "right", showarrow = FALSE,
                                                      text = "Iganga includes Bugweri and Jinja includes Jinja City, for a like-for-like trend", font = list(size = 10, color = "#8a8799"))))
  })
  # ---- Busoga among Uganda's sub-regions --------------------------------------------------------
  output$rg_facts <- renderUI({
    req(SUBREG); s <- SUBREG[subregion != "Uganda"][order(-pop)]; b <- SUBREG[subregion == "Busoga"]; u <- SUBREG[subregion == "Uganda"]
    rk <- function(v, hi = TRUE) { o <- if (hi) order(-s[[v]]) else order(s[[v]]); which(s$subregion[o] == "Busoga") }
    g <- census_rates("Busoga"); gu <- census_rates("Uganda")
    tile <- function(v, l, sub) div(class = "st-num", div(class = "st-num-v", v), div(class = "st-num-l", l), div(class = "st-num-s", sub))
    div(class = "st-nums",
        tile(c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th")[rk("pop")], "largest of Uganda's 17 sub-regions",
             sprintf("%s people; only %s %s larger", format(b$pop, big.mark = ","), paste(s[pop > b$pop, subregion], collapse = " and "), if (sum(s$pop > b$pop) == 1) "is" else "are")),
        tile(sprintf("%.1f%%", 100 * b$pop / u$pop), "of Uganda's population", "about one Ugandan in ten"),
        tile(sprintf("%.1f%%", tail(g$rate, 1)), "growth a year, 2014-24", sprintf("Uganda %.1f%%", tail(gu$rate, 1))),
        tile(sprintf("%.0f", b$u5mr), "under-five deaths per 1,000 births", sprintf("Uganda %.0f", u$u5mr)),
        tile(sprintf("%.1f", b$cbr), "births per 1,000 people a year", sprintf("Uganda %.1f", u$cbr)),
        tile(sprintf("%+.1f%%", b$net_migration_pct), "net migration", sprintf("%s more people moved out than in", format(-b$net_migration, big.mark = ","))))
  })
  output$rg_bar <- renderPlotly({
    req(SUBREG); m <- input$rg_measure %||% "pop"
    s <- copy(SUBREG); s[, share := 100 * pop / s[subregion == "Uganda", pop]]
    u <- s[subregion == "Uganda", get(m)]; s <- s[subregion != "Uganda" & is.finite(get(m))]
    s[, v := get(m)]; setorder(s, v)
    lab <- names(which(c("Population, 2024" = "pop", "Share of Uganda's population (%)" = "share", "Households" = "households", "Average household size" = "hh_size",
                         "Crude birth rate (births per 1,000 people)" = "cbr", "Infant mortality (per 1,000 live births)" = "imr", "Under-five mortality (per 1,000 live births)" = "u5mr",
                         "Net migration (% of population)" = "net_migration_pct", "People with a birth certificate (%)" = "birth_cert_pct", "Households owning a mosquito net (%)" = "net_pct") == m))
    s[, col := fifelse(subregion == "Busoga", "#B0306A", "#b9b4d6")]
    fmt <- if (m %in% c("pop", "households")) function(v) format(round(v), big.mark = ",") else function(v) sprintf("%.1f", v)
    plot_ly(s, y = ~factor(subregion, levels = subregion), x = ~v, type = "bar", orientation = "h", marker = list(color = ~col),
            text = fmt(s$v), textposition = "outside", cliponaxis = FALSE, hovertemplate = "%{y}: %{text}<extra></extra>") |>
      plotly_base(xtitle = lab, legend = FALSE) |>
      layout(yaxis = list(title = "", tickfont = list(size = 11)), margin = list(l = 10, r = 50, t = 10, b = 10),
             shapes = if (length(u) && is.finite(u) && !m %in% c("pop", "households", "share")) list(list(type = "line", x0 = u, x1 = u, y0 = 0, y1 = 1, yref = "paper", line = list(color = BRAND$ink2, dash = "dash"))),
             annotations = if (length(u) && is.finite(u) && !m %in% c("pop", "households", "share")) list(list(x = u, y = 1.02, yref = "paper", text = sprintf("Uganda %s", fmt(u)), showarrow = FALSE, font = list(size = 10))))
  })
  output$rg_growth <- renderPlotly({
    req(CH); b <- census_rates("Busoga"); u <- census_rates("Uganda")
    plot_ly() |>
      add_trace(data = u, x = ~mid, y = ~rate, type = "scatter", mode = "lines+markers", name = "Uganda", line = list(color = "#9b9a94", width = 2), marker = list(size = 8, color = "#9b9a94"),
                text = ~sprintf("%d-%d", from, to), hovertemplate = "Uganda %{text}: %{y:.2f}% a year<extra></extra>") |>
      add_trace(data = b, x = ~mid, y = ~rate, type = "scatter", mode = "lines+markers", name = "Busoga", line = list(color = "#B0306A", width = 3), marker = list(size = 10, color = "#B0306A"),
                text = ~sprintf("%d-%d", from, to), hovertemplate = "Busoga %{text}: %{y:.2f}% a year<extra></extra>") |>
      plotly_base(ytitle = "Average annual growth (%)", xtitle = "Mid-point of the period between censuses") |>
      layout(yaxis = list(ticksuffix = "%", rangemode = "tozero"), legend = list(orientation = "h", y = 1.08))
  })
  # ---- access: nearest HC IV / hospital, facility density, road access ----------------------------
  acc <- local({
    f <- OU[level_name == "facility" & !is.na(lat) & !is.na(lon)]
    big <- f[grp_level %in% c("HC IV", "General Hospital", "RRH")]
    g <- GEO$subcounty
    cen <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(g))))
    hav <- function(lon1, lat1, lon2, lat2) { r <- pi / 180; a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2; 12742 * asin(sqrt(a)) }
    near <- vapply(seq_len(nrow(cen)), function(i) { d <- hav(cen[i, 1], cen[i, 2], big$lon, big$lat); c(min(d), which.min(d)) }, c(0, 0))
    d <- data.table(uid = g$uid, name = g$name, lon = cen[, 1], lat = cen[, 2], km = near[1, ], nearest = big$name[near[2, ]])
    d[, district := OU$district[match(uid, OU$uid)]]
    d[POP[level == "subcounty" & year == max(POP$year)], on = "uid", pop := i.pop]
    list(d = d, f = f, big = big)
  })
  output$acc_tiles <- renderUI({
    d <- acc$d; f <- OU[level_name == "facility"]; pop <- story_cen(META$region_uid, "Population by Sex", "Total")
    tile <- function(icon, v, l, sub) div(class = "st-num", span(class = "st-num-icon", fontawesome::fa(icon, fill = "#fff", height = "1.1em")),
                                          div(class = "st-num-v", v), div(class = "st-num-l", l), div(class = "st-num-s", sub))
    far <- d[km > 15]
    div(class = "st-nums",
        tile("hospital", sprintf("%.1f", 1e4 * nrow(f) / pop), "health facilities per 10,000 people", sprintf("%s facilities for %s people", format(nrow(f), big.mark = ","), format(pop, big.mark = ","))),
        tile("truck-medical", format(round(pop / nrow(acc$big)), big.mark = ","), "people per hospital or HC IV", sprintf("%d hospitals and HC IVs", nrow(acc$big))),
        tile("ruler", sprintf("%.1f km", median(d$km)), "typical distance to a hospital or HC IV", "median over sub-counties, straight line"),
        tile("person-walking", sprintf("%d", nrow(far)), "sub-counties more than 15 km away", sprintf("home to about %s people", format(round(sum(far$pop, na.rm = TRUE), -3), big.mark = ","))),
        tile("road", if (!is.null(HEI)) sprintf("%.0f%%", weighted.mean(HEI$coping$RAI_total_pop, HEI$vulnerability$total_pop[match(HEI$coping$district, HEI$vulnerability$district)])) else "–",
             "of rural people live near an all-season road", "Rural Access Index, HeiGIT"))
  })
  output$acc_map <- renderLeaflet({
    d <- acc$d; g <- merge(GEO$subcounty[, "uid"], d[, .(uid, name, km, nearest, district)], by = "uid")
    pal <- colorBin(c("#e0f3db", "#a8ddb5", "#fdd49e", "#fc8d59", "#b30000"), g$km, bins = c(0, 5, 10, 15, 25, 60), na.color = "#ddd")
    lv_col <- c("RRH" = "#75002C", "General Hospital" = "#A3214A", "HC IV" = "#C0582B", "HC III" = "#3949AB", "HC II" = "#8E9BD8", "Clinic" = "#9b9a94")
    f <- acc$f[grp_level %in% names(lv_col)]
    m <- base_map() |>
      addPolygons(data = g, fillColor = ~pal(km), fillOpacity = .78, color = "white", weight = .6, group = "Distance",
                  label = lapply(sprintf("<b>%s</b> <span style='color:#888'>%s</span><br>%.1f km to %s", htmlEscape(g$name), htmlEscape(g$district), g$km, htmlEscape(g$nearest)), HTML)) |>
      addPolylines(data = GEO$district, color = BRAND$navy, weight = 1.6) |>
      addCircles(data = f[grp_level %in% c("HC III", "HC IV", "General Hospital", "RRH")], lng = ~lon, lat = ~lat, radius = 5000, stroke = FALSE,
                 fillColor = "#3949AB", fillOpacity = .08, group = "5 km around HC III and above")
    for (lv in names(lv_col)) { z <- f[grp_level == lv]; if (!nrow(z)) next
      m <- m |> addCircleMarkers(data = z, lng = ~lon, lat = ~lat, radius = c(RRH = 8, `General Hospital` = 7, `HC IV` = 6, `HC III` = 4.5, `HC II` = 3, Clinic = 3)[[lv]],
                                 stroke = TRUE, color = "white", weight = 1, fillColor = lv_col[[lv]], fillOpacity = .95, group = lv, label = ~sprintf("%s (%s)", name, lv)) }
    m |> addLayersControl(overlayGroups = c("Distance", "5 km around HC III and above", names(lv_col)), options = layersControlOptions(collapsed = TRUE)) |>
      hideGroup(c("HC II", "Clinic")) |>
      addLegend("bottomright", pal = pal, values = g$km, title = "km to nearest<br>HC IV or hospital", opacity = .9) |>
      addLegend("bottomleft", colors = unname(lv_col), labels = names(lv_col), title = "Facility level", opacity = 1)
  })
  output$acc_dens <- renderPlotly({
    f <- OU[level_name == "facility" & grp_level %in% c("HC II", "HC III", "HC IV", "General Hospital", "RRH", "Clinic")]
    f[, lv := fifelse(grp_level %in% c("General Hospital", "RRH"), "Hospital", grp_level)]
    x <- f[, .N, by = .(uid = uid_l3, lv)]
    x[CTX$census[level == "district" & table == "Population by Sex" & column == "Total", .(uid, pop = value)], on = "uid", pop := i.pop]
    x <- x[!is.na(pop)][, v := 1e4 * N / pop][, name := sub(" District$", "", ou_name[uid])]
    ord <- x[, sum(v), by = name][order(V1), name]
    cols <- c(Hospital = "#75002C", `HC IV` = "#C0582B", `HC III` = "#3949AB", `HC II` = "#8E9BD8", Clinic = "#c9c6d6")
    p <- plot_ly()
    for (k in names(cols)) { z <- x[lv == k]; if (nrow(z)) p <- p |> add_bars(data = z, y = ~factor(name, levels = ord), x = ~v, name = k, orientation = "h", marker = list(color = cols[[k]]),
                                                                              customdata = ~N, hovertemplate = paste0("%{y}: %{customdata} ", k, " (%{x:.2f} per 10,000)<extra></extra>")) }
    p |> plotly_base() |> layout(barmode = "stack", xaxis = list(title = "per 10,000 people"), yaxis = list(title = ""), legend = list(orientation = "h", y = 1.12), margin = list(t = 10))
  })
  output$acc_rai <- renderPlotly({
    req(HEI); x <- HEI$coping[, .(district, v = RAI_total_pop)][order(v)]
    plot_ly(x, y = ~factor(district, levels = district), x = ~v, type = "bar", orientation = "h", marker = list(color = "#00897B"),
            text = ~sprintf("%.0f%%", v), textposition = "outside", cliponaxis = FALSE, hovertemplate = "%{y}: %{x:.1f}%<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(xaxis = list(ticksuffix = "%", range = c(0, 100), title = ""), yaxis = list(title = "", tickfont = list(size = 10.5)), margin = list(r = 30, t = 5))
  })
  output$acc_far <- renderPlotly({
    x <- acc$d[order(-km)][1:20][order(km)][, name := make.unique(paste0(name, " (", district, ")"), sep = " ")]
    plot_ly(x, y = ~factor(name, levels = name), x = ~km, type = "bar", orientation = "h", marker = list(color = ~km, colorscale = list(c(0, "#fdd49e"), c(1, "#b30000"))),
            text = ~sprintf("%.0f km · %s", km, nearest), textposition = "outside", cliponaxis = FALSE, customdata = ~district,
            hovertemplate = "%{y} (%{customdata}): %{text}<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(xaxis = list(title = "km (straight line)", range = c(0, max(x$km) * 1.6)), yaxis = list(title = "", tickfont = list(size = 10.5)), margin = list(r = 10, t = 5))
  })
  # ---- floods ---------------------------------------------------------------------------------------
  fl <- reactive({ req(HEI); rp <- input$rp %||% "RP100"; f <- HEI$flood; g <- function(v) f[[paste0(rp, "_", v)]]
    data.table(district = f$district, women = g("female_pop_30cm"), u5 = g("children_u5_30cm"), elderly = g("elderly_30cm"), u15 = g("pop_u15_30cm"),
               wra = g("wra_pop_30cm"), schools = g("education_30cm_count"), health = g("primary_healthcare_30cm_count") + g("hospitals_30cm_count"), crops = g("crops_30cm_km2")) })
  output$fl_tiles <- renderUI({
    x <- fl(); rp <- sub("RP", "", input$rp %||% "RP100")
    tile <- function(icon, v, l, sub) div(class = "st-num", span(class = "st-num-icon", style = "background:linear-gradient(135deg,#0b4f8a,#3987e5)", fontawesome::fa(icon, fill = "#fff", height = "1.1em")),
                                          div(class = "st-num-v", v), div(class = "st-num-l", l), div(class = "st-num-s", sub))
    ev <- if (!is.null(HEI$coping)) HEI$coping[[sprintf("RP%s_evac_time_minutes_max", rp)]] else NA
    div(class = "st-nums",
        tile("person-dress", format(sum(x$women, na.rm = TRUE), big.mark = ","), "women and girls in the flood zone", sprintf("a 1-in-%s-year flood", rp)),
        tile("baby", format(sum(x$u5, na.rm = TRUE), big.mark = ","), "children under five", "who would need to be moved first"),
        tile("person-cane", format(sum(x$elderly, na.rm = TRUE), big.mark = ","), "older people (65+)", "often the slowest to evacuate"),
        tile("school", sum(x$schools, na.rm = TRUE), "schools in the flood zone", "possible shelters are out of use"),
        tile("house-medical", sum(x$health, na.rm = TRUE), "health facilities in the flood zone", "services at risk during floods"),
        tile("stopwatch", if (length(ev) && any(is.finite(ev))) sprintf("%.0f min", max(ev, na.rm = TRUE)) else "–", "longest walk to safety", sprintf("worst district: %s", HEI$coping$district[which.max(ev)])))
  })
  output$flood <- renderPlotly({
    x <- fl()[order(women + u5)]
    plot_ly(x, y = ~factor(district, levels = district)) |>
      add_bars(x = ~women, name = "Women and girls", orientation = "h", marker = list(color = "#B0306A"), hovertemplate = "%{y}: %{x:,.0f} women and girls<extra></extra>") |>
      add_bars(x = ~u5, name = "Children under 5", orientation = "h", marker = list(color = "#F4A259"), hovertemplate = "%{y}: %{x:,.0f} children under 5<extra></extra>") |>
      add_bars(x = ~elderly, name = "Aged 65+", orientation = "h", marker = list(color = "#5C6BC0"), hovertemplate = "%{y}: %{x:,.0f} aged 65+<extra></extra>") |>
      plotly_base() |> layout(barmode = "group", xaxis = list(title = "People in the flood zone", tickformat = ",.0f"), yaxis = list(title = ""), legend = list(orientation = "h", y = 1.08))
  })
  output$fl_evac <- renderPlotly({
    req(HEI); rp <- input$rp %||% "RP100"; c <- HEI$coping
    x <- data.table(district = c$district, mean = c[[paste0(rp, "_evac_time_minutes_mean")]], max = c[[paste0(rp, "_evac_time_minutes_max")]])[order(mean)]
    plot_ly(x, y = ~factor(district, levels = district)) |>
      add_segments(x = ~mean, xend = ~max, yend = ~factor(district, levels = district), line = list(color = "#c9d8ef", width = 6), showlegend = FALSE, hoverinfo = "skip") |>
      add_markers(x = ~mean, name = "Average", marker = list(color = "#0b4f8a", size = 11), hovertemplate = "%{y}: average %{x:.0f} min<extra></extra>") |>
      add_markers(x = ~max, name = "Longest", marker = list(color = "#b30000", size = 9, symbol = "diamond"), hovertemplate = "%{y}: longest %{x:.0f} min<extra></extra>") |>
      plotly_base() |> layout(xaxis = list(title = "minutes on foot", rangemode = "tozero"), yaxis = list(title = ""), legend = list(orientation = "h", y = 1.08))
  })
  output$vul <- renderPlotly({
    req(HEI); v <- HEI$vulnerability
    v <- copy(v)[, msize := 18 + 42 * sqrt(total_pop / max(total_pop))]
    plot_ly(v, x = ~dependency_ratio, y = ~rural_pop_perc, type = "scatter", mode = "markers+text", text = ~district, textposition = "top center",
            textfont = list(size = 12, color = "#2b2a36"), marker = list(size = ~msize, sizemode = "diameter", color = "#B0306A", opacity = .6, line = list(color = "#fff", width = 1)),
            customdata = ~sprintf("%s people, %s children under 5", format(total_pop, big.mark = ","), format(children_u5, big.mark = ",")),
            hovertemplate = "<b>%{text}</b><br>dependency ratio %{x:.0f}<br>%{y:.1f}% rural<br>%{customdata}<extra></extra>") |>
      plotly_base(xtitle = "Dependency ratio (children and older people per 100 of working age)", ytitle = "% of people in rural areas", legend = FALSE) |>
      layout(yaxis = list(range = c(0, max(v$rural_pop_perc) * 1.25)), xaxis = list(range = c(min(v$dependency_ratio) - 4, max(v$dependency_ratio) + 4)))
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
    req(exists("CLIM") && !is.null(CLIM)); x <- CLIM[level == "region"][order(period)]
    x[, `:=`(y = period %/% 100L, m = period %% 100L)]
    # the 1991-2020 normal, recovered from each month's rainfall and its anomaly (% of normal)
    nrm <- x[is.finite(rain_anom) & rain_anom > -100, .(normal = median(rain_mm / (1 + rain_anom / 100))), by = m][order(m)]
    full <- x[, .N, by = y][N == 12, y]; last <- max(x$y); cur <- x[y == last][order(m)]; prev <- x[y == max(full[full < last], last - 1)][order(m)]
    seas <- list(list(x0 = 2.5, x1 = 5.5, t = "First rains (Mar-May)"), list(x0 = 8.5, x1 = 11.5, t = "Second rains (Sep-Nov)"))
    plot_ly() |>
      add_bars(data = nrm, x = ~m, y = ~normal, name = "1991-2020 normal", marker = list(color = "#d9d7ee"),
               hovertemplate = paste0("%{customdata} normal: %{y:.0f} mm<extra></extra>"), customdata = month.name[nrm$m]) |>
      add_trace(data = prev, x = ~m, y = ~rain_mm, type = "scatter", mode = "lines+markers", name = as.character(unique(prev$y)),
                line = list(color = "#9b9a94", width = 2), marker = list(size = 6, color = "#9b9a94"), customdata = month.name[prev$m],
                hovertemplate = paste0("%{customdata} ", unique(prev$y), ": %{y:.0f} mm<extra></extra>")) |>
      add_trace(data = cur, x = ~m, y = ~rain_mm, type = "scatter", mode = "lines+markers", name = sprintf("%d (to %s)", last, month.abb[max(cur$m)]),
                line = list(color = BRAND$navy, width = 3), marker = list(size = 8, color = BRAND$navy), customdata = month.name[cur$m],
                hovertemplate = paste0("%{customdata} ", last, ": %{y:.0f} mm<extra></extra>")) |>
      plotly_base(ytitle = "mm per month") |>
      layout(xaxis = list(tickmode = "array", tickvals = 1:12, ticktext = month.abb, range = c(0.4, 12.6), title = ""),
             title = list(text = "<b>Farming seasons: Busoga rainfall against the long-term normal</b>", x = 0, font = list(size = 13)),
             shapes = lapply(seas, function(z) list(type = "rect", x0 = z$x0, x1 = z$x1, y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(46,125,50,.07)", line = list(width = 0), layer = "below")),
             annotations = lapply(seas, function(z) list(x = (z$x0 + z$x1) / 2, y = 1.02, yref = "paper", text = z$t, showarrow = FALSE, font = list(size = 10, color = "#2E7D32"))),
             margin = list(t = 50), legend = list(orientation = "h", y = -0.15))
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
  output$com_tiles <- renderUI({
    req(COM); x <- COM$points[, .N, by = type]; pop <- story_cen(META$region_uid, "Population by Sex", "Total")
    ic <- c("Banks & microfinance" = "building-columns", "ATMs" = "money-bill-wave", "Mobile money & forex" = "mobile-screen-button", "Fuel stations" = "gas-pump",
            "Markets" = "store", "Agro-input shops" = "seedling", "Shops & wholesale" = "cart-shopping")
    div(class = "st-nums st-nums-7", lapply(names(COM_COL), function(k) { n <- x[type == k, N]; n <- if (length(n)) n else 0
      div(class = "st-num", span(class = "st-num-icon", style = sprintf("background:%s", COM_COL[[k]]), fontawesome::fa(ic[[k]], fill = "#fff", height = "1.1em")),
          div(class = "st-num-v", format(n, big.mark = ",")), div(class = "st-num-l", k), div(class = "st-num-s", sprintf("%.1f per 100,000 people", 1e5 * n / pop))) }))
  })
  output$com_map <- renderLeaflet({
    req(COM); m <- base_map() |> addPolylines(data = GEO$district, color = BRAND$navy, weight = 1.4)
    for (k in names(COM_COL)) { z <- COM$points[type == k]; if (!nrow(z)) next
      m <- m |> addCircleMarkers(data = z, lng = ~lon, lat = ~lat, radius = 6, color = "white", weight = 1.2, fillColor = COM_COL[[k]],
                                 fillOpacity = .92, group = k, label = ~sprintf("%s (%s)", ifelse(is.na(name), k, name), k),
                                 clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE, spiderfyOnMaxZoom = TRUE, maxClusterRadius = 40)) }
    m |> addLayersControl(overlayGroups = names(COM_COL), options = layersControlOptions(collapsed = FALSE)) |>
      addLegend("bottomright", colors = unname(COM_COL), labels = names(COM_COL), opacity = 1)
  })
  output$com_fin <- renderPlotly({
    req(COM); x <- COM$counts[level == "district" & type %in% c("Banks & microfinance", "ATMs", "Mobile money & forex"), .(N = sum(N)), by = uid]
    x[CTX$census[level == "district" & table == "Population by Sex" & column == "Total", .(uid, pop = value)], on = "uid", pop := i.pop]
    x <- x[!is.na(pop)][, `:=`(v = 1e5 * N / pop, name = sub(" District$", "", ou_name[uid]))][order(v)]
    plot_ly(x, y = ~factor(name, levels = name), x = ~v, type = "bar", orientation = "h", marker = list(color = "#3949AB"), customdata = ~N,
            text = ~sprintf("%.1f", v), textposition = "outside", cliponaxis = FALSE, hovertemplate = "%{y}: %{customdata} places, %{x:.1f} per 100,000<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(xaxis = list(title = "", range = c(0, max(x$v) * 1.2)), yaxis = list(title = "", tickfont = list(size = 10.5)), margin = list(r = 30, t = 5))
  })
  output$com_bar <- renderPlotly({
    req(COM); x <- COM$counts[level == "district"][, district := sub(" District$", "", ou_name[uid])]
    ord <- x[, sum(N), by = district][order(V1)]$district
    p <- plot_ly()
    for (k in names(COM_COL)) { z <- x[type == k]; if (!nrow(z)) next
      p <- p |> add_bars(data = z, y = ~factor(district, levels = ord), x = ~N, name = k, orientation = "h", marker = list(color = COM_COL[[k]])) }
    p |> plotly_base(legend = FALSE) |> layout(barmode = "stack", xaxis = list(title = "Places mapped"), yaxis = list(title = "", tickfont = list(size = 10.5)), margin = list(t = 5))
  })
})
