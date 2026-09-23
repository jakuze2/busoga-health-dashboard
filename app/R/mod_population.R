# Population & education: age/sex pyramids (WorldPop, UBOS), UBOS district population, schools.

PYR  <- if (file.exists(file.path(DATA, "pyramid.rds"))) readRDS(file.path(DATA, "pyramid.rds"))
UBOS <- if (file.exists(file.path(DATA, "ubos_pop.rds"))) readRDS(file.path(DATA, "ubos_pop.rds"))
EDU  <- if (file.exists(file.path(DATA, "education.rds"))) readRDS(file.path(DATA, "education.rds"))
SCH  <- if (file.exists(file.path(DATA, "schools.rds"))) readRDS(file.path(DATA, "schools.rds"))
SCHOOL_COL <- c("Pre-primary" = "#C17D11", "Primary" = "#3949AB", "Secondary" = "#A3214A", "Technical / vocational" = "#00897B",
                "Tertiary college / institute" = "#8E44AD", "University" = "#2E7D32", "Unclassified" = "#9b9a94")
AGE_ORDER <- c("0", "1-4", sprintf("%d-%d", seq(5, 85, 5), seq(9, 89, 5)), "90+")

pyramid_plot <- function(x, title = NULL, compare = NULL, compare_name = "Comparison") {
  x <- x[, .(pop = sum(pop)), by = .(age, sex)]
  x[, share := 100 * pop / sum(pop)]
  lv <- intersect(AGE_ORDER, unique(x$age))
  p <- plot_ly() |>
    add_bars(data = x[sex == "Male"], y = ~factor(age, levels = lv), x = ~-share, name = "Male", orientation = "h",
             marker = list(color = BRAND$navy), customdata = ~pop,
             hovertemplate = "Male %{y}: %{customdata:,.0f} (%{x:.1f}%)<extra></extra>") |>
    add_bars(data = x[sex == "Female"], y = ~factor(age, levels = lv), x = ~share, name = "Female", orientation = "h",
             marker = list(color = BRAND$maroon), customdata = ~pop,
             hovertemplate = "Female %{y}: %{customdata:,.0f} (%{x:.1f}%)<extra></extra>")
  if (!is.null(compare)) {
    c2 <- compare[, .(pop = sum(pop)), by = .(age, sex)][, share := 100 * pop / sum(pop)]
    p <- p |>
      add_lines(data = c2[sex == "Male"][order(match(age, lv))], y = ~factor(age, levels = lv), x = ~-share, name = as.character(compare_name),
                line = list(color = "#0b0b0b", width = 1.6, dash = "dot", shape = "hvh"), hoverinfo = "skip") |>
      add_lines(data = c2[sex == "Female"][order(match(age, lv))], y = ~factor(age, levels = lv), x = ~share, name = as.character(compare_name),
                showlegend = FALSE, line = list(color = "#0b0b0b", width = 1.6, dash = "dot", shape = "hvh"), hoverinfo = "skip")
  }
  m <- ceiling(max(c(abs(x$share), if (!is.null(compare)) 100 * compare[, sum(pop), by = .(age, sex)]$V1 / sum(compare$pop))) + 0.5)
  p |> plotly_base() |>
    layout(barmode = "overlay", bargap = .08, yaxis = list(title = ""),
           xaxis = list(title = "% of population", range = c(-m, m), tickvals = seq(-m, m, by = max(1, round(m / 4))),
                        ticktext = abs(seq(-m, m, by = max(1, round(m / 4))))),
           title = if (!is.null(title)) list(text = title, font = list(size = 13), x = 0) else NULL, margin = list(t = if (!is.null(title)) 36 else 10))
}

population_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Population & education", "Who lives in Busoga, and where the schools are",
              "Age and sex structure for every area, official UBOS district population, and every school by level with its distance to the nearest health facility."),
    filter_bar(area_ui(ns("area"), depth = 3)),
    navset_card_underline(full_screen = TRUE,
      nav_panel("Population pyramid",
        div(class = "pyr-controls",
            selectizeInput(ns("cmp"), "Compare with", NULL, width = "320px", options = list(placeholder = "No comparison")),
            radioButtons(ns("cmp_view"), NULL, c("Overlay" = "overlay", "Side by side" = "side"), inline = TRUE),
            radioButtons(ns("pyr_year"), "Year", c("2026", "2020"), inline = TRUE)),
        uiOutput(ns("pyr_area")),
        info_note("Pick any district, DLG or sub-county above, and compare it with another area (overlay or side by side). Shares are % of each area's own population, so areas of different size compare fairly. Without a comparison, the dotted outline is the other year. Age and sex structure: WorldPop R2025A (University of Southampton), 1 km estimates summed within each DHIS2 boundary; totals scaled to the Ministry of Health 107a projection (UBOS-based), so every page shows one official total.")),
      nav_panel("UBOS district population",
        layout_columns(col_widths = c(6, 6), plotlyOutput(ns("ubos_bar"), height = 480), plotlyOutput(ns("ubos_pyr"), height = 480)),
        info_note("Uganda Bureau of Statistics subnational population statistics (2023 projections, census base), via the OCHA Humanitarian Data Exchange (CC BY-IGO). Jinja City is counted within Jinja District in this table.")),
      nav_panel("Education & schools",
        uiOutput(ns("edu_hero")),
        layout_columns(col_widths = c(6, 6), leafletOutput(ns("sch_map"), height = 520), plotlyOutput(ns("sch_bar"), height = 520)),
        reactableOutput(ns("edu_tbl")),
        info_note("Schools: OpenStreetMap (ODbL), classified by level from their tags and names; OSM coverage is incomplete, so treat counts as a minimum. Children 5-19: WorldPop. Distance: straight line to the nearest DHIS2 facility with coordinates.")))
  )
}

population_server <- function(id) moduleServer(id, function(input, output, session) {
  area <- area_server("area")
  ns <- session$ns
  o <- OU[level_name %in% c("district", "dlg", "subcounty")][order(match(level_name, c("district", "dlg", "subcounty")), name)]
  updateSelectizeInput(session, "cmp", server = TRUE, selected = "",
                       choices = c("No comparison" = "", "Busoga" = META$region_uid,
                                   setNames(o$uid, sprintf("%s · %s", o$name, LEVEL_LABEL[o$level_name]))))
  output$pyr_area <- renderUI({
    if (nzchar(input$cmp %||% "") && input$cmp_view == "side")
      layout_columns(col_widths = c(6, 6), plotlyOutput(ns("pyr"), height = 540), plotlyOutput(ns("pyr2"), height = 540))
    else layout_columns(col_widths = c(8, 4), plotlyOutput(ns("pyr"), height = 560), uiOutput(ns("pyr_stats")))
  })
  output$pyr2 <- renderPlotly({
    req(PYR, nzchar(input$cmp %||% "")); y <- as.integer(input$pyr_year)
    x <- PYR[uid == input$cmp & year == y]; req(nrow(x))
    pyramid_plot(x, sprintf("<b>%s</b>, %d", htmlEscape(if (input$cmp == META$region_uid) "Busoga" else ou_name[[input$cmp]]), y))
  })

  output$pyr <- renderPlotly({
    a <- area(); req(PYR); y <- as.integer(input$pyr_year)
    x <- PYR[uid == a$uid]
    if (!nrow(x)) return(empty_plot("No population estimate for this area"))
    cmp <- input$cmp %||% ""
    if (nzchar(cmp) && input$cmp_view == "overlay") {
      c2 <- PYR[uid == cmp & year == y]
      cname <- if (cmp == META$region_uid) "Busoga" else ou_name[[cmp]]
      return(pyramid_plot(x[year == y], sprintf("<b>%s</b> (bars) vs <b>%s</b> (outline), %d", htmlEscape(a$name), htmlEscape(cname), y),
                          c2, compare_name = cname))
    }
    other <- setdiff(unique(x$year), y)
    pyramid_plot(x[year == y], sprintf("<b>%s</b>, %d", htmlEscape(a$name), y),
                 if (length(other) && !nzchar(cmp)) x[year == other[1]] else NULL, compare_name = if (length(other)) other[1] else NULL)
  })
  output$pyr_stats <- renderUI({
    a <- area(); req(PYR); x <- PYR[uid == a$uid & year == max(year)]; req(nrow(x))
    x <- PYR[uid == a$uid & year == as.integer(input$pyr_year)]
    tot <- sum(x$pop); u5 <- x[age_start < 5, sum(pop)]; kids <- x[age_start < 15, sum(pop)]
    old <- x[age_start >= 65, sum(pop)]; wra <- x[sex == "Female" & age_start >= 15 & age_start < 50, sum(pop)]
    adol <- x[age_start %in% c(10, 15), sum(pop)]
    st <- function(l, v, s) div(class = "pop-stat", div(class = "muted", l), tags$b(v), span(class = "muted", s))
    div(class = "stat-row", style = "flex-direction:column;gap:.8rem;padding-top:1rem",
        st("Total population", format(round(tot), big.mark = ","), "MoH 107a projection (UBOS-based); age structure from WorldPop"),
        st("Children under 5", format(round(u5), big.mark = ","), sprintf("%.1f%%", 100 * u5 / tot)),
        st("Children under 15", format(round(kids), big.mark = ","), sprintf("%.1f%%", 100 * kids / tot)),
        st("Adolescents 10-19", format(round(adol), big.mark = ","), sprintf("%.1f%%", 100 * adol / tot)),
        st("Women 15-49", format(round(wra), big.mark = ","), sprintf("%.1f%%", 100 * wra / tot)),
        st("Aged 65+", format(round(old), big.mark = ","), sprintf("%.1f%%", 100 * old / tot)),
        st("Dependency ratio", sprintf("%.0f", 100 * (kids + old) / (tot - kids - old)), "per 100 aged 15-64"))
  })

  output$ubos_bar <- renderPlotly({
    req(UBOS); x <- UBOS[, .(pop = sum(pop)), by = .(district, sex)]
    ord <- x[, sum(pop), by = district][order(V1)]$district
    plot_ly(x, y = ~factor(district, levels = ord), x = ~pop, color = ~sex, colors = c(Female = BRAND$maroon, Male = BRAND$navy),
            type = "bar", orientation = "h", hovertemplate = "%{y}: %{x:,.0f}<extra></extra>") |>
      plotly_base(ytitle = "") |> layout(barmode = "stack", xaxis = list(title = "Population (UBOS 2023)"), yaxis = list(title = ""))
  })
  output$ubos_pyr <- renderPlotly({
    req(UBOS); a <- area()
    dname <- if (a$level == "region") NULL else sub(" District$| City$", "", OU$name[OU$uid == (if (a$level == "district") a$uid else OU$uid_l3[OU$uid == a$uid])])
    x <- if (is.null(dname)) UBOS else UBOS[tolower(district) == tolower(dname) | (tolower(dname) == "jinja" & district == "Jinja")]
    if (!nrow(x)) return(empty_plot("No UBOS table for this area"))
    pyramid_plot(x, sprintf("<b>UBOS 2023: %s</b>", if (is.null(dname)) "Busoga" else htmlEscape(dname)))
  })

  edu_area <- reactive({ req(EDU); a <- area(); EDU[uid == a$uid] })
  output$edu_hero <- renderUI({
    e <- edu_area(); req(nrow(e))
    tile <- function(lab, v, sub) div(div(class = "h-lab", lab), div(class = "h-val", v), div(class = "h-sub", sub))
    div(class = "hero",
        tile("Schools mapped", format(e$schools, big.mark = ","), "OpenStreetMap"),
        tile("Primary", format(e$Primary, big.mark = ","), sprintf("pre-primary %s", format(e$`Pre-primary`, big.mark = ","))),
        tile("Secondary", format(e$Secondary, big.mark = ","), sprintf("technical %s, tertiary %s, universities %s", e$`Technical / vocational`, e$`Tertiary college / institute`, e$University)),
        tile("Schools per 10,000 children", sprintf("%.1f", e$schools_per_10k), "children aged 5-19"),
        tile("Distance to a facility", sprintf("%.1f km", e$median_km), sprintf("median; %s schools over 5 km", e$over5km)))
  })
  output$sch_map <- renderLeaflet({
    req(SCH); a <- area()
    s <- if (a$level == "region") SCH else SCH[sc_uid %in% OU[uid_l3 == a$uid | uid_l4 == a$uid | uid == a$uid, uid]]
    m <- base_map() |> addPolylines(data = GEO$district, color = BRAND$navy, weight = 1.4)
    for (k in names(SCHOOL_COL)) {
      z <- s[category == k]; if (!nrow(z)) next
      m <- m |> addCircleMarkers(data = z, lng = ~lon, lat = ~lat, radius = if (k %in% c("University", "Tertiary college / institute")) 6 else 3.5,
                                 stroke = TRUE, color = "white", weight = .6, fillColor = SCHOOL_COL[[k]], fillOpacity = .9, group = k,
                                 label = ~sprintf("%s (%s) · %.1f km to %s", ifelse(is.na(name), "School", name), category, near_km, near_fac))
    }
    m |> addLayersControl(overlayGroups = names(SCHOOL_COL), options = layersControlOptions(collapsed = TRUE)) |>
      addLegend("bottomright", colors = unname(SCHOOL_COL), labels = names(SCHOOL_COL), opacity = 1, title = "School level")
  })
  output$sch_bar <- renderPlotly({
    req(EDU); a <- area()
    lvl <- if (a$level %in% c("region")) "district" else if (a$level == "district") "subcounty" else "subcounty"
    x <- EDU[level == lvl]
    if (a$level != "region") x <- x[uid %in% OU[uid_l3 == a$uid | uid_l4 == a$uid | uid == a$uid, uid]]
    x[, name := ou_name[uid]]
    long <- melt(x, id.vars = c("name"), measure.vars = intersect(names(SCHOOL_COL), names(x)), variable.name = "cat", value.name = "n")
    ord <- x[order(schools)]$name
    p <- plot_ly()
    for (k in names(SCHOOL_COL)) { z <- long[cat == k]; if (!nrow(z) || !sum(z$n)) next
      p <- p |> add_bars(data = z, y = ~factor(name, levels = ord), x = ~n, name = k, orientation = "h", marker = list(color = SCHOOL_COL[[k]]),
                         hovertemplate = paste0(k, ": %{x}<extra></extra>")) }
    p |> plotly_base() |> layout(barmode = "stack", yaxis = list(title = "", tickfont = list(size = 9)), xaxis = list(title = "Schools"))
  })
  output$edu_tbl <- renderReactable({
    req(EDU); x <- EDU[level %in% c("district", "subcounty")][, `:=`(Area = ou_name[uid], Level = LEVEL_LABEL[level])]
    reactable(x[order(Level, Area), .(Level, Area, Schools = schools, `Pre-primary`, Primary, Secondary, Technical = `Technical / vocational`,
                                     Tertiary = `Tertiary college / institute`, University, `Children 5-19` = round(children_5_19),
                                     `Schools per 10k` = schools_per_10k, `Median km to facility` = median_km, `Over 5 km` = over5km)],
              searchable = TRUE, compact = TRUE, groupBy = "Level", pagination = FALSE, height = 420,
              columns = list(`Children 5-19` = colDef(format = colFormat(separators = TRUE))))
  })
})
