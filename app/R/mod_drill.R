# Drill-down: walk the cascade Busoga > district > DLG > sub-county > facility.

drill_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Cascade drill-down", "From Busoga down to every facility",
              "Start at Busoga. Click a row (or an area on the map) to go one level down; use the breadcrumb to go back up. The treemap shows the whole cascade at once.", key = "drill"),
    filter_bar(
      selectInput(ns("theme"), "Programme theme", THEMES$theme, width = "260px"),
      period_ui(ns("period")),
      selectInput(ns("tm_ind"), "Treemap measure", indicator_choices(facility = TRUE)[sapply(indicator_choices(facility = TRUE), length) > 0],
                  selected = "DEL01", width = "300px")),
    uiOutput(ns("crumbs")),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE, card_header(textOutput(ns("tbl_title"), inline = TRUE)), reactableOutput(ns("tbl"))),
      card(full_screen = TRUE, card_header("Where they are"), leafletOutput(ns("map"), height = 430))),
    card(full_screen = TRUE, card_header("The whole cascade", span(class = "sub", "size = selected measure (counts); click to zoom, click the centre to go back")),
         plotlyOutput(ns("treemap"), height = 560))
  )
}

drill_server <- function(id) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  per <- period_server("period")
  cur <- reactiveVal(META$region_uid)
  observeEvent(input$tm_ind, {}, ignoreInit = TRUE)

  output$crumbs <- renderUI({
    chain <- parent_chain(cur())
    items <- lapply(seq_along(chain), function(k) {
      u <- chain[k]; last <- k == length(chain)
      tagList(if (k > 1) span(class = "sep", "›"),
              actionButton(ns(paste0("crumb_", k)), if (u == META$region_uid) "Busoga" else ou_name[[u]],
                           class = if (last) "btn-primary" else "btn-outline-secondary",
                           onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("go"), u)))
    })
    div(class = "crumbs", fontawesome::fa("sitemap", fill = BRAND$navy), items)
  })
  observeEvent(input$go, cur(input$go))

  kids <- reactive(children_of(cur()))
  child_lvl <- reactive({
    lvl <- if (cur() == META$region_uid) "region" else ou_level[[cur()]]
    child_level(lvl)
  })
  tbl_data <- reactive({
    p <- per(); k <- kids(); cl <- child_lvl(); req(length(k), !is.na(cl))
    codes <- IND[theme == input$theme, code]
    if (cl == "facility") codes <- IND[code %in% codes & area_only == FALSE, code]
    s <- summarise_ind(codes, cl, p$from, p$to, uids = k)
    parent_lvl <- if (cur() == META$region_uid) "region" else ou_level[[cur()]]
    ref <- summarise_ind(codes, parent_lvl, p$from, p$to, uids = cur())
    w <- dcast(s[, .(uid, code, value)], uid ~ code, value.var = "value")
    w <- merge(data.table(uid = unname(k), Unit = names(k)), w, by = "uid", all.x = TRUE)
    list(w = w, codes = intersect(codes, names(w)), ref = setNames(ref$value, ref$code), cl = cl)
  })
  output$tbl_title <- renderText({
    d <- tbl_data()
    sprintf("%d %s in %s · %s", nrow(d$w), LEVEL_PLURAL[[d$cl]],
            if (cur() == META$region_uid) "Busoga" else ou_name[[cur()]], per()$label)
  })
  output$tbl <- renderReactable({
    d <- tbl_data(); validate(need(length(d$codes), "No data for this theme at this level."))
    cols <- lapply(d$codes, function(cd) {
      dirn <- IND[code == cd, direction]; ref <- d$ref[cd]
      colDef(name = ind_label(cd), minWidth = 110, align = "right",
             header = function(v) div(style = "white-space:normal;line-height:1.15", v),
             cell = function(value) {
               st <- status_vs(value, ref, dirn)
               div(class = "sc-cell", style = sprintf("background:%s", if (st == "none") "transparent" else paste0(STATUS[[st]], "30")),
                   span(class = "ic", style = sprintf("color:%s", if (st == "none") BRAND$muted else STATUS[[st]]), STATUS_ICON[[st]]),
                   fmt_val(value, cd))
             })
    })
    names(cols) <- d$codes
    drillable <- d$cl != "facility"
    reactable(d$w[, c("uid", "Unit", d$codes), with = FALSE], compact = TRUE, highlight = TRUE, searchable = nrow(d$w) > 12,
              pagination = nrow(d$w) > 25, defaultPageSize = 25,
              columns = c(list(uid = colDef(show = FALSE),
                               Unit = colDef(name = LEVEL_LABEL[[d$cl]], sticky = "left", minWidth = 200,
                                             cell = function(v) if (drillable) span(style = sprintf("color:%s;font-weight:600;cursor:pointer", BRAND$navy), v, " ›") else span(style = "font-weight:600", v))),
                          cols),
              onClick = if (drillable) JS(sprintf("function(rowInfo){ Shiny.setInputValue('%s', rowInfo.values.uid, {priority:'event'}) }", ns("go"))) else
                JS(sprintf("function(rowInfo){ Shiny.setInputValue('%s', rowInfo.values.uid, {priority:'event'}); %s }", ns("fac_click"), go_to_page("deep"))),
              rowStyle = list(cursor = "pointer"))
  })
  observeEvent(input$fac_click, {
    session$userData$deep_dive_uid(input$fac_click)
  })

  output$map <- renderLeaflet({
    d <- tbl_data(); k <- kids(); cl <- d$cl
    m <- base_map()
    if (cl == "facility") {
      pts <- FAC_PTS[uid %in% k]
      if (cur() %in% GEO$subcounty$uid) m <- m |> addPolygons(data = GEO$subcounty[GEO$subcounty$uid == cur(), ], fill = FALSE, color = BRAND$navy, weight = 2)
      if (nrow(pts)) m <- m |> addCircleMarkers(data = pts, lng = ~lon, lat = ~lat, radius = 6, color = "white", weight = 1,
                                                fillColor = BRAND$maroon, fillOpacity = .9, label = ~name) |>
        fitBounds(min(pts$lon) - .02, min(pts$lat) - .02, max(pts$lon) + .02, max(pts$lat) + .02)
    } else {
      g <- GEO[[cl]][GEO[[cl]]$uid %in% k, ]
      if (nrow(g)) {
        bb <- sf::st_bbox(g)
        m <- m |> addPolygons(data = g, layerId = ~uid, fillColor = BRAND$navy, fillOpacity = .12, color = BRAND$navy,
                              weight = 1.2, label = ~name,
                              highlightOptions = highlightOptions(fillOpacity = .35, weight = 2.5)) |>
          fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      }
    }
    m
  })
  observeEvent(input$map_shape_click, { if (!is.null(input$map_shape_click$id)) cur(input$map_shape_click$id) })

  output$treemap <- renderPlotly({
    p <- per(); cd <- input$tm_ind
    s <- summarise_ind(cd, "facility", p$from, p$to)[!is.na(value) & value > 0]
    if (!nrow(s)) return(empty_plot())
    f <- merge(s[, .(uid, value)], OU[, .(uid, name, uid_l3, uid_l4, uid_l5)], by = "uid")
    ids <- c(f$uid, unique(f$uid_l5), unique(f$uid_l4), unique(f$uid_l3), META$region_uid)
    par <- c(f$uid_l5, OU$uid_l4[match(unique(f$uid_l5), OU$uid)], OU$uid_l3[match(unique(f$uid_l4), OU$uid)],
             rep(META$region_uid, length(unique(f$uid_l3))), "")
    lab <- c(f$name, ou_name[unique(f$uid_l5)], ou_name[unique(f$uid_l4)], ou_name[unique(f$uid_l3)], "Busoga")
    val <- c(f$value, rep(0, length(ids) - nrow(f)))
    dcol <- setNames(rep(SERIES, length.out = length(unique(f$uid_l3))), sort(unique(f$uid_l3)))
    plot_ly(type = "treemap", ids = ids, parents = par, labels = lab, values = val, branchvalues = "remainder",
            maxdepth = 3, textinfo = "label+value",
            hovertemplate = paste0("<b>%{label}</b><br>", ind_label(cd), ": %{value:,.0f}<br>%{percentRoot:.1%} of Busoga<extra></extra>"),
            marker = list(colors = c(rep(NA, length(ids) - length(unique(f$uid_l3)) - 1), dcol[unique(f$uid_l3)], BRAND$navy),
                          line = list(color = "white", width = 1))) |>
      layout(margin = list(t = 10, l = 0, r = 0, b = 0), font = list(family = "Arial, Helvetica, sans-serif")) |>
      config(displaylogo = FALSE)
  })
})
