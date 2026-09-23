# Deep dive: complete profile of any unit (region, district, DLG, sub-county or facility).

deepdive_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Deep dive", "Profile of one area or facility",
              "Everything about one unit: where it sits, how it performs on every indicator against its parent area and Busoga, its rank among peers, its trends and how well it reports."),
    filter_bar(
      selectizeInput(ns("unit"), "Area or facility", choices = NULL, width = "440px",
                     options = list(placeholder = "Search any district, DLG, sub-county or facility...")),
      period_ui(ns("period"))),
    uiOutput(ns("head")),
    navset_card_underline(full_screen = TRUE,
      nav_panel("All indicators", reactableOutput(ns("ind_tbl"))),
      nav_panel("Trend", div(style = "padding:.4rem 0",
                             selectInput(ns("trend_ind"), NULL, indicator_choices(), width = "360px")),
                plotlyOutput(ns("trend"), height = 420)),
      nav_panel("Reporting", plotlyOutput(ns("rep_heat"), height = 360),
                info_note("Share of expected monthly reports received, by dataset. Grey: not expected that month.")),
      nav_panel("Data quality", uiOutput(ns("dq"))),
      nav_panel("Location", leafletOutput(ns("loc"), height = 460)))
  )
}

deepdive_server <- function(id, jump = NULL) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  per <- period_server("period")
  ord <- c(region = 1, district = 2, dlg = 3, subcounty = 4, facility = 5)
  o <- OU[order(ord[level_name], district, name)]
  ch <- c(setNames(META$region_uid, "Busoga (region)"),
          setNames(o[level_name != "region", uid],
                   sprintf("%s · %s%s", o[level_name != "region", name], LEVEL_LABEL[o[level_name != "region", level_name]],
                           ifelse(o[level_name != "region", level_name] %in% c("subcounty", "facility"),
                                  paste0(", ", o[level_name != "region", district]), ""))))
  updateSelectizeInput(session, "unit", choices = ch, selected = "", server = TRUE)
  if (!is.null(jump)) observeEvent(jump(), { req(jump()); updateSelectizeInput(session, "unit", choices = ch, selected = jump(), server = TRUE) })

  u <- reactive({ req(input$unit); input$unit })
  lvl <- reactive(if (u() == META$region_uid) "region" else ou_level[[u()]])

  output$head <- renderUI({
    x <- OU[uid == u()]; l <- lvl(); p <- per()
    chain <- parent_chain(u()); chain <- chain[-length(chain)]
    path <- if (length(chain)) paste(vapply(chain, function(c) if (c == META$region_uid) "Busoga" else ou_name[[c]], ""), collapse = " › ") else "Uganda"
    stats <- if (l == "facility") {
      d <- if (!is.null(DQF)) DQF[uid == u()] else NULL
      list(div("Level", tags$b(x$grp_level %||% "–")), div("Ownership", tags$b(x$grp_ownership %||% "–")),
           div("Authority", tags$b(x$grp_authority %||% "–")),
           div("Reporting status", tags$b(if (!is.null(d) && nrow(d)) d$report_status else "–")),
           div("Completeness (12 m)", tags$b(if (!is.null(d) && nrow(d) && !is.na(d$completeness_12m)) sprintf("%.0f%%", d$completeness_12m) else "–")),
           div("Opened", tags$b(x$opening_date %||% "–")))
    } else {
      f <- OU[level_name == "facility"]; if (l != "region") f <- f[uid_l3 == u() | uid_l4 == u() | uid_l5 == u()]
      popv <- POP[uid == u() & year == p$to %/% 100L]
      list(div("Projected population", tags$b(if (nrow(popv)) formatC(popv$pop, big.mark = ",", format = "d") else "–")),
           div("Health facilities", tags$b(nrow(f))),
           div("Government", tags$b(f[grp_ownership == "GOV", .N])), div("PNFP", tags$b(f[grp_ownership == "PNFP", .N])),
           div("Private for profit", tags$b(f[grp_ownership == "PFP", .N])),
           div("Hospitals / HC IV / HC III / HC II", tags$b(sprintf("%d / %d / %d / %d", f[grp_level %in% c("RRH", "General Hospital"), .N],
                                                               f[grp_level == "HC IV", .N], f[grp_level == "HC III", .N], f[grp_level == "HC II", .N]))))
    }
    card(class = "mb-3", card_body(
      div(class = "profile-head",
          div(style = "flex:1", div(class = "ptype", LEVEL_LABEL[[l]]), h3(if (l == "region") "Busoga" else x$name),
              div(class = "muted", path)),
          div(class = "stat-row", stats))))
  })

  ind_rows <- reactive({
    p <- per(); l <- lvl()
    codes <- if (l == "facility") IND[area_only == FALSE, code] else IND$code
    me <- summarise_ind(codes, l, p$from, p$to, uids = u())
    peers <- summarise_ind(codes, l, p$from, p$to)
    ref <- region_value(codes, p$from, p$to)
    chain <- parent_chain(u()); par <- if (length(chain) >= 2) chain[length(chain) - 1] else NA
    pl <- if (is.na(par)) NA else if (par == META$region_uid) "region" else ou_level[[par]]
    pv <- if (is.na(par)) data.table(code = character(), value = numeric()) else summarise_ind(codes, pl, p$from, p$to, uids = par)
    tr <- summarise_ind(codes, l, max(MONTH_MIN, date_ym(seq(ym_date(p$to), by = "-23 months", length.out = 2)[2])), p$to,
                        uids = u(), by = "month")
    x <- IND[code %in% codes, .(code, theme = as.character(theme), label, direction, unit)]
    x[, value := me$value[match(code, me$code)]]
    x[, parent := pv$value[match(code, pv$code)]]
    x[, busoga := ref[code]]
    x[, rank := vapply(code, function(cd) {
      pe <- peers[code == cd & !is.na(value)]; v <- me[code == cd, value]
      if (!length(v) || is.na(v) || nrow(pe) < 2) return(NA_character_)
      dirn <- IND[code == cd, direction]
      r <- if (dirn == "low") sum(pe$value < v) + 1 else sum(pe$value > v) + 1
      if (dirn == "neutral") sprintf("%d of %d (by size)", sum(pe$value > v) + 1, nrow(pe)) else sprintf("%d of %d", r, nrow(pe))
    }, "")]
    x[, spark := vapply(code, function(cd) as.character(spark_svg(tr[code == cd][order(bucket)]$value, theme_col(IND[code == cd, theme]), 120, 26)), "")]
    x[, status := status_vs(value, busoga, direction)]
    list(x = x[!(is.na(value) & is.na(busoga))], parent_name = if (is.na(par)) "" else if (par == META$region_uid) "Busoga" else ou_name[[par]])
  })
  output$ind_tbl <- renderReactable({
    d <- ind_rows(); x <- d$x
    reactable(x[, .(theme, label, value, parent, busoga, rank, spark, status, code)], groupBy = "theme", defaultExpanded = TRUE,
      compact = TRUE, pagination = FALSE, height = 620, highlight = TRUE,
      columns = list(
        theme = colDef(name = "Theme", minWidth = 150, grouped = JS("function(cell){return cell.value}"),
                       style = function(v) list(borderLeft = sprintf("4px solid %s", theme_col(v)))),
        label = colDef(name = "Indicator", minWidth = 260),
        value = colDef(name = "This unit", align = "right", minWidth = 110,
                       cell = function(v, i) { st <- x$status[i]
                         div(class = "sc-cell", style = sprintf("background:%s;font-weight:700", if (st == "none") "transparent" else paste0(STATUS[[st]], "30")),
                             span(class = "ic", style = sprintf("color:%s", if (st == "none") BRAND$muted else STATUS[[st]]), STATUS_ICON[[st]]),
                             fmt_val(v, x$code[i])) }),
        parent = colDef(name = if (nzchar(d$parent_name)) d$parent_name else "Parent", align = "right", minWidth = 100,
                        cell = function(v, i) fmt_val(v, x$code[i])),
        busoga = colDef(name = "Busoga", align = "right", minWidth = 90, cell = function(v, i) fmt_val(v, x$code[i])),
        rank = colDef(name = "Rank among peers", align = "right", minWidth = 120),
        spark = colDef(name = "Last 24 months", html = TRUE, minWidth = 130, cell = function(v) HTML(v)),
        status = colDef(show = FALSE), code = colDef(show = FALSE)))
  })

  output$trend <- renderPlotly({
    cd <- input$trend_ind; l <- lvl()
    if (l == "facility" && IND[code == cd, area_only]) return(empty_plot("Population-based indicator: not available for a single facility"))
    chain <- parent_chain(u())
    series <- lapply(seq_along(chain), function(k) {
      uu <- chain[k]; ll <- if (uu == META$region_uid) "region" else ou_level[[uu]]
      s <- summarise_ind(cd, ll, MONTH_MIN, MONTH_MAX, uids = uu, by = "month")
      if (nrow(s)) s[, who := if (uu == META$region_uid) "Busoga" else ou_name[[uu]]]
      s
    })
    col <- theme_col(IND[code == cd, theme])
    p <- plot_ly()
    n <- length(series)
    greys <- c("#b9b7b0", "#9b9a94", "#7d7b74", "#5f5e59")
    for (k in seq_len(n)) {
      s <- series[[k]]; if (!nrow(s)) next
      last <- k == n
      p <- p |> add_lines(data = s[order(bucket)], x = ~bucket_date(bucket, "month"), y = ~value, name = s$who[1],
                          line = list(color = if (last) col else greys[min(k, 4)], width = if (last) 2.5 else 1.5,
                                      dash = if (last) "solid" else "dot"),
                          hovertemplate = paste0(htmlEscape(s$who[1]), ": %{y:,.1f}<extra></extra>"))
    }
    p |> plotly_base(ytitle = unit_label(cd)) |> layout(hovermode = "x unified")
  })

  output$rep_heat <- renderPlotly({
    l <- lvl(); f <- OU[level_name == "facility"]
    if (l == "facility") f <- f[uid == u()] else if (l != "region") f <- f[uid_l3 == u() | uid_l4 == u() | uid_l5 == u()]
    r <- REP[uid %in% f$uid, .(expected = sum(expected), actual = sum(actual)), by = .(dataset, period)]
    if (!nrow(r)) return(empty_plot("No reporting records"))
    r[, rate := fifelse(expected > 0, pmin(100, 100 * actual / expected), NA_real_)]
    r[, ds := META$datasets[[dataset]] %||% dataset, by = dataset]
    w <- dcast(r, ds ~ period, value.var = "rate")
    z <- as.matrix(w[, -1]); xs <- ym_date(as.integer(colnames(z)))
    plot_ly(x = xs, y = w$ds, z = z, type = "heatmap", zmin = 0, zmax = 100, xgap = 1, ygap = 2,
            colorscale = list(c(0, "#d03b3b"), c(.5, "#fab219"), c(.8, "#b7e4b7"), c(1, "#0ca30c")),
            colorbar = list(title = "% received", len = .8),
            hovertemplate = "%{y}<br>%{x|%b %Y}: %{z:.0f}% received<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(yaxis = list(title = "", autorange = "reversed"), xaxis = list(title = ""))
  })

  output$dq <- renderUI({
    l <- lvl()
    if (is.null(DQF)) return(p("Data-quality results are not available."))
    f <- OU[level_name == "facility"]
    if (l == "facility") f <- f[uid == u()] else if (l != "region") f <- f[uid_l3 == u() | uid_l4 == u() | uid_l5 == u()]
    d <- DQF[uid %in% f$uid]
    chk <- if (!is.null(DQC)) DQC[uid %in% f$uid] else NULL
    out <- if (!is.null(DQO)) DQO[uid %in% f$uid] else NULL
    kp <- function(lab, v, sub) div(class = "kpi", style = sprintf("--th:%s", BRAND$navy), div(class = "kpi-label", lab),
                                    div(class = "kpi-value", v), div(class = "kpi-delta", sub))
    tagList(
      div(class = "kpi-grid",
          kp("Data quality score", if (nrow(d)) sprintf("%.0f", mean(d$dq_score, na.rm = TRUE)) else "–", "0 to 100, last 12 months"),
          kp("Completeness", if (nrow(d)) sprintf("%.0f%%", mean(d$completeness_12m, na.rm = TRUE)) else "–", "105:01, last 12 months"),
          kp("Timeliness", if (nrow(d)) sprintf("%.0f%%", mean(d$timeliness_12m, na.rm = TRUE)) else "–", "on time, last 12 months"),
          kp("Consistency flags", if (!is.null(chk)) format(nrow(chk), big.mark = ",") else "–", "impossible values, all months"),
          kp("Outlier values", if (!is.null(out)) format(nrow(out), big.mark = ",") else "–", "extreme monthly values, all months")),
      if (!is.null(chk) && nrow(chk)) tagList(h6(class = "mt-3", "Most recent consistency flags"),
        renderDT(datatable(chk[order(-period)][1:min(.N, 200), .(Facility = ou_name[uid], Month = fmt_month(period), Check = check, Detail = detail)],
                           rownames = FALSE, options = list(pageLength = 8, dom = "tip")))))
  })

  output$loc <- renderLeaflet({
    l <- lvl(); m <- base_map()
    if (l == "facility") {
      x <- OU[uid == u()]
      if (!is.na(x$lat)) {
        near <- FAC_PTS[abs(lat - x$lat) < .15 & abs(lon - x$lon) < .15 & uid != u()]
        m <- m |> addCircleMarkers(data = near, lng = ~lon, lat = ~lat, radius = 4, stroke = FALSE, fillColor = "#7d7b74",
                                   fillOpacity = .7, label = ~name) |>
          addCircleMarkers(lng = x$lon, lat = x$lat, radius = 10, color = "white", weight = 2, fillColor = BRAND$maroon,
                           fillOpacity = 1, label = x$name) |> setView(x$lon, x$lat, zoom = 12)
      } else m <- m |> addControl("This facility has no coordinates in DHIS2.", position = "topright")
      return(m)
    }
    g <- if (l == "region") GEO$region else GEO[[l]][GEO[[l]]$uid == u(), ]
    if (nrow(g)) { bb <- sf::st_bbox(g)
      m <- m |> addPolygons(data = g, fillColor = BRAND$navy, fillOpacity = .1, color = BRAND$navy, weight = 2.5) |>
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) }
    f <- FAC_PTS; if (l != "region") f <- f[uid_l3 == u() | uid_l4 == u() | uid_l5 == u()]
    m |> addCircleMarkers(data = f, lng = ~lon, lat = ~lat, radius = 4, color = "white", weight = 1, fillColor = BRAND$maroon,
                          fillOpacity = .9, label = ~name)
  })
})
