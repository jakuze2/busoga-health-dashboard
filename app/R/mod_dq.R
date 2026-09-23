# Data quality: completeness, timeliness, non-reporting, consistency, outliers, where to improve.

dq_ui <- function(id) {
  ns <- NS(id)
  ds <- unlist(META$datasets)
  tagList(
    page_head("Data quality", "How good are the data, and where should we improve?",
              "Completeness and timeliness of monthly reports (DHIS2 reporting rates), facilities that do not report, internally inconsistent values and extreme outliers. Use 'Where to improve' to target support visits."),
    filter_bar(area_ui(ns("area"), depth = 2),
               selectInput(ns("ds"), "Report (dataset)", setNames(names(ds), ds), selected = "RtEYsASU7PG", width = "260px"),
               period_ui(ns("period"))),
    uiOutput(ns("hero")),
    navset_card_underline(full_screen = TRUE,
      nav_panel("Where to improve",
        div(style = "display:flex;gap:1rem;flex-wrap:wrap;align-items:flex-end;padding-top:.3rem",
            selectInput(ns("lvl"), "Rank", c("Districts / cities" = "district", "DLGs / municipalities" = "dlg",
                                             "Sub-counties / divisions" = "subcounty", "Health facilities" = "facility"),
                        selected = "subcounty", width = "220px")),
        reactableOutput(ns("improve")),
        info_note("Score (0 to 100) = average of completeness, timeliness, share of facility-months with no consistency flag, and share of values that are not outliers, over the last 12 months. Lowest scores first.")),
      nav_panel("Completeness & timeliness",
        layout_columns(col_widths = c(8, 4), plotlyOutput(ns("ct_trend"), height = 400), plotlyOutput(ns("ct_ds"), height = 400))),
      nav_panel("Reporting heatmap", plotlyOutput(ns("heat"), height = 700),
        info_note("One row per facility in the selected area. Green: on time. Yellow: late. Red: expected but missing. Grey: not expected.")),
      nav_panel("Non-reporting facilities",
        layout_columns(col_widths = c(4, 8), plotlyOutput(ns("nr_bar"), height = 420), DTOutput(ns("nr_tbl")))),
      nav_panel("Consistency checks",
        layout_columns(col_widths = c(5, 7), plotlyOutput(ns("chk_bar"), height = 420), DTOutput(ns("chk_tbl"))),
        info_note("A value fails a check when it is impossible: a proportion above 100% (numerator larger than its denominator, e.g. more ANC 1 clients in the first trimester than ANC 1 clients) or a negative dropout (more third doses than first doses).")),
      nav_panel("Outliers", DTOutput(ns("out_tbl")),
        info_note("Outlier: a monthly count far from the facility's own typical month: a modified z-score above 3.5, measured from the median and the median absolute deviation. Check the register; a genuine surge is not an error.")))
  )
}

dq_server <- function(id) moduleServer(id, function(input, output, session) {
  area <- area_server("area"); per <- period_server("period")
  facs <- reactive({
    a <- area(); f <- OU[level_name == "facility"]
    if (a$level != "region") f <- f[uid_l3 == a$uid | uid_l4 == a$uid | uid_l5 == a$uid]
    f
  })
  rep_sel <- reactive({ p <- per(); REP[dataset == input$ds & period >= p$from & period <= p$to & uid %in% facs()$uid] })

  output$hero <- renderUI({
    r <- rep_sel(); f <- facs(); d <- if (!is.null(DQF)) DQF[uid %in% f$uid] else data.table()
    pc <- function(a, b) if (b > 0) sprintf("%.0f%%", 100 * a / b) else "–"
    tile <- function(lab, v, sub) div(div(class = "h-lab", lab), div(class = "h-val", v), div(class = "h-sub", sub))
    div(class = "hero",
        tile("Completeness", pc(sum(r$actual), sum(r$expected)), sprintf("%s of %s expected reports", format(sum(r$actual), big.mark = ","), format(sum(r$expected), big.mark = ","))),
        tile("Timeliness", pc(sum(r$on_time), sum(r$expected)), "received by the deadline"),
        tile("Regular reporters", if (nrow(d)) d[report_status == "Regular reporter", .N] else "–", "≥ 75% of the last 12 months"),
        tile("Stopped reporting", if (nrow(d)) d[report_status == "Stopped reporting", .N] else "–", "none in the last 6 months"),
        tile("Never reported", if (nrow(d)) d[report_status == "Never reported", .N] else "–", "expected, but no report ever"),
        tile("Average DQ score", if (nrow(d)) sprintf("%.0f", mean(d$dq_score, na.rm = TRUE)) else "–", "0 to 100, last 12 months"))
  })

  output$improve <- renderReactable({
    req(DQF); f <- facs(); lvl <- input$lvl
    d <- merge(DQF[uid %in% f$uid], OU[, .(uid, name, uid_l3, uid_l4, uid_l5, district)], by = "uid")
    key <- switch(lvl, district = "uid_l3", dlg = "uid_l4", subcounty = "uid_l5", facility = "uid")
    x <- d[, .(Facilities = .N,
               Completeness = mean(completeness_12m, na.rm = TRUE), Timeliness = mean(timeliness_12m, na.rm = TRUE),
               Consistency = mean(consistency_12m, na.rm = TRUE), `Non-outlier` = mean(nonoutlier_12m, na.rm = TRUE),
               Score = mean(dq_score, na.rm = TRUE),
               `Not reporting` = sum(report_status %in% c("Never reported", "Stopped reporting")),
               Flags = sum(flags_12m, na.rm = TRUE), Outliers = sum(outliers_12m, na.rm = TRUE)), by = c(u = key)]
    x[, Unit := ou_name[u]]
    x[, `Main issue` := fcase(
      `Not reporting` > 0 & lvl != "facility", sprintf("%d facilities not reporting", `Not reporting`),
      Completeness < 80, sprintf("Missing reports (%.0f%% complete)", Completeness),
      Timeliness < 70, sprintf("Late reports (%.0f%% on time)", Timeliness),
      Consistency < 95, sprintf("%d impossible values", Flags),
      `Non-outlier` < 97, sprintf("%d outlier values", Outliers),
      default = "No major issue")]
    if (lvl != "facility") x[, District := NULL] else x[, District := OU$district[match(u, OU$uid)]]
    x <- x[order(Score)]
    bar <- function(v) { if (is.na(v)) return("–")
      col <- if (v >= 90) STATUS$good else if (v >= 75) STATUS$warning else STATUS$critical
      div(style = "display:flex;gap:.35rem;align-items:center",
          div(style = "flex:1;height:7px;background:#f0efec;border-radius:4px",
              div(style = sprintf("width:%.0f%%;height:7px;background:%s;border-radius:4px", max(0, min(100, v)), col))),
          span(style = "min-width:2.6em;text-align:right;font-variant-numeric:tabular-nums", sprintf("%.0f", v))) }
    cols <- list(u = colDef(show = FALSE), Unit = colDef(name = LEVEL_LABEL[[lvl]], minWidth = 190, sticky = "left",
                                                          style = list(fontWeight = 600)),
                 Score = colDef(minWidth = 120, cell = bar), Completeness = colDef(minWidth = 110, cell = bar),
                 Timeliness = colDef(minWidth = 110, cell = bar), Consistency = colDef(minWidth = 110, cell = bar),
                 `Non-outlier` = colDef(minWidth = 110, cell = bar), `Main issue` = colDef(minWidth = 210,
                   style = function(v) list(color = if (v == "No major issue") BRAND$muted else "#8a1c1c", fontWeight = 600)))
    cn <- c("u", "Unit", if (lvl == "facility") "District", "Score", "Main issue", "Completeness", "Timeliness", "Consistency",
            "Non-outlier", if (lvl != "facility") c("Facilities", "Not reporting"), "Flags", "Outliers")
    reactable(x[, ..cn], columns = cols, compact = TRUE, searchable = TRUE, pagination = TRUE, defaultPageSize = 20, highlight = TRUE)
  })

  output$ct_trend <- renderPlotly({
    a <- area(); f <- facs()
    r <- REP[dataset == input$ds & uid %in% f$uid, .(e = sum(expected), a = sum(actual), t = sum(on_time)), by = period][order(period)]
    if (!nrow(r)) return(empty_plot())
    plot_ly(r, x = ~ym_date(period)) |>
      add_lines(y = ~100 * a / e, name = "Completeness", line = list(color = BRAND$navy, width = 2.5),
                hovertemplate = "Completeness %{x|%b %Y}: %{y:.0f}%<extra></extra>") |>
      add_lines(y = ~100 * t / e, name = "Timeliness", line = list(color = BRAND$maroon, width = 2),
                hovertemplate = "Timeliness %{x|%b %Y}: %{y:.0f}%<extra></extra>") |>
      plotly_base(ytitle = "% of expected reports") |>
      layout(yaxis = list(range = c(0, 105)), hovermode = "x unified",
             shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 80, y1 = 80,
                                line = list(color = BRAND$muted, dash = "dot", width = 1))),
             annotations = list(list(x = 1, xref = "paper", y = 80, text = "80%", showarrow = FALSE, xanchor = "right",
                                     yanchor = "bottom", font = list(size = 10, color = BRAND$muted))))
  })
  output$ct_ds <- renderPlotly({
    p <- per(); f <- facs()
    r <- REP[period >= p$from & period <= p$to & uid %in% f$uid, .(e = sum(expected), a = sum(actual), t = sum(on_time)), by = dataset]
    r <- r[e > 0][, `:=`(name = unlist(META$datasets)[dataset], comp = 100 * a / e, time = 100 * t / e)][order(comp)]
    plot_ly(r, y = ~factor(name, levels = name)) |>
      add_bars(x = ~comp, name = "Completeness", marker = list(color = BRAND$navy), orientation = "h",
               hovertemplate = "%{y}: %{x:.0f}% complete<extra></extra>") |>
      add_bars(x = ~time, name = "Timeliness", marker = list(color = BRAND$maroon), orientation = "h",
               hovertemplate = "%{y}: %{x:.0f}% on time<extra></extra>") |>
      plotly_base() |> layout(barmode = "group", bargap = .3, xaxis = list(range = c(0, 100), title = "%"), yaxis = list(title = ""))
  })

  output$heat <- renderPlotly({
    p <- per(); f <- facs()
    if (nrow(f) > 160) return(empty_plot("Pick a district or DLG to see facility-level reporting (too many facilities for all of Busoga)."))
    r <- REP[dataset == input$ds & uid %in% f$uid & period >= p$from & period <= p$to]
    if (!nrow(r)) return(empty_plot())
    r[, code := fifelse(expected == 0, 0, fifelse(on_time > 0, 3, fifelse(actual > 0, 2, 1)))]
    w <- dcast(r, uid ~ period, value.var = "code", fill = 0)
    w[, name := ou_name[uid]]; w <- w[order(name)]
    z <- as.matrix(w[, !c("uid", "name")])
    lab <- matrix(c("Not expected", "Missing", "Late", "On time")[z + 1], nrow = nrow(z))
    plot_ly(x = ym_date(as.integer(colnames(z))), y = w$name, z = z, type = "heatmap", zmin = 0, zmax = 3, text = lab,
            colorscale = list(c(0, "#e8e7e2"), c(.249, "#e8e7e2"), c(.25, "#d03b3b"), c(.499, "#d03b3b"),
                              c(.5, "#fab219"), c(.749, "#fab219"), c(.75, "#0ca30c"), c(1, "#0ca30c")),
            showscale = FALSE, xgap = 1, ygap = 1, hovertemplate = "%{y}<br>%{x|%b %Y}: %{text}<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(yaxis = list(autorange = "reversed", tickfont = list(size = 9), title = ""))
  })

  nr <- reactive({
    req(DQF); f <- facs()
    merge(DQF[uid %in% f$uid & report_status != "Regular reporter"], OU[, .(uid, name, district, dlg, subcounty, grp_level, grp_ownership)], by = "uid")
  })
  output$nr_bar <- renderPlotly({
    x <- nr()[, .N, by = .(district, report_status)]
    if (!nrow(x)) return(empty_plot("Every facility reports regularly"))
    p <- plot_ly()
    for (s in intersect(names(REPORT_STATUS), unique(x$report_status)))
      p <- p |> add_bars(data = x[report_status == s], y = ~district, x = ~N, name = s, orientation = "h",
                         marker = list(color = REPORT_STATUS[[s]], line = list(color = "white", width = 1)))
    p |> plotly_base() |> layout(barmode = "stack", yaxis = list(title = "", categoryorder = "total ascending"), xaxis = list(title = "Facilities"))
  })
  output$nr_tbl <- renderDT({
    x <- nr()[, .(Facility = name, Status = report_status, `Last report` = ifelse(is.na(last_report), "never", fmt_month(last_report)),
                  `Completeness 12 m` = round(completeness_12m), Level = grp_level, Ownership = grp_ownership,
                  `Sub-county` = subcounty, District = district)][order(Status, District, Facility)]
    datatable(x, rownames = FALSE, filter = "top", extensions = "Buttons",
              options = list(pageLength = 12, dom = "Bfrtip", buttons = c("csv"), scrollX = TRUE))
  })

  chk <- reactive({ req(DQC); p <- per(); DQC[uid %in% facs()$uid & period >= p$from & period <= p$to] })
  output$chk_bar <- renderPlotly({
    x <- chk()[, .N, by = check][order(N)]
    if (!nrow(x)) return(empty_plot("No consistency flags in this selection"))
    plot_ly(x, y = ~factor(check, levels = check), x = ~N, type = "bar", orientation = "h", marker = list(color = STATUS$critical),
            hovertemplate = "%{y}: %{x:,} facility-months<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(yaxis = list(title = "", tickfont = list(size = 10)), xaxis = list(title = "Facility-months flagged"))
  })
  output$chk_tbl <- renderDT({
    x <- chk()[order(-period)][, .(Facility = ou_name[uid], District = OU$district[match(uid, OU$uid)], Month = fmt_month(period),
                                   Check = check, Detail = detail)]
    datatable(x, rownames = FALSE, filter = "top", extensions = "Buttons",
              options = list(pageLength = 12, dom = "Bfrtip", buttons = c("csv"), scrollX = TRUE))
  })
  output$out_tbl <- renderDT({
    req(DQO); p <- per()
    x <- DQO[uid %in% facs()$uid & period >= p$from & period <= p$to][order(-abs(z))][
      , .(Facility = ou_name[uid], District = OU$district[match(uid, OU$uid)], Month = fmt_month(period), Item = item,
          Value = value, `Typical month (median)` = median, `Modified z` = round(z, 1))]
    datatable(x, rownames = FALSE, filter = "top", extensions = "Buttons",
              options = list(pageLength = 15, dom = "Bfrtip", buttons = c("csv"), scrollX = TRUE))
  })
})
