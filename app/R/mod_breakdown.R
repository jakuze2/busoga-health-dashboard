# Breakdowns: by facility ownership, level and authority, and by age and sex.

breakdown_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Breakdowns", "Who delivers the services, and to whom",
              "Split any indicator by facility ownership, level or authority inside any area, and see age and sex breakdowns for the main service counts."),
    navset_card_underline(full_screen = TRUE,
      nav_panel("By facility group",
        filter_bar(selectInput(ns("ind"), "Indicator", indicator_choices(facility = TRUE), selected = "DEL01", width = "320px"),
                   selectInput(ns("set"), "Group facilities by", c(Ownership = "ownership", `Facility level` = "level", Authority = "authority"), width = "180px"),
                   area_ui(ns("area"), depth = 2), period_ui(ns("period"))),
        layout_columns(col_widths = c(5, 7),
          plotlyOutput(ns("grp_bar"), height = 400), plotlyOutput(ns("grp_trend"), height = 400)),
        h6(class = "mt-3", "Every indicator in the theme, by group"),
        plotlyOutput(ns("grp_heat"), height = 420),
        info_note("Groups come from the DHIS2 organisation unit group sets. For counts, bars show totals; for rates, the pooled rate of the group.")),
      nav_panel("By age and sex",
        filter_bar(selectInput(ns("item"), "Service", NULL, width = "340px"),
                   area_ui(ns("area2"), depth = 2),
                   selectInput(ns("year"), "Year", rev(unique(MONTHS %/% 100L)), width = "120px")),
        layout_columns(col_widths = c(7, 5),
          plotlyOutput(ns("pyramid"), height = 460), plotlyOutput(ns("age_trend"), height = 460)),
        info_note("Age bands and sex are the category option combos of the DHIS2 data element; 'Total' where the element has no sex split.")))
  )
}

breakdown_server <- function(id) moduleServer(id, function(input, output, session) {
  per <- period_server("period"); area <- area_server("area"); area2 <- area_server("area2")
  if (!is.null(BRK)) updateSelectInput(session, "item", choices = sort(unique(BRK$item)))

  grp <- reactive({
    p <- per(); a <- area()
    summarise_groups(input$ind, input$set, a$uid, p$from, p$to)
  })
  output$grp_bar <- renderPlotly({
    s <- grp(); if (!nrow(s)) return(empty_plot())
    s <- s[order(value)]
    col <- theme_col(IND[code == input$ind, theme])
    plot_ly(s, x = ~value, y = ~factor(grp, levels = grp), type = "bar", orientation = "h",
            marker = list(color = col), text = ~sprintf("%s facilities", n_fac), textposition = "none",
            hovertemplate = "%{y}: %{x:,.1f}<br>%{text}<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(xaxis = list(title = unit_label(input$ind)), yaxis = list(title = NULL))
  })
  output$grp_trend <- renderPlotly({
    a <- area(); s <- summarise_groups(input$ind, input$set, a$uid, MONTH_MIN, MONTH_MAX, by = "quarter")
    if (!nrow(s)) return(empty_plot())
    top <- s[, .(v = sum(num, na.rm = TRUE)), by = grp][order(-v)][seq_len(min(.N, 3)), grp]
    s[!grp %in% top, grp := "Other"]
    if ("Other" %in% s$grp) s <- s[, .(num = sum(num, na.rm = TRUE), den_sum = sum(den_sum, na.rm = TRUE),
                                       den_mean = mean(den_mean, na.rm = TRUE), n_months = n_months[1]), by = .(code, grp, bucket)][
      , value := ind_value(code, num, den_sum, den_mean, n_months)]
    lv <- c(top, if ("Other" %in% s$grp) "Other")
    p <- plot_ly()
    for (k in seq_along(lv)) p <- p |> add_lines(data = s[grp == lv[k]][order(bucket)], x = ~bucket_date(bucket, "quarter"), y = ~value,
                                                 name = lv[k], line = list(color = if (lv[k] == "Other") "#9b9a94" else SERIES[k], width = 2),
                                                 hovertemplate = paste0(lv[k], ": %{y:,.1f}<extra></extra>"))
    p |> plotly_base(ytitle = unit_label(input$ind)) |> layout(hovermode = "x unified")
  })
  output$grp_heat <- renderPlotly({
    p <- per(); a <- area(); th <- IND[code == input$ind, theme]
    codes <- IND[theme == th & area_only == FALSE, code]
    s <- summarise_groups(codes, input$set, a$uid, p$from, p$to)
    if (!nrow(s)) return(empty_plot())
    # colour = position within each indicator's own range (rows are not on a common scale)
    lab <- fmt_val(s$value, s$code)
    s[, lab := lab]
    w <- dcast(s, code ~ grp, value.var = "value"); wl <- dcast(s[, .(code, grp, lab)], code ~ grp, value.var = "lab")
    zz <- as.matrix(w[, -1]); z_scaled <- t(apply(zz, 1, function(r) { r <- as.numeric(r); if (all(is.na(r)) || diff(range(r, na.rm = TRUE)) == 0) r * 0 else (r - min(r, na.rm = TRUE)) / diff(range(r, na.rm = TRUE)) }))
    plot_ly(x = colnames(zz), y = ind_label(w$code), z = z_scaled, type = "heatmap", text = as.matrix(wl[, -1]),
            texttemplate = "%{text}", hovertemplate = "%{y}<br>%{x}: %{text}<extra></extra>", showscale = FALSE,
            colorscale = list(c(0, "#f4f3ef"), c(1, theme_col(th))), xgap = 2, ygap = 2) |>
      plotly_base(legend = FALSE) |> layout(yaxis = list(autorange = "reversed", title = NULL), xaxis = list(title = NULL, side = "top"),
                                            margin = list(l = 10))
  })

  brk <- reactive({
    req(BRK, input$item); a <- area2()
    x <- BRK[item == input$item & uid == a$uid]
    x
  })
  output$pyramid <- renderPlotly({
    x <- brk()[year == as.integer(input$year)]
    if (!nrow(x)) return(empty_plot())
    x <- x[, .(value = sum(value)), by = .(age, age_order, sex)][order(age_order)]
    lv <- unique(x$age)
    if (all(x$sex %in% c("Male", "Female"))) {
      plot_ly() |>
        add_bars(data = x[sex == "Male"], y = ~factor(age, levels = lv), x = ~-value, name = "Male", orientation = "h",
                 marker = list(color = "#2a78d6"), customdata = ~value, hovertemplate = "Male %{y}: %{customdata:,.0f}<extra></extra>") |>
        add_bars(data = x[sex == "Female"], y = ~factor(age, levels = lv), x = ~value, name = "Female", orientation = "h",
                 marker = list(color = "#eb6834"), hovertemplate = "Female %{y}: %{x:,.0f}<extra></extra>") |>
        plotly_base() |> layout(barmode = "relative", bargap = .15, yaxis = list(title = NULL),
                                xaxis = list(title = "Number", tickformat = "~s",
                                             tickvals = pretty(c(-max(x$value), max(x$value))),
                                             ticktext = formatC(abs(pretty(c(-max(x$value), max(x$value)))), format = "d", big.mark = ",")))
    } else {
      plot_ly(x, y = ~factor(age, levels = lv), x = ~value, type = "bar", orientation = "h", marker = list(color = BRAND$navy),
              hovertemplate = "%{y}: %{x:,.0f}<extra></extra>") |> plotly_base(legend = FALSE) |> layout(yaxis = list(title = NULL))
    }
  })
  output$age_trend <- renderPlotly({
    x <- brk(); if (!nrow(x)) return(empty_plot())
    x <- x[, .(value = sum(value)), by = .(year, age, age_order)]
    x[, share := 100 * value / sum(value), by = year]
    lv <- x[order(age_order), unique(age)]
    ramp <- colorRampPalette(c("#cde2fb", "#2a78d6", "#0d366b"))(length(lv))
    p <- plot_ly()
    for (k in seq_along(lv)) p <- p |> add_bars(data = x[age == lv[k]], x = ~factor(year), y = ~share, name = lv[k],
                                                marker = list(color = ramp[k], line = list(color = "white", width = 1)),
                                                hovertemplate = paste0(lv[k], ": %{y:.1f}%<extra></extra>"))
    p |> plotly_base(ytitle = "% of the year's total") |> layout(barmode = "stack", legend = list(orientation = "v", y = 0.5, x = 1.02))
  })
})
