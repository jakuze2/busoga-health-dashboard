# Compare & trends: units side by side over time, league table, and theme small multiples.

compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Compare & trends", "Compare areas and facilities over time",
              "Choose a level, pick up to eight units and watch them against the Busoga line. The league table ranks every unit at that level."),
    filter_bar(
      selectInput(ns("ind"), "Indicator", indicator_choices(), selected = "DEL01", width = "330px"),
      selectInput(ns("level"), "Level", c("Districts / cities" = "district", "DLGs / municipalities" = "dlg",
                                          "Sub-counties / divisions" = "subcounty", "Health facilities" = "facility"),
                  width = "200px"),
      selectizeInput(ns("units"), "Units (up to 8)", choices = NULL, multiple = TRUE, width = "420px",
                     options = list(maxItems = 8, placeholder = "Top four by default")),
      radioButtons(ns("by"), "Time step", c(Month = "month", Quarter = "quarter", Year = "year"), selected = "quarter", inline = TRUE),
      period_ui(ns("period"), selected = "all")),
    layout_columns(col_widths = c(8, 4),
      card(full_screen = TRUE, card_header(textOutput(ns("t_title"), inline = TRUE)), plotlyOutput(ns("trend"), height = 430),
           info_note("The latest year may be partial; the latest month may still be receiving late reports.")),
      card(full_screen = TRUE, card_header("League table", span(class = "sub", textOutput(ns("l_sub"), inline = TRUE))),
           reactableOutput(ns("league")))),
    card(full_screen = TRUE,
         card_header("Whole programme for one unit", span(class = "sub", "every indicator in the theme, unit vs Busoga")),
         div(style = "display:flex;gap:1rem;flex-wrap:wrap;padding:.4rem .2rem 0",
             selectInput(ns("sm_theme"), NULL, THEMES$theme, width = "260px"),
             selectizeInput(ns("sm_unit"), NULL, choices = NULL, width = "340px")),
         plotlyOutput(ns("small"), height = 520))
  )
}

compare_server <- function(id) moduleServer(id, function(input, output, session) {
  per <- period_server("period")
  all_units <- c(setNames(META$region_uid, "Busoga (region)"),
                 setNames(OU[level_name != "region", uid], sprintf("%s · %s", OU[level_name != "region", name],
                                                                   LEVEL_LABEL[OU[level_name != "region", level_name]])))
  updateSelectizeInput(session, "sm_unit", choices = all_units, selected = META$region_uid, server = TRUE)

  observeEvent(list(input$level, input$ind), {
    if (input$level == "facility" && IND[code == input$ind, area_only]) {
      updateSelectInput(session, "level", selected = "subcounty"); return()
    }
    u <- units_at(input$level)
    p <- isolate(per())
    s <- summarise_ind(input$ind, input$level, p$from, p$to)
    top <- if (nrow(s)) s[order(-value)][seq_len(min(4, .N)), uid] else head(unname(u), 4)
    updateSelectizeInput(session, "units", choices = u, selected = top, server = TRUE)
  })
  trend_data <- reactive({
    req(input$units); p <- per()
    list(s = summarise_ind(input$ind, input$level, p$from, p$to, uids = input$units, by = input$by),
         r = summarise_ind(input$ind, "region", p$from, p$to, by = input$by), p = p)
  })
  output$t_title <- renderText(sprintf("%s · by %s", ind_label(input$ind), input$by))
  output$trend <- renderPlotly({
    d <- trend_data(); if (!nrow(d$s)) return(empty_plot())
    p <- plot_ly() |>
      add_lines(data = d$r[order(bucket)], x = ~bucket_date(bucket, input$by), y = ~value, name = "Busoga",
                line = list(color = BRAND$ink2, width = 2, dash = "dot"),
                hovertemplate = "Busoga: %{y:,.1f}<extra></extra>")
    for (k in seq_along(input$units)) {
      u <- input$units[k]; x <- d$s[uid == u][order(bucket)]
      if (!nrow(x)) next
      p <- p |> add_lines(data = x, x = ~bucket_date(bucket, input$by), y = ~value, name = ou_name[[u]],
                          line = list(color = SERIES[k], width = 2), marker = list(size = 8),
                          hovertemplate = paste0(htmlEscape(ou_name[[u]]), ": %{y:,.1f}<extra></extra>"))
    }
    p |> plotly_base(ytitle = unit_label(input$ind)) |> layout(hovermode = "x unified")
  })
  output$l_sub <- renderText(per()$label)
  output$league <- renderReactable({
    p <- per(); s <- summarise_ind(input$ind, input$level, p$from, p$to)
    validate(need(nrow(s), "No data."))
    dirn <- IND[code == input$ind, direction]
    s <- s[!is.na(value)][order(if (dirn == "low") value else -value)]
    s[, `:=`(Rank = seq_len(.N), Unit = ou_name[uid])]
    ref <- region_value(input$ind, p$from, p$to)[[input$ind]] %||% NA
    mx <- max(abs(s$value), na.rm = TRUE)
    col <- theme_col(IND[code == input$ind, theme])
    reactable(s[, .(Rank, Unit, value, n_rep)], compact = TRUE, searchable = TRUE, pagination = TRUE, defaultPageSize = 15,
      columns = list(Rank = colDef(width = 52),
        Unit = colDef(minWidth = 150),
        n_rep = colDef(name = "Months", width = 66, align = "right"),
        value = colDef(name = unit_label(input$ind), minWidth = 150, cell = function(v) {
          w <- if (is.na(v) || mx == 0) 0 else 100 * abs(v) / mx
          div(style = "display:flex;align-items:center;gap:.4rem",
              div(style = sprintf("flex:1;height:8px;background:#f0efec;border-radius:4px;position:relative"),
                  div(style = sprintf("width:%.0f%%;height:8px;background:%s;border-radius:4px", w, col))),
              span(style = "min-width:4.5em;text-align:right;font-variant-numeric:tabular-nums", fmt_val(v, input$ind)))
        })),
      rowStyle = function(i) if (!is.na(ref) && status_vs(s$value[i], ref, dirn) == "critical") list(color = "#8a1c1c") else NULL)
  })
  output$small <- renderPlotly({
    req(input$sm_unit); u <- input$sm_unit
    lvl <- if (u == META$region_uid) "region" else ou_level[[u]]
    codes <- IND[theme == input$sm_theme, code]
    if (lvl == "facility") codes <- IND[code %in% codes & area_only == FALSE, code]
    s <- summarise_ind(codes, lvl, MONTH_MIN, MONTH_MAX, uids = u, by = "quarter")
    r <- summarise_ind(codes, "region", MONTH_MIN, MONTH_MAX, by = "quarter")
    codes <- intersect(codes, unique(s$code))
    if (!length(codes)) return(empty_plot())
    col <- theme_col(input$sm_theme)
    plots <- lapply(codes, function(cd) {
      plot_ly() |>
        add_lines(data = r[code == cd][order(bucket)], x = ~bucket_date(bucket, "quarter"), y = ~value,
                  line = list(color = "#b9b7b0", width = 1.5, dash = "dot"), showlegend = FALSE,
                  hovertemplate = "Busoga: %{y:,.1f}<extra></extra>") |>
        add_lines(data = s[code == cd][order(bucket)], x = ~bucket_date(bucket, "quarter"), y = ~value,
                  line = list(color = col, width = 2), showlegend = FALSE,
                  hovertemplate = paste0(htmlEscape(ou_name[[u]]), ": %{y:,.1f}<extra></extra>")) |>
        layout(annotations = list(list(text = sprintf("<b>%s</b>", htmlEscape(ind_label(cd))), x = 0, y = 1.12,
                                       xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left",
                                       font = list(size = 11, color = BRAND$ink))),
               yaxis = list(rangemode = "tozero", gridcolor = BRAND$grid, tickfont = list(size = 9, color = BRAND$muted)),
               xaxis = list(tickfont = list(size = 9, color = BRAND$muted)))
    })
    nc <- 4
    subplot(plots, nrows = ceiling(length(plots) / nc), margin = c(0.03, 0.03, 0.07, 0.05), shareX = FALSE) |>
      layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", margin = list(t = 30, l = 10, r = 10),
             font = list(family = "Arial, Helvetica, sans-serif")) |>
      config(displaylogo = FALSE)
  })
})
