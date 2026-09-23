# Indicator explorer: bundle up to 10 indicators and compare them across facilities, sub-counties,
# DLGs, districts or Busoga, or across facility groups (ownership, level, authority).

explorer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Analyse", "Indicator explorer",
              "Bundle up to 10 indicators and compare them across any level, from single facilities to Busoga, or across facility ownership, level and authority. Opened from an Overview tile, it starts with that indicator and area.", key = "explorer"),
    filter_bar(
      selectizeInput(ns("codes"), "Indicators (up to 10)", indicator_choices(), selected = c("ANC03", "DEL01"), multiple = TRUE,
                     width = "460px", options = list(maxItems = 10, plugins = list("remove_button"))),
      selectInput(ns("mode"), "Compare", c("Areas or facilities" = "units", "Facility ownership" = "ownership",
                                           "Facility level" = "level", "Facility authority" = "authority"), width = "190px"),
      conditionalPanel(sprintf("input['%s'] == 'units'", ns("mode")),
        selectInput(ns("level"), "Level", c("Districts / cities" = "district", "DLGs / municipalities" = "dlg",
                                            "Sub-counties / divisions" = "subcounty", "Health facilities" = "facility",
                                            "Busoga only" = "region"), selected = "district", width = "200px")),
      selectInput(ns("within"), "Within", c("All Busoga" = META$region_uid, units_at("district")), width = "190px"),
      conditionalPanel(sprintf("input['%s'] == 'units' && input['%s'] == 'facility'", ns("mode"), ns("level")),
        selectInput(ns("f_level"), "Facility level", c("All", "Hospital", "HC IV", "HC III", "HC II", "Clinic / other"), width = "150px"),
        selectInput(ns("f_own"), "Ownership", c("All", "GOV", "PNFP", "PFP"), width = "120px")),
      conditionalPanel(sprintf("input['%s'] == 'units' && input['%s'] != 'region'", ns("mode"), ns("level")),
        selectizeInput(ns("units"), "Units to compare (up to 12)", NULL, multiple = TRUE, width = "420px",
                       options = list(maxItems = 12, plugins = list("remove_button"), placeholder = "Largest by default"))),
      radioButtons(ns("by"), "Time step", c(Month = "month", Quarter = "quarter", Year = "year"), selected = "quarter", inline = TRUE),
      period_ui(ns("period"), selected = "all")),
    navset_card_underline(full_screen = TRUE,
      nav_panel("Trends", uiOutput(ns("trend_note")), uiOutput(ns("trends_box"))),
      nav_panel("Comparison table", uiOutput(ns("tbl_note")), reactableOutput(ns("tbl"))),
      nav_panel("Rankings",
        div(style = "padding:.4rem 0", selectInput(ns("rank_code"), NULL, NULL, width = "360px")),
        plotlyOutput(ns("rank"), height = 620)),
      nav_panel("Heatmap", plotlyOutput(ns("heat"), height = 560),
        info_note("Each cell compares the unit with Busoga for that indicator: darker green is better than Busoga, darker red is worse (direction-aware); grey means no fixed direction or no data.")))
  )
}

explorer_server <- function(id, incoming = NULL) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  per <- period_server("period")
  pending_units <- reactiveVal(NULL)

  # arrive from the Overview: indicator(s), area
  if (!is.null(incoming)) observeEvent(incoming(), {
    x <- incoming(); req(x)
    updateSelectizeInput(session, "codes", selected = x$codes)
    updateSelectInput(session, "mode", selected = "units")
    if (x$level %in% c("region")) {
      updateSelectInput(session, "within", selected = META$region_uid); updateSelectInput(session, "level", selected = "district")
    } else if (x$level == "facility") {
      updateSelectInput(session, "within", selected = OU$uid_l3[OU$uid == x$uid]); updateSelectInput(session, "level", selected = "facility")
      pending_units(x$uid)
    } else {
      d <- if (x$level == "district") x$uid else OU$uid_l3[OU$uid == x$uid]
      updateSelectInput(session, "within", selected = d)
      updateSelectInput(session, "level", selected = child_level(x$level) %||% "facility")
    }
  })

  fac_pool <- reactive({
    f <- OU[level_name == "facility"]
    if (input$within != META$region_uid) f <- f[uid_l3 == input$within | uid_l4 == input$within | uid_l5 == input$within]
    fl <- fcase(f$grp_level %in% c("RRH", "General Hospital"), "Hospital", f$grp_level %in% c("HC IV", "HC III", "HC II"), f$grp_level,
                default = "Clinic / other")
    if (input$f_level != "All") f <- f[fl == input$f_level]
    if (input$f_own != "All") f <- f[grp_ownership %in% input$f_own]
    f
  })
  pool <- reactive({
    lvl <- input$level
    if (lvl == "facility") { f <- fac_pool(); return(setNames(f$uid, f$name)[order(f$name)]) }
    if (lvl == "region") return(setNames(META$region_uid, "Busoga"))
    units_at(lvl, if (input$within == META$region_uid) NULL else input$within)
  })
  observeEvent(list(input$level, input$within, input$f_level, input$f_own, input$codes), {
    u <- pool(); req(length(u))
    pu <- pending_units(); pending_units(NULL)
    sel <- if (!is.null(pu) && all(pu %in% u)) pu else {
      cd <- input$codes[1] %||% "DEL01"
      s <- summarise_ind(cd, input$level, DEFAULT_FROM, DEFAULT_TO, uids = unname(u))
      if (nrow(s) && length(u) > 8) s[order(-num)][seq_len(min(8, .N)), uid] else head(unname(u), 12)
    }
    updateSelectizeInput(session, "units", choices = u, selected = sel, server = TRUE)
  }, ignoreInit = FALSE)
  observeEvent(input$codes, updateSelectInput(session, "rank_code", choices = setNames(input$codes, ind_label(input$codes))))

  codes_ok <- reactive({
    req(input$codes); cd <- input$codes
    if (input$mode == "units" && input$level == "facility" || input$mode != "units") cd <- IND[code %in% cd & area_only == FALSE, code]
    cd
  })
  # long table: code, key (unit or group), label, bucket, value
  data <- reactive({
    p <- per(); cd <- codes_ok(); validate(need(length(cd), "Pick at least one indicator (population-based coverage indicators have no facility-level values)."))
    if (input$mode == "units") {
      u <- if (input$level == "region") META$region_uid else input$units
      validate(need(length(u), "Pick units to compare."))
      s <- summarise_ind(cd, input$level, p$from, p$to, uids = u, by = input$by)
      s[, `:=`(key = uid, label = ou_name[uid])]
      tot <- summarise_ind(cd, input$level, p$from, p$to, uids = u)[, `:=`(key = uid, label = ou_name[uid])]
    } else {
      w <- if (input$within == META$region_uid) NULL else input$within
      s <- summarise_groups(cd, input$mode, w, p$from, p$to, by = input$by); s[, `:=`(key = grp, label = grp)]
      tot <- summarise_groups(cd, input$mode, w, p$from, p$to); tot[, `:=`(key = grp, label = grp)]
    }
    ref <- summarise_ind(cd, "region", p$from, p$to, by = input$by)
    ref_tot <- region_value(cd, p$from, p$to)
    list(s = s, tot = tot, ref = ref, ref_tot = ref_tot, cd = cd, p = p)
  })

  output$trend_note <- renderUI({
    d <- data(); dropped <- setdiff(input$codes, d$cd); ended <- d$cd[!is.na(END_OF[d$cd])]
    tagList(
      if (length(dropped)) info_note(sprintf("Left out at this level (population-based, areas only): %s.", paste(ind_label(dropped), collapse = "; "))),
      if (length(ended)) info_note(paste(sprintf("%s: %s.", ind_label(ended), ended_note(ended)), collapse = " ")))
  })
  # the chart container is sized from the number of panels (an "auto" height stays at zero and never renders)
  output$trends_box <- renderUI({
    n <- length(codes_ok()); nc <- if (n <= 2) n else if (n <= 4) 2 else 3
    plotlyOutput(ns("trends"), height = sprintf("%dpx", 300 * ceiling(max(n, 1) / max(nc, 1)) + 60))
  })
  output$trends <- renderPlotly({
    d <- data(); cd <- d$cd; keys <- unique(d$tot[order(-num)]$key)
    cols <- setNames(rep(SERIES, length.out = length(keys)), keys)
    nc <- if (length(cd) <= 2) length(cd) else if (length(cd) <= 4) 2 else 3
    plots <- lapply(seq_along(cd), function(i) {
      c1 <- cd[i]; p <- plot_ly()
      r <- d$ref[code == c1][order(bucket)]
      if (IND[code == c1, unit] == "count" && input$mode == "units" && input$level != "region") r <- r[0]   # a Busoga total would dwarf the units
      if (nrow(r)) p <- p |> add_lines(data = r, x = ~bucket_date(bucket, input$by), y = ~value, name = "Busoga", legendgroup = "Busoga",
                                       showlegend = i == 1, line = list(color = "#8a8883", width = 1.6, dash = "dot"),
                                       hovertemplate = "Busoga: %{y:,.1f}<extra></extra>")
      for (k in keys) {
        x <- d$s[code == c1 & key == k][order(bucket)]; if (!nrow(x)) next
        p <- p |> add_lines(data = x, x = ~bucket_date(bucket, input$by), y = ~value, name = x$label[1], legendgroup = k,
                            showlegend = i == 1, line = list(color = cols[[k]], width = 2.2),
                            hovertemplate = paste0(htmlEscape(x$label[1]), ": %{y:,.1f}<extra></extra>"))
      }
      p |> layout(annotations = list(list(text = sprintf("<b>%s</b> <span style='color:#898781'>(%s)</span>", htmlEscape(ind_label(c1)), unit_label(c1)),
                                          x = 0, y = 1.13, xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "left",
                                          font = list(size = 12, color = theme_col(as.character(IND[code == c1, theme]))))),
                  yaxis = list(rangemode = "tozero", gridcolor = BRAND$grid, tickfont = list(size = 10, color = BRAND$muted)),
                  xaxis = list(tickfont = list(size = 10, color = BRAND$muted)))
    })
    nr <- ceiling(length(cd) / nc)
    subplot(plots, nrows = nr, margin = c(0.035, 0.035, 0.09, 0.05), shareX = FALSE) |>
      layout(hovermode = "x unified", legend = list(orientation = "h", y = -0.06 / nr),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", margin = list(t = 40, l = 10, r = 10),
             font = list(family = "Arial, Helvetica, sans-serif")) |> config(displaylogo = FALSE)
  })

  output$tbl_note <- renderUI(info_note(sprintf("%s. First row: Busoga. Cells are rated against Busoga for the same period.", data()$p$label)))
  output$tbl <- renderReactable({
    d <- data(); cd <- d$cd
    w <- dcast(d$tot[, .(key, label, code, value)], key + label ~ code, value.var = "value")
    ref_row <- as.data.table(c(list(key = "busoga", label = "BUSOGA"), as.list(d$ref_tot[cd])))
    w <- rbind(ref_row, w, fill = TRUE)
    cols <- lapply(intersect(cd, names(w)), function(c1) {
      dirn <- IND[code == c1, direction]; ref <- d$ref_tot[[c1]] %||% NA
      colDef(name = ind_label(c1), minWidth = 120, align = "right",
             header = function(v) div(style = sprintf("border-top:3px solid %s;padding-top:3px;white-space:normal;line-height:1.15", theme_col(as.character(IND[code == c1, theme]))), v),
             cell = function(value, index) {
               if (index == 1) return(div(class = "sc-cell", style = "background:#f0efec;font-weight:700;justify-content:flex-end", fmt_val(value, c1)))
               st <- status_vs(value, ref, dirn)
               div(class = "sc-cell", style = sprintf("background:%s", if (st == "none") "transparent" else paste0(STATUS[[st]], "30")),
                   span(class = "ic", style = sprintf("color:%s", if (st == "none") BRAND$muted else STATUS[[st]]), STATUS_ICON[[st]]),
                   fmt_val(value, c1))
             })
    })
    names(cols) <- intersect(cd, names(w))
    reactable(w, compact = TRUE, highlight = TRUE, pagination = FALSE, searchable = nrow(w) > 12,
              columns = c(list(key = colDef(show = FALSE), label = colDef(name = "Unit / group", sticky = "left", minWidth = 200,
                                                                         style = list(fontWeight = 600))), cols))
  })

  output$rank <- renderPlotly({
    req(input$rank_code); p <- per(); c1 <- input$rank_code
    if (input$mode == "units") {
      lvl <- if (input$level == "region") "district" else input$level
      if (lvl == "facility" && IND[code == c1, area_only]) return(empty_plot("Population-based indicator: no facility values"))
      allu <- if (lvl == "facility") fac_pool()$uid else unname(units_at(lvl, if (input$within == META$region_uid) NULL else input$within))
      s <- summarise_ind(c1, lvl, p$from, p$to, uids = allu)[!is.na(value)]; s[, label := ou_name[uid]]
      sel <- input$units
    } else {
      w <- if (input$within == META$region_uid) NULL else input$within
      s <- summarise_groups(c1, input$mode, w, p$from, p$to)[!is.na(value)]; s[, `:=`(uid = grp, label = grp)]; sel <- character()
    }
    if (!nrow(s)) return(empty_plot())
    dirn <- IND[code == c1, direction]
    s <- s[order(if (dirn == "low") -value else value)]
    if (nrow(s) > 60) s <- s[unique(c(seq_len(25), which(uid %in% sel), (.N - 24):.N))]
    col <- theme_col(as.character(IND[code == c1, theme]))
    s[, fill := fifelse(uid %in% sel, BRAND$maroon, col)]
    ref <- region_value(c1, p$from, p$to)[[c1]] %||% NA
    plot_ly(s, x = ~value, y = ~factor(label, levels = label), type = "bar", orientation = "h", marker = list(color = ~fill),
            hovertemplate = "%{y}: %{x:,.1f}<extra></extra>") |>
      plotly_base(legend = FALSE) |>
      layout(yaxis = list(title = "", tickfont = list(size = 9)), xaxis = list(title = unit_label(c1)),
             shapes = if (!is.na(ref)) list(list(type = "line", x0 = ref, x1 = ref, y0 = 0, y1 = 1, yref = "paper",
                                                 line = list(color = BRAND$navy, dash = "dash"))),
             annotations = if (!is.na(ref)) list(list(x = ref, y = 1.01, yref = "paper", text = "Busoga", showarrow = FALSE,
                                                      font = list(color = BRAND$navy, size = 10))))
  })

  output$heat <- renderPlotly({
    d <- data(); cd <- d$cd
    t <- d$tot[, .(label, code, value)]
    t[, ref := d$ref_tot[code]]
    t[, dirn := IND$direction[match(code, IND$code)]]
    t[, score := fifelse(dirn == "neutral" | is.na(value) | is.na(ref) | ref == 0, NA_real_,
                         pmax(-1, pmin(1, (value - ref) / abs(ref) * fifelse(dirn == "low", -1, 1))))]
    t[, txt := fmt_val(value, code)]
    zw <- dcast(t, label ~ code, value.var = "score"); tw <- dcast(t, label ~ code, value.var = "txt")
    cols_ok <- intersect(cd, names(zw))
    plot_ly(x = ind_label(cols_ok), y = zw$label, z = as.matrix(zw[, ..cols_ok]), text = as.matrix(tw[, ..cols_ok]),
            type = "heatmap", zmin = -1, zmax = 1, texttemplate = "%{text}", textfont = list(size = 10),
            colorscale = list(c(0, "#b3261e"), c(.5, "#f4f3ef"), c(1, "#1b7a3a")), showscale = FALSE, xgap = 2, ygap = 2,
            hovertemplate = "%{y}<br>%{x}: %{text}<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(xaxis = list(side = "top", tickangle = -20, title = ""), yaxis = list(autorange = "reversed", title = ""))
  })
})
