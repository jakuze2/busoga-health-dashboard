# Scorecard: units x indicators, each cell rated against the Busoga value for the same period.

scorecard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Scorecards", "Performance scorecard",
              "Every district, DLG, sub-county or facility against every indicator, rated against the Busoga-wide value for the same period. No external targets are invented: the reference is Busoga itself.", key = "scorecard"),
    filter_bar(
      selectInput(ns("level"), "Compare", c("Districts / cities" = "district", "DLGs / municipalities" = "dlg",
                                            "Sub-counties / divisions" = "subcounty", "Health facilities" = "facility"),
                  selected = "district", width = "210px"),
      selectInput(ns("within"), "Within", c("All Busoga" = META$region_uid, units_at("district")), width = "200px"),
      selectInput(ns("themes"), "Programme themes", THEMES$theme, selected = THEMES$theme[1:3], multiple = TRUE, width = "380px"),
      period_ui(ns("period")),
      selectInput(ns("band"), "\"Close to Busoga\" band", c("± 5%" = 0.05, "± 10%" = 0.10, "± 20%" = 0.20),
                  selected = 0.10, width = "150px")),
    div(class = "status-legend",
        span(span(class = "status-dot", style = sprintf("background:%s", STATUS$good)), STATUS_ICON[["good"]], STATUS_LABEL[["good"]]),
        span(span(class = "status-dot", style = sprintf("background:%s", STATUS$warning)), STATUS_ICON[["warning"]], STATUS_LABEL[["warning"]]),
        span(span(class = "status-dot", style = sprintf("background:%s", STATUS$critical)), STATUS_ICON[["critical"]], STATUS_LABEL[["critical"]]),
        span(span(class = "status-dot", style = sprintf("background:%s", STATUS$none)), "Neutral indicator or no data")),
    card(full_screen = TRUE,
         card_header(textOutput(ns("title"), inline = TRUE),
                     span(class = "float-end", downloadButton(ns("dl"), "Download CSV", class = "btn-sm btn-outline-secondary"))),
         reactableOutput(ns("tbl"))),
    info_note("Direction matters: for indicators where lower is better (stillbirths, dropouts, test positivity, deaths) a value below Busoga is rated better. Population-based coverage indicators have no facility-level value.")
  )
}

scorecard_server <- function(id) moduleServer(id, function(input, output, session) {
  per <- period_server("period")
  observeEvent(input$level, {
    ch <- switch(input$level, district = c("All Busoga" = META$region_uid),
                 c("All Busoga" = META$region_uid, units_at("district")))
    updateSelectInput(session, "within", choices = ch)
  })
  data <- reactive({
    req(input$themes); p <- per()
    codes <- IND[theme %in% input$themes, code]
    if (input$level == "facility") codes <- IND[code %in% codes & area_only == FALSE, code]
    uids <- units_at(input$level, if (input$within == META$region_uid) NULL else input$within)
    s <- summarise_ind(codes, input$level, p$from, p$to, uids = uids)
    ref <- region_value(codes, p$from, p$to)
    list(s = s, ref = ref, codes = codes, uids = uids, p = p)
  })
  wide <- reactive({
    d <- data()
    w <- dcast(d$s[, .(uid, code, value)], uid ~ code, value.var = "value")
    w <- merge(data.table(uid = unname(d$uids), Unit = names(d$uids)), w, by = "uid", all.x = TRUE)
    if (input$level != "district") w[, District := OU$district[match(uid, OU$uid)]]
    w
  })
  output$title <- renderText({
    d <- data()
    sprintf("%d %s × %d indicators · %s", length(d$uids), LEVEL_PLURAL[[input$level]], length(d$codes), d$p$label)
  })
  output$tbl <- renderReactable({
    d <- data(); w <- copy(wide()); band <- as.numeric(input$band)
    codes <- intersect(d$codes, names(w))
    validate(need(length(codes), "No data for this selection."))
    ref_row <- as.data.table(c(list(uid = META$region_uid, Unit = "BUSOGA (reference)"), as.list(d$ref[codes])))
    if ("District" %in% names(w)) ref_row[, District := ""]
    w <- rbind(ref_row, w, fill = TRUE)
    cols <- lapply(codes, function(cd) {
      dirn <- IND[code == cd, direction]; ref <- d$ref[[cd]] %||% NA
      colDef(name = ind_label(cd), minWidth = 118, align = "right",
        header = function(value) div(title = value, style = sprintf("border-top:3px solid %s;padding-top:3px;white-space:normal;line-height:1.15;", theme_col(IND[code == cd, theme])), value),
        cell = function(value, index) {
          if (index == 1) return(div(class = "sc-cell", style = "background:#f0efec;font-weight:700;", span(), fmt_val(value, cd)))
          st <- status_vs(value, ref, dirn, band)
          bg <- if (st == "none") "transparent" else paste0(STATUS[[st]], "33")
          div(class = "sc-cell", style = sprintf("background:%s;", bg),
              span(class = "ic", style = sprintf("color:%s", if (st == "none") BRAND$muted else STATUS[[st]]), STATUS_ICON[[st]]),
              fmt_val(value, cd))
        })
    })
    names(cols) <- codes
    base <- list(uid = colDef(show = FALSE),
                 Unit = colDef(name = LEVEL_LABEL[[input$level]], sticky = "left", minWidth = 220,
                               style = list(fontWeight = 600, borderRight = "1px solid #e1e0d9")))
    if ("District" %in% names(w)) base$District <- colDef(minWidth = 120, sticky = "left", style = list(color = BRAND$ink2))
    # theme column groups
    groups <- lapply(intersect(THEMES$theme, IND[code %in% codes, as.character(theme)]), function(th)
      colGroup(name = th, columns = IND[code %in% codes & theme == th, code],
               headerStyle = list(background = paste0(theme_col(th), "22"), borderBottom = sprintf("3px solid %s", theme_col(th)))))
    setcolorder(w, c("uid", "Unit", intersect("District", names(w)), codes))
    reactable(w, columns = c(base, cols), columnGroups = groups, searchable = TRUE, pagination = nrow(w) > 60,
              defaultPageSize = 60, compact = TRUE, highlight = TRUE, bordered = FALSE, striped = FALSE,
              defaultSorted = "Unit", rowStyle = function(i) if (i == 1) list(position = "sticky", top = 0) else NULL,
              theme = reactableTheme(headerStyle = list(fontSize = "0.74rem", color = BRAND$ink2),
                                     cellPadding = "4px 6px"), height = 640)
  })
  output$dl <- downloadHandler(
    filename = function() sprintf("busoga_scorecard_%s_%s.csv", input$level, format(Sys.Date(), "%Y%m%d")),
    content = function(f) {
      w <- copy(wide()); d <- data()
      setnames(w, d$codes[d$codes %in% names(w)], ind_label(d$codes[d$codes %in% names(w)]))
      fwrite(w, f)
    })
})
