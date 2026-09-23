# Targets: which indicators are below the national (or, failing that, global) target, for any
# area, and which districts / cities are below target for each indicator.

targets_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Analyse", "Performance against targets",
              "Every indicator that has a Uganda national target (or, where there is none, a global target) compared with that target, for the selected area and period, and for each district and city."),
    filter_bar(area_ui(ns("area")), period_ui(ns("period")),
               checkboxInput(ns("only_below"), "Only indicators below target", FALSE)),
    uiOutput(ns("summary")),
    card(full_screen = TRUE, card_header(textOutput(ns("t_title"), inline = TRUE),
                                         span(class = "sub", "hover a target for its source")),
         reactableOutput(ns("tbl")),
         downloadButton(ns("dl_area"), "Download table (CSV)", class = "btn-sm btn-outline-secondary mt-2 align-self-start")),
    card(full_screen = TRUE,
         card_header(textOutput(ns("m_title"), inline = TRUE),
                     span(class = "sub", "red = below target, green = target met; values for the selected period")),
         reactableOutput(ns("matrix")),
         downloadButton(ns("dl_matrix"), "Download district matrix (CSV)", class = "btn-sm btn-outline-secondary mt-2 align-self-start")),
    info_note(TARGET_NOTE, " ", CAP_NOTE, " Population-based coverages (‡) use projected populations and can be unreliable for small areas.")
  )
}

targets_server <- function(id) moduleServer(id, function(input, output, session) {
  area <- area_server("area"); per <- period_server("period")
  codes <- TGT$code[is.na(END_OF[TGT$code])]

  area_tbl <- reactive({
    a <- area(); p <- per()
    s <- summarise_ind(codes, a$level, p$from, p$to, uids = a$uid)
    t <- TGT[match(codes, TGT$code)]
    d <- data.table(code = codes, theme = as.character(IND$theme[match(codes, IND$code)]),
                    indicator = ind_label(codes), target = target_text(codes), basis = t$basis, source = t$source,
                    note = t$note)
    d <- merge(d, s[, .(code, value, value_raw, capped)], by = "code", all.x = TRUE, sort = FALSE)
    d[, status := target_status(value_raw, code, p$n)]
    d[, gap := { tv <- target_value_line(code); fifelse(is.na(tv) | is.na(value_raw), NA_real_, target_basis_value(value_raw, code, p$n) - tv) }]
    d[, theme := factor(theme, THEMES$theme)]
    setorder(d, theme, code)
    d[]
  })

  output$summary <- renderUI({
    d <- area_tbl(); a <- area(); p <- per()
    n <- sum(!is.na(d$status)); b <- sum(d$status == "below", na.rm = TRUE)
    below <- d[status == "below"]
    div(class = "hero hero-targets",
        div(div(class = "h-lab", a$name), div(class = "h-val", sprintf("%d of %d", b, n)),
            div(class = "h-sub", sprintf("indicators below target, %s", p$label))),
        div(div(class = "h-lab", "Target met"), div(class = "h-val", sum(d$status == "met", na.rm = TRUE)),
            div(class = "h-sub", "indicators at or better than target")),
        div(div(class = "h-lab", "Furthest below target"),
            div(class = "h-val h-val-sm", if (nrow(below)) {
              below[, rel := abs(gap) / pmax(abs(target_value_line(code)), 1)]
              below[order(-rel)][1, indicator] } else "–"),
            div(class = "h-sub", if (nrow(below)) { z <- below[order(-rel)][1]
              sprintf("%s vs target %s", fmt_val(z$value, z$code), z$target) } else "")))
  })

  output$t_title <- renderText(sprintf("%s, %s", area()$name, per()$label))
  output$tbl <- renderReactable({
    d <- area_tbl(); if (isTRUE(input$only_below)) d <- d[status == "below"]
    d[, shown := paste0(fmt_val(value, code), fifelse(capped %in% TRUE, "*", ""))]
    reactable(d[, .(theme = as.character(theme), indicator, shown, target, basis, status, source, note)],
      pagination = FALSE, compact = TRUE, highlight = TRUE, defaultSorted = NULL,
      columns = list(
        theme = colDef(name = "Theme", minWidth = 130),
        indicator = colDef(name = "Indicator", minWidth = 230),
        shown = colDef(name = "Value", align = "right", minWidth = 80),
        target = colDef(name = "Target", align = "right", minWidth = 80,
                        cell = function(value, index) span(title = d$source[index], value)),
        basis = colDef(name = "Basis", minWidth = 80),
        status = colDef(name = "Status", minWidth = 110, cell = function(value) {
          if (is.na(value)) span(class = "tgt-chip none", "no data")
          else if (value == "met") span(class = "tgt-chip met", "Target met")
          else span(class = "tgt-chip below", "Below target") }),
        source = colDef(show = FALSE), note = colDef(show = FALSE)))
  })

  matrix_dt <- reactive({
    p <- per()
    s <- summarise_ind(codes, "district", p$from, p$to)
    r <- summarise_ind(codes, "region", p$from, p$to)[, uid := "Busoga"]
    s <- rbind(r, s[, names(r), with = FALSE])
    s[, `:=`(area = fifelse(uid == "Busoga", "Busoga", ou_name[uid]), status = target_status(value_raw, code, p$n))]
    s
  })
  output$m_title <- renderText(sprintf("Districts and cities against target, %s", per()$label))
  output$matrix <- renderReactable({
    s <- matrix_dt(); if (!nrow(s)) return(NULL)
    keep <- if (isTRUE(input$only_below)) unique(s[status == "below", code]) else unique(s$code)
    s <- s[code %in% keep]
    areas <- c("Busoga", sort(setdiff(unique(s$area), "Busoga")))
    w <- dcast(s, code ~ area, value.var = "value")
    st <- dcast(s, code ~ area, value.var = "status")
    w <- w[match(intersect(TGT$code, w$code), w$code)]; st <- st[match(w$code, st$code)]
    w[, `:=`(indicator = ind_label(code), target = target_text(code), below_n = rowSums(st[, setdiff(areas, "Busoga"), with = FALSE] == "below", na.rm = TRUE))]
    cols <- lapply(areas, function(a) colDef(name = a, align = "right", minWidth = 78,
      cell = function(value, index) fmt_val(value, w$code[index]),
      style = function(value, index) { x <- st[[a]][index]
        if (is.na(x)) list(color = BRAND$muted) else if (x == "below") list(background = "#fbe3e1", color = "#8c1d18", fontWeight = 600)
        else list(background = "#e3f3e6", color = "#1b5e20") }))
    names(cols) <- areas
    reactable(w[, c("indicator", "target", "below_n", areas), with = FALSE], pagination = FALSE, compact = TRUE,
      bordered = TRUE, highlight = TRUE,
      columns = c(list(indicator = colDef(name = "Indicator", minWidth = 220, sticky = "left"),
                       target = colDef(name = "Target", minWidth = 75, align = "right", sticky = "left"),
                       below_n = colDef(name = "Districts below", minWidth = 85, align = "right")), cols))
  })

  output$dl_area <- downloadHandler(
    filename = function() sprintf("targets_%s_%s.csv", gsub("[^A-Za-z0-9]+", "_", area()$name), format(Sys.Date(), "%Y%m%d")),
    content = function(f) {
      d <- area_tbl()
      writeLines(sprintf("# Busoga Health Forum Dashboard - %s, %s. %s", area()$name, per()$label, TARGET_NOTE), f)
      fwrite(d[, .(theme, indicator_code = code, indicator, value = round(value_raw, 2), shown_capped_at_100 = capped,
                   target, basis, status, gap_to_target = round(gap, 2), source, note)], f, append = TRUE, col.names = TRUE)
    })
  output$dl_matrix <- downloadHandler(
    filename = function() sprintf("targets_by_district_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(f) {
      s <- matrix_dt()
      writeLines(sprintf("# Busoga Health Forum Dashboard - districts and cities against target, %s. %s", per()$label, TARGET_NOTE), f)
      fwrite(s[, .(area, indicator_code = code, indicator = ind_label(code), value = round(value_raw, 2),
                   target = target_text(code), status)][order(indicator_code, area)], f, append = TRUE, col.names = TRUE)
    })
})
