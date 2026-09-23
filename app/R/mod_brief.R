# Brief: a short, downloadable summary (Word or PDF) for any area, period and set of themes.
# Word is written as Markdown and converted with pandoc; PDF is drawn with grid graphics, so no
# extra packages or LaTeX are needed.

brief_facts <- function(a, p) {
  facs <- OU[level_name == "facility"]
  if (a$level != "region") facs <- facs[uid_l3 == a$uid | uid_l4 == a$uid | uid_l5 == a$uid]
  if (a$level == "facility") facs <- OU[uid == a$uid]
  r <- REP[dataset == "RtEYsASU7PG" & period >= p$from & period <= p$to & uid %in% facs$uid]
  popv <- POP[uid == a$uid & year == p$to %/% 100L, pop]
  data.table(item = c("Projected population", "Health facilities", "Facilities reporting in the period",
                      "Reporting completeness (105:01 OPD)", "Reports on time"),
             value = c(if (length(popv)) formatC(popv, big.mark = ",", format = "d") else "-",
                       formatC(nrow(facs), big.mark = ","), formatC(uniqueN(r[actual > 0, uid]), big.mark = ","),
                       if (sum(r$expected)) sprintf("%.0f%%", 100 * sum(r$actual) / sum(r$expected)) else "-",
                       if (sum(r$expected)) sprintf("%.0f%%", 100 * sum(r$on_time) / sum(r$expected)) else "-"))
}

brief_data <- function(a, p, themes) {
  codes <- IND[theme %in% themes & is.na(ended) & (a$level != "facility" | area_only == FALSE), code]
  cur <- summarise_ind(codes, a$level, p$from, p$to, uids = a$uid)
  pw <- previous_window(p$from, p$to)
  prev <- if (!is.null(pw)) summarise_ind(codes, a$level, pw[1], pw[2], uids = a$uid) else data.table(code = character(), value = numeric())
  d <- data.table(code = codes)
  d[, `:=`(theme = as.character(IND$theme[match(code, IND$code)]), indicator = ind_label(code),
           direction = IND$direction[match(code, IND$code)], unit = IND$unit[match(code, IND$code)])]
  d[, value := cur$value[match(code, cur$code)]][, value_raw := cur$value_raw[match(code, cur$code)]]
  d[, capped := cur$capped[match(code, cur$code)] %in% TRUE][, prev := prev$value[match(code, prev$code)]]
  if (!is.null(pw)) d[SERIES_START[code] > pw[1], prev := NA_real_]      # series that did not exist for the whole comparison period
  d[, change := fifelse(is.na(value) | is.na(prev), NA_real_,
                        fifelse(unit == "%", value - prev, fifelse(prev != 0, 100 * (value - prev) / abs(prev), NA_real_)))]
  d[, better := fifelse(direction == "high", change, fifelse(direction == "low", -change, NA_real_))]
  d[, `:=`(target = target_text(code), basis = TGT$basis[match(code, TGT$code)], status = target_status(value_raw, code, p$n))]
  d[, gap := { tv <- target_value_line(code); fifelse(is.na(tv) | is.na(value_raw), NA_real_, target_basis_value(value_raw, code, p$n) - tv) }]
  d <- d[!is.na(value)]
  d[, theme := factor(theme, THEMES$theme)]; setorder(d, theme, code)
  # districts below target (only when the brief is for Busoga)
  dist <- NULL
  if (a$level == "region") {
    s <- summarise_ind(intersect(codes, TGT$code), "district", p$from, p$to)
    s[, status := target_status(value_raw, code, p$n)]
    dist <- s[status == "below", .(n_below = .N, districts = paste(sort(ou_name[uid]), collapse = ", ")), by = code]
    dist[, indicator := ind_label(code)]
  }
  s_from <- max(MONTH_MIN, date_ym(seq(ym_date(p$to), by = "-23 months", length.out = 2)[2]))
  pick <- head(c(d[status == "below"][order(-abs(gap) / pmax(abs(target_value_line(code)), 1)), code],
                 d[is.na(status) & direction != "neutral"][order(better), code]), 4)
  tr <- if (length(pick)) summarise_ind(pick, a$level, s_from, p$to, uids = a$uid, by = "month") else data.table()
  list(area = a, period = p, facts = brief_facts(a, p), ind = d, dist = dist, trend = tr, pick = pick,
       prev_label = if (!is.null(pw)) period_caption(pw[1], pw[2]) else NA_character_)
}

brief_sentences <- function(b) {
  d <- b$ind; tg <- d[!is.na(status)]
  out <- character()
  if (nrow(tg)) out <- c(out, sprintf("%d of the %d indicators with a target are below target in %s (%s).",
                                      sum(tg$status == "below"), nrow(tg), b$area$name, b$period$label))
  imp <- d[!is.na(better) & better > 0][order(-better)]
  wor <- d[!is.na(better) & better < 0][order(better)]
  chg <- function(z) ifelse(z$unit == "%", sprintf("%+.1f points", z$change), sprintf("%+.0f%%", z$change))
  if (nrow(imp)) out <- c(out, sprintf("Largest improvements compared with %s: %s.", b$prev_label,
                                       paste(sprintf("%s (%s)", head(imp$indicator, 3), head(chg(imp), 3)), collapse = "; ")))
  if (nrow(wor)) out <- c(out, sprintf("Largest deteriorations: %s.",
                                       paste(sprintf("%s (%s)", head(wor$indicator, 3), head(chg(wor), 3)), collapse = "; ")))
  if (any(d$capped)) out <- c(out, "Coverages above 100% are shown as 100% (*); this usually means the population denominator is under-estimated.")
  out
}

brief_chart <- function(b, cd, file) {
  tr <- b$trend[code == cd][order(bucket)]
  if (nrow(tr) < 2) return(FALSE)
  tr[, date := ym_date(bucket)]
  tv <- target_value_line(cd, monthly = TRUE)
  g <- ggplot2::ggplot(tr, ggplot2::aes(date, value)) +
    ggplot2::geom_line(colour = theme_col(as.character(IND$theme[IND$code == cd])), linewidth = 0.9) +
    ggplot2::geom_point(size = 1.2, colour = theme_col(as.character(IND$theme[IND$code == cd]))) +
    { if (is.finite(tv)) list(ggplot2::geom_hline(yintercept = tv, colour = "#b3261e", linetype = "dashed"),
                              ggplot2::annotate("text", x = min(tr$date), y = tv, label = paste("Target", target_text(cd)),
                                                hjust = 0, vjust = -0.5, size = 3, colour = "#b3261e")) } +
    ggplot2::labs(title = ind_label(cd), subtitle = sprintf("%s, monthly (%s)", b$area$name, unit_label(cd)),
                  x = NULL, y = unit_label(cd), caption = "Source: Uganda MoH DHIS2 (HMIS). Busoga Health Forum Dashboard.") +
    ggplot2::expand_limits(y = 0) + ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", colour = BRAND$navy),
                   plot.caption = ggplot2::element_text(colour = BRAND$muted, size = 7), panel.grid.minor = ggplot2::element_blank())
  ggplot2::ggsave(file, g, width = 6.5, height = 3, dpi = 160, bg = "white")
  TRUE
}

md_table <- function(df) {
  esc <- function(x) gsub("|", "\\|", as.character(x), fixed = TRUE)
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(esc(r), collapse = " | "), " |")), "")
}

brief_rows <- function(d) {
  data.table(Indicator = d$indicator,
             Value = paste0(fmt_val(d$value, d$code), fifelse(d$capped, "*", "")),
             Previous = fmt_val(d$prev, d$code),
             Change = fifelse(is.na(d$change), "-", fifelse(d$unit == "%", sprintf("%+.1f pts", d$change), sprintf("%+.0f%%", d$change))),
             Target = fifelse(is.na(d$target), "-", d$target),
             Status = fifelse(is.na(d$status), "-", fifelse(d$status == "met", "Met", "Below")))
}

write_brief_docx <- function(b, file) {
  dir <- tempfile("brief"); dir.create(dir)
  md <- c(sprintf("# Health brief: %s", b$area$name), "",
          sprintf("**Period:** %s  ", b$period$label),
          sprintf("**Generated:** %s from the Busoga Health Forum Dashboard. Data: Uganda MoH DHIS2 (hmis.health.go.ug), extracted %s.", format(Sys.Date(), "%d %B %Y"), META$extracted), "",
          "## Key facts", "", md_table(b$facts[, .(Item = item, Value = value)]),
          "## Summary", "", paste("-", brief_sentences(b)), "")
  below <- b$ind[status == "below"]
  if (nrow(below)) md <- c(md, "## Indicators below target", "",
                           md_table(brief_rows(below)[, .(Indicator, Value, Target)]))
  if (!is.null(b$dist) && nrow(b$dist)) md <- c(md, "## Districts and cities below target", "",
                                                md_table(b$dist[order(-n_below), .(Indicator = indicator, `Number below` = n_below, `Districts / cities` = districts)]))
  for (th in levels(droplevels(b$ind$theme))) md <- c(md, sprintf("## %s", th), "", md_table(brief_rows(b$ind[theme == th])))
  if (length(b$pick)) {
    md <- c(md, "## Trends", "")
    for (cd in b$pick) { f <- file.path(dir, paste0(cd, ".png")); if (brief_chart(b, cd, f)) md <- c(md, sprintf("![%s](%s){width=6.5in}", ind_label(cd), basename(f)), "") }
  }
  md <- c(md, "## Notes", "", paste("-", c(TARGET_NOTE, CAP_NOTE,
          "Values follow DHIS2 rules: rates are sum(numerator) / sum(denominator); population-based coverages are annualised.",
          "Change is in percentage points for percentages and in percent for other measures, compared with the previous period of the same length.")))
  mdf <- file.path(dir, "brief.md"); writeLines(md, mdf, useBytes = TRUE)
  owd <- setwd(dir); on.exit(setwd(owd), add = TRUE)
  rmarkdown::pandoc_convert("brief.md", to = "docx", from = "markdown", output = file.path(dir, "brief.docx"))
  file.copy(file.path(dir, "brief.docx"), file, overwrite = TRUE)
}

# ---- PDF (grid graphics) ----
write_brief_pdf <- function(b, file) {
  dev <- if (capabilities("cairo")) function(f) grDevices::cairo_pdf(f, width = 8.27, height = 11.69, onefile = TRUE)
         else function(f) grDevices::pdf(f, width = 8.27, height = 11.69, encoding = "ISOLatin1")
  txt <- if (capabilities("cairo")) identity else function(x) chartr("≥≤–", "><-", x)
  dev(file); on.exit(grDevices::dev.off(), add = TRUE)
  L <- 0.7; R <- 7.57; y <- 0
  newpage <- function() { grid::grid.newpage(); y <<- 11.0
    grid::grid.text(txt(sprintf("Busoga Health Forum Dashboard · %s · %s", b$area$name, b$period$label)), grid::unit(L, "in"), grid::unit(11.35, "in"),
                    just = "left", gp = grid::gpar(fontsize = 7.5, col = BRAND$muted)) }
  need <- function(h) if (y - h < 0.7) newpage()
  line <- function(s, size = 10, bold = FALSE, col = BRAND$ink, gap = 0.2, wrap = 105) {
    for (w in strwrap(txt(s), width = wrap)) { need(gap)
      grid::grid.text(w, grid::unit(L, "in"), grid::unit(y, "in"), just = c("left", "top"),
                      gp = grid::gpar(fontsize = size, fontface = if (bold) "bold" else "plain", col = col)); y <<- y - gap } }
  heading <- function(s) { need(0.6); y <<- y - 0.12; line(s, 13, TRUE, BRAND$navy, 0.3) }
  table <- function(df, widths) {
    x0 <- L + c(0, cumsum(widths))[seq_along(widths)]; rh <- 0.22
    row <- function(vals, head = FALSE, shade = FALSE) { need(rh)
      if (shade || head) grid::grid.rect(grid::unit(L, "in"), grid::unit(y, "in"), grid::unit(R - L, "in"), grid::unit(rh, "in"),
                                         just = c("left", "top"), gp = grid::gpar(fill = if (head) BRAND$navy_soft else "#f7f6f3", col = NA))
      for (k in seq_along(vals)) {
        v <- txt(as.character(vals[k])); mx <- floor(widths[k] * 15.5)
        if (nchar(v) > mx) v <- paste0(substr(v, 1, mx - 1), "…")
        grid::grid.text(v, grid::unit(x0[k] + 0.05, "in"), grid::unit(y - rh / 2, "in"), just = "left",
                        gp = grid::gpar(fontsize = 8.5, fontface = if (head) "bold" else "plain",
                                        col = if (!head && identical(vals[k], "Below")) "#b3261e" else BRAND$ink)) }
      y <<- y - rh }
    row(names(df), head = TRUE)
    for (i in seq_len(nrow(df))) row(unlist(df[i]), shade = i %% 2 == 0)
    y <<- y - 0.12 }

  newpage()
  line(sprintf("Health brief: %s", b$area$name), 20, TRUE, BRAND$navy, 0.42)
  line(sprintf("Period: %s", b$period$label), 11, FALSE, BRAND$ink2, 0.24)
  line(sprintf("Generated %s from the Busoga Health Forum Dashboard. Data: Uganda MoH DHIS2 (hmis.health.go.ug), extracted %s.",
               format(Sys.Date(), "%d %B %Y"), META$extracted), 8.5, FALSE, BRAND$muted, 0.18); y <- y - 0.1
  heading("Key facts"); table(b$facts[, .(Item = item, Value = value)], c(3.5, 3.37))
  heading("Summary"); for (s in brief_sentences(b)) line(paste("•", s), 10, gap = 0.2)
  below <- b$ind[status == "below"]
  if (nrow(below)) { heading("Indicators below target"); table(brief_rows(below)[, .(Indicator, Value, Target)], c(4.4, 1.2, 1.27)) }
  if (!is.null(b$dist) && nrow(b$dist)) { heading("Districts and cities below target")
    table(b$dist[order(-n_below), .(Indicator = indicator, `Below` = n_below, `Districts / cities` = districts)], c(2.6, 0.6, 3.67)) }
  for (th in levels(droplevels(b$ind$theme))) { heading(th); table(brief_rows(b$ind[theme == th]), c(2.75, 0.85, 0.95, 0.8, 0.8, 0.72)) }
  if (length(b$pick)) {
    heading("Trends")
    for (cd in b$pick) { f <- tempfile(fileext = ".png"); if (!brief_chart(b, cd, f)) next
      need(3.1); img <- png::readPNG(f)
      grid::grid.raster(img, grid::unit(L, "in"), grid::unit(y, "in"), width = grid::unit(6.5, "in"), height = grid::unit(3, "in"), just = c("left", "top"))
      y <- y - 3.15 }
  }
  heading("Notes")
  for (s in c(TARGET_NOTE, CAP_NOTE, "Values follow DHIS2 rules: rates are sum(numerator) / sum(denominator); population-based coverages are annualised."))
    line(paste("•", s), 8.5, gap = 0.17, col = BRAND$ink2, wrap = 125)
  invisible(file)
}

brief_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Reports", "Download a brief",
              "A short summary for any area and period: key facts, indicators below target, changes since the previous period, district results and trend charts. Choose the area, period and programmes, then download it as Word or PDF."),
    filter_bar(area_ui(ns("area"), depth = 4), period_ui(ns("period"))),
    layout_columns(col_widths = c(4, 8),
      card(card_header("Brief settings"),
           selectInput(ns("themes"), "Programme themes", THEMES$theme, selected = THEMES$theme, multiple = TRUE),
           radioButtons(ns("fmt"), "Format", c("Word (.docx)" = "docx", "PDF" = "pdf"), inline = TRUE),
           downloadButton(ns("dl"), "Download brief", class = "btn-primary w-100 mt-2"),
           info_note("The brief uses exactly what is selected on this page. Word files can be edited before sharing.")),
      card(full_screen = TRUE, card_header("Preview", span(class = "sub", textOutput(ns("sub"), inline = TRUE))),
           uiOutput(ns("preview"))))
  )
}

brief_server <- function(id) moduleServer(id, function(input, output, session) {
  area <- area_server("area"); per <- period_server("period")
  b <- reactive({ req(length(input$themes) > 0); brief_data(area(), per(), input$themes) })
  output$sub <- renderText(sprintf("%s · %s", area()$name, per()$label))
  output$preview <- renderUI({
    x <- b(); below <- x$ind[status == "below"]
    tagList(
      h4(sprintf("Health brief: %s", x$area$name)), p(class = "muted", x$period$label),
      tags$ul(lapply(brief_sentences(x), tags$li)),
      if (nrow(below)) tagList(h5("Indicators below target"),
        tags$table(class = "table table-sm", tags$thead(tags$tr(tags$th("Indicator"), tags$th("Value"), tags$th("Target"))),
          tags$tbody(lapply(seq_len(nrow(below)), function(i) tags$tr(tags$td(below$indicator[i]),
            tags$td(fmt_val(below$value[i], below$code[i])), tags$td(below$target[i])))))),
      p(class = "muted", sprintf("The download adds key facts, a table for each of the %d selected themes%s and up to %d trend charts.",
                                 length(input$themes), if (!is.null(x$dist)) ", districts below target" else "", length(x$pick))))
  })
  output$dl <- downloadHandler(
    filename = function() sprintf("BHF_brief_%s_%s.%s", gsub("[^A-Za-z0-9]+", "_", area()$name), format(Sys.Date(), "%Y%m%d"), input$fmt),
    content = function(file) {
      x <- b()
      withProgress(message = "Preparing the brief", value = 0.3, {
        if (input$fmt == "docx") write_brief_docx(x, file) else write_brief_pdf(x, file)
      })
    })
})
