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

md_table <- function(df, widths = NULL) {
  esc <- function(x) gsub("|", "\\|", as.character(x), fixed = TRUE)
  # pipe-table dashes set relative column widths in Word (pandoc uses them when a row is long)
  dash <- if (is.null(widths)) rep("---", ncol(df)) else strrep("-", pmax(3, round(72 * widths / sum(widths))))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(dash, collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(esc(r), collapse = " | "), " |")), "")
}

short_area <- function(x) gsub(" District$", "", gsub(" District,", ",", x))

brief_rows <- function(d) {
  data.table(Indicator = d$indicator,
             Value = paste0(fmt_val(d$value, d$code), fifelse(d$capped, "*", "")),
             Previous = fmt_val(d$prev, d$code),
             Change = fifelse(is.na(d$change), "-", fifelse(d$unit == "%", sprintf("%+.1f pts", d$change), sprintf("%+.0f%%", d$change))),
             Target = fifelse(is.na(d$target), "-", d$target),
             Status = fifelse(is.na(d$status), "-", fifelse(d$status == "met", "Met", "Below")))
}

# ---- shared pieces for the Word and PDF briefs (at most 3 pages) ------------------------------
MORT_CODES <- c("DEL07", "MCM01", "MCM02", "MCM03", "MCM04", "MCM06", "MCM05")

# the indicator summary table: each theme's headline indicators, plus anything below target
brief_summary_rows <- function(b) {
  hl <- unlist(HEADLINE[intersect(names(HEADLINE), as.character(unique(b$ind$theme)))])
  d <- b$ind[code %in% c(hl, b$ind[status == "below", code]) & !code %in% MORT_CODES]
  d[order(theme, code)]
}

brief_mortality <- function(b) b$ind[code %in% MORT_CODES][match(intersect(MORT_CODES, code), code)]

chg_text <- function(d) fifelse(is.na(d$change), "no comparison",
                               fifelse(d$unit == "%", sprintf("%+.1f pts", d$change), sprintf("%+.0f%%", d$change)))

brief_plot_theme <- function(base = 8.5) ggplot2::theme_minimal(base_size = base, base_family = "sans") +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(colour = BRAND$ink2), plot.title = ggplot2::element_blank(),
                 legend.position = "bottom", legend.title = ggplot2::element_blank(), plot.margin = ggplot2::margin(2, 6, 2, 2))

# targets met / below, by theme
brief_score_plot <- function(b) {
  d <- b$ind[!is.na(status), .N, by = .(theme = as.character(theme), status)]
  if (!nrow(d)) return(NULL)
  d[, status := factor(fifelse(status == "met", "Target met", "Below target"), c("Below target", "Target met"))]
  d[, theme := factor(theme, rev(intersect(THEMES$theme, unique(theme))))]
  ggplot2::ggplot(d, ggplot2::aes(N, theme, fill = status)) +
    ggplot2::geom_col(width = 0.62, colour = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = N), position = ggplot2::position_stack(vjust = 0.5), colour = "white", size = 2.7, fontface = "bold") +
    ggplot2::scale_fill_manual(values = c("Below target" = "#b3261e", "Target met" = "#1b7a3a"), breaks = c("Target met", "Below target")) +
    ggplot2::scale_x_continuous(breaks = function(l) seq(0, ceiling(l[2]), by = 1), expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(x = "Number of indicators with a target", y = NULL) + brief_plot_theme(8) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "#eeede8"), axis.text.x = ggplot2::element_text(size = 6.5))
}

# how far each below-target indicator is from its target (achievement, % of target)
brief_gap_plot <- function(b) {
  d <- b$ind[status == "below"]
  if (!nrow(d)) return(NULL)
  d[, tv := target_value_line(code)]
  d[, basis_v := mapply(target_basis_value, value_raw, code, b$period$n)]
  d <- d[is.finite(tv) & tv > 0]
  if (!nrow(d)) return(NULL)
  d[, ach := fifelse(direction == "low", 100 * tv / pmax(basis_v, 1e-9), 100 * basis_v / tv)]
  d[, ach := pmin(pmax(ach, 0), 100)]
  d[, lab := sprintf("%s (target %s)", fmt_val(value, code), target)]
  d[, ind := factor(indicator, rev(indicator[order(ach)]))]
  d[, col := theme_col(as.character(theme))]
  ggplot2::ggplot(d, ggplot2::aes(ach, ind)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = 100, yend = ind), colour = "#eeede8", linewidth = 3.2, lineend = "round") +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = ach, yend = ind, colour = col), linewidth = 3.2, lineend = "round") +
    ggplot2::geom_text(ggplot2::aes(x = 101, label = lab), hjust = 0, size = 2.5, colour = BRAND$ink2) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(limits = c(0, 160), breaks = c(0, 50, 100), labels = c("0%", "50%", "target"),
                                expand = ggplot2::expansion(mult = 0)) +
    ggplot2::labs(x = "Progress towards the target (100% = target reached)", y = NULL) + brief_plot_theme(8) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7.2, colour = BRAND$ink))
}

# up to four trends, two by two
brief_trend_plot <- function(b) {
  if (!length(b$pick) || !nrow(b$trend)) return(NULL)
  tr <- b$trend[code %in% b$pick & is.finite(value)][order(code, bucket)]
  if (!nrow(tr)) return(NULL)
  tr[, date := ym_date(bucket)]
  tr[, panel := factor(sprintf("%s (%s)", ind_label(code), unit_label(code)), sprintf("%s (%s)", ind_label(b$pick), unit_label(b$pick)))]
  tr[, col := theme_col(as.character(IND$theme[match(code, IND$code)]))]
  tg <- unique(tr[, .(code, panel)])[, tv := vapply(code, target_value_line, 0, monthly = TRUE)][is.finite(tv)]
  ggplot2::ggplot(tr, ggplot2::aes(date, value)) +
    ggplot2::geom_area(ggplot2::aes(fill = col), alpha = 0.12) +
    ggplot2::geom_line(ggplot2::aes(colour = col), linewidth = 0.7) +
    ggplot2::geom_point(data = tr[, .SD[.N], by = panel], ggplot2::aes(colour = col), size = 1.3) +
    { if (nrow(tg)) ggplot2::geom_hline(data = tg, ggplot2::aes(yintercept = tv), colour = "#b3261e", linetype = "dashed", linewidth = 0.4) } +
    ggplot2::scale_colour_identity() + ggplot2::scale_fill_identity() +
    ggplot2::scale_x_date(date_labels = "%b %y", breaks = scales::breaks_pretty(4)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(accuracy = 1), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::facet_wrap(~panel, ncol = 2, scales = "free_y", labeller = ggplot2::label_wrap_gen(46)) +
    ggplot2::labs(x = NULL, y = NULL) + brief_plot_theme(7.5) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0, colour = BRAND$navy, size = 7.5),
                   panel.grid.major.y = ggplot2::element_line(colour = "#eeede8"), panel.spacing = grid::unit(0.5, "cm"))
}

BRIEF_NOTES <- c(
  "Values follow DHIS2 rules: rates are the sum of the numerator over the sum of the denominator; population-based coverages are annualised.",
  "Change is in percentage points for percentages and in percent for other measures, compared with the previous period of the same length.",
  "Mortality figures are facility-based (deaths reported by health facilities), not population rates; a few implausible single-month entries are left out.")

# ---- Word ------------------------------------------------------------------------------------
write_brief_docx <- function(b, file) {
  dir <- tempfile("brief"); dir.create(dir)
  png_of <- function(g, name, w, h) { if (is.null(g)) return(NULL); f <- file.path(dir, name)
    ggplot2::ggsave(f, g, width = w, height = h, dpi = 200, bg = "white"); name }
  pagebreak <- c("", "```{=openxml}", '<w:p><w:r><w:br w:type="page"/></w:r></w:p>', "```", "")
  small <- function(x) c("::: {custom-style=\"Source Note\"}", x, ":::", "")
  file.copy("www/bhf_logo.png", file.path(dir, "logo.png"))
  f <- b$facts; tg <- b$ind[!is.na(status)]
  # page 1: title, key figures, key messages, mortality, targets at a glance
  md <- c("![](logo.png){width=1.6in}", "", "::: {custom-style=\"Brand Word\"}", "DASHBOARD", ":::", "",
          sprintf("# Health brief: %s", b$area$name), "",
          sprintf("**%s** · generated %s from the Busoga Health Forum Dashboard · data extracted %s",
                  b$period$label, format(Sys.Date(), "%d %B %Y"), META$extracted), "",
          md_table(data.table(`Projected population` = f$value[1], `Health facilities` = f$value[2],
                              `Reporting completeness` = f$value[4],
                              `Indicators below target` = if (nrow(tg)) sprintf("%d of %d", sum(tg$status == "below"), nrow(tg)) else "-")),
          "## Key messages", "", paste("-", brief_sentences(b)), "")
  mo <- brief_mortality(b)
  if (nrow(mo)) md <- c(md, "## Maternal and child mortality", "",
                        md_table(data.table(Indicator = mo$indicator, Value = fmt_val(mo$value, mo$code),
                                            Previous = fmt_val(mo$prev, mo$code), Change = chg_text(mo)), c(4, 1.2, 1.2, 1.3)),
                        small("Facility-reported deaths, compared with the previous period of the same length."))
  sp <- png_of(brief_score_plot(b), "score.png", 6.6, 2.0)
  if (!is.null(sp)) md <- c(md, "## Targets at a glance", "", sprintf("![](%s){width=6.3in}", sp), "")
  # page 2: below target, districts, trends
  md <- c(md, pagebreak)
  below <- b$ind[status == "below"]
  if (nrow(below)) {
    gp <- png_of(brief_gap_plot(b), "gap.png", 6.8, min(3.3, 0.7 + 0.17 * nrow(below)))
    md <- c(md, "## Indicators below target", "", if (!is.null(gp)) sprintf("![](%s){width=6.5in}", gp), "")
  }
  if (!is.null(b$dist) && nrow(b$dist)) {
    dd <- b$dist[order(-n_below)]
    md <- c(md, "## Districts and cities below target", "",
            md_table(head(dd, 5)[, .(Indicator = indicator, Below = n_below, `Districts / cities` = short_area(districts))], c(2.4, 0.6, 4)),
            if (nrow(dd) > 5) small(sprintf("%d more indicators have districts below target: see 'Performance against targets' in the dashboard.", nrow(dd) - 5)))
  }
  tp <- png_of(brief_trend_plot(b), "trend.png", 6.8, 2.8)
  if (!is.null(tp)) md <- c(md, "## Trends to watch", "", sprintf("![](%s){width=6.5in}", tp), "")
  # page 3: indicator summary, about the data
  rows <- brief_summary_rows(b); rows <- rows[code %in% unlist(HEADLINE)]
  z <- rbindlist(lapply(intersect(levels(rows$theme), as.character(unique(rows$theme))), function(th)
    rbind(data.table(Indicator = sprintf("**%s**", th), Value = "", Previous = "", Change = "", Target = "", Status = ""),
          brief_rows(rows[theme == th])[, Status := fifelse(Status == "Below", "**Below**", Status)])))
  md <- c(md, pagebreak, "## Indicator summary", "", md_table(z, c(3.3, 0.9, 0.9, 0.9, 0.8, 0.7)),
          "## About the data", "",
          small(paste(sprintf("The summary lists headline indicators; all %d indicators, with district and facility results, are in the dashboard (* = shown capped at 100%%).", nrow(b$ind)),
                      DATA_STATEMENT, paste(BRIEF_NOTES, collapse = " "),
                      "Targets: Uganda national target (MoH Strategic Plan 2020/21-2024/25; AHSPR 2024/25) where one exists, otherwise a global target.",
                      sprintf("**For queries about this brief or the dashboard, contact %s.**", CONTACT))))
  mdf <- file.path(dir, "brief.md"); writeLines(md, mdf, useBytes = TRUE)
  ref <- normalizePath("brief_reference.docx", mustWork = FALSE)
  owd <- setwd(dir); on.exit(setwd(owd), add = TRUE)
  rmarkdown::pandoc_convert("brief.md", to = "docx", from = "markdown+fenced_divs+raw_attribute+pipe_tables", output = file.path(dir, "brief.docx"),
                            options = if (file.exists(ref)) c("--reference-doc", ref) else NULL)
  file.copy(file.path(dir, "brief.docx"), file, overwrite = TRUE)
}

# ---- PDF (grid graphics, A4, at most 3 pages) --------------------------------------------------
write_brief_pdf <- function(b, file) {
  W <- 8.27; H <- 11.69; L <- 0.55; R <- W - 0.55; CW <- R - L
  cairo <- capabilities("cairo")
  if (cairo) grDevices::cairo_pdf(file, width = W, height = H, onefile = TRUE, family = "sans")
  else grDevices::pdf(file, width = W, height = H, encoding = "ISOLatin1")
  on.exit(grDevices::dev.off(), add = TRUE)
  txt <- if (cairo) identity else function(x) chartr("≥≤–·°", "><-.o", x)
  u <- function(x) grid::unit(x, "in")
  gp <- grid::gpar
  NAVY <- BRAND$navy; MAROON <- BRAND$maroon; INK2 <- BRAND$ink2; MUTED <- BRAND$muted
  page <- 0L; y <- 0; BOTTOM <- 0.75
  logo <- tryCatch(png::readPNG("www/bhf_logo.png"), error = function(e) NULL)

  text <- function(s, x, yy, size = 9, bold = FALSE, col = BRAND$ink, just = c("left", "top"), rot = 0)
    grid::grid.text(txt(s), u(x), u(yy), just = just, rot = rot, gp = gp(fontsize = size, fontface = if (bold) "bold" else "plain", col = col))
  wrap <- function(s, width_in, size) strwrap(txt(s), width = max(10, floor(width_in * 72 / (0.5 * size))))
  para <- function(s, x, width_in, size = 9, col = BRAND$ink, lh = 1.35, bold = FALSE) {
    for (w in wrap(s, width_in, size)) { text(w, x, y, size, bold, col); y <<- y - size * lh / 72 } }
  rect <- function(x, yy, w, h, fill, col = NA, r = 0.08)
    grid::grid.roundrect(u(x), u(yy), u(w), u(h), r = u(r), just = c("left", "top"), gp = gp(fill = fill, col = col, lwd = 0.6))
  footer <- function() {
    grid::grid.lines(u(c(L, R)), u(c(0.55, 0.55)), gp = gp(col = "#e1e0d9", lwd = 0.6))
    text(sprintf("Busoga Health Forum · Health brief · %s · %s", b$area$name, b$period$label), L, 0.47, 6.5, col = MUTED)
    text(sprintf("Queries: %s", CONTACT), L, 0.33, 6.5, col = MUTED)
    text(sprintf("Page %d", page), R, 0.47, 6.5, col = MUTED, just = c("right", "top"))
  }
  newpage <- function(title = NULL) {
    if (page > 0) footer()
    grid::grid.newpage(); page <<- page + 1L; y <<- H - 0.55
    if (page > 1) {                                   # running header
      if (!is.null(logo)) grid::grid.raster(logo, u(L), u(H - 0.3), height = u(0.3), just = c("left", "top"))
      text("DASHBOARD", L + 0.02, H - 0.62, 5.8, TRUE, MAROON)
      text(sprintf("%s · %s", b$area$name, b$period$label), R, H - 0.4, 8, col = INK2, just = c("right", "top"))
      grid::grid.lines(u(c(L, R)), u(c(H - 0.8, H - 0.8)), gp = gp(col = "#e1e0d9", lwd = 0.6))
      y <<- H - 1.02
    }
  }
  heading <- function(s, sub = NULL) {
    grid::grid.rect(u(L), u(y), u(0.06), u(0.2), just = c("left", "top"), gp = gp(fill = MAROON, col = NA))
    text(s, L + 0.14, y + 0.005, 12.5, TRUE, NAVY)
    if (!is.null(sub)) text(sub, R, y - 0.03, 7, col = MUTED, just = c("right", "top"))
    y <<- y - 0.34
  }
  room <- function() y - BOTTOM
  draw_plot <- function(g, h, x = L, w = CW) {
    vp <- grid::viewport(x = u(x), y = u(y), width = u(w), height = u(h), just = c("left", "top"))
    print(g, vp = vp); y <<- y - h
  }
  # a table with fixed column widths; returns the rows it could not fit
  table <- function(df, widths, size = 7.6, rh = 0.19, status_col = NULL, max_y = BOTTOM) {
    x0 <- L + c(0, cumsum(widths))[seq_along(widths)]
    cut <- function(v, w) { mx <- floor(w * 72 / (0.5 * size)) - 1; ifelse(nchar(v) > mx, paste0(substr(v, 1, mx - 1), "…"), v) }
    grid::grid.rect(u(L), u(y), u(sum(widths)), u(rh + 0.02), just = c("left", "top"), gp = gp(fill = BRAND$navy_soft, col = NA))
    for (k in seq_along(df)) text(cut(names(df)[k], widths[k]), x0[k] + 0.05, y - (rh + 0.02) / 2, size - 0.3, TRUE, NAVY, just = "left")
    y <<- y - rh - 0.02
    for (i in seq_len(nrow(df))) {
      if (y - rh < max_y) return(df[i:nrow(df)])
      if (i %% 2 == 0) grid::grid.rect(u(L), u(y), u(sum(widths)), u(rh), just = c("left", "top"), gp = gp(fill = "#f7f6f3", col = NA))
      for (k in seq_along(df)) {
        v <- txt(as.character(df[[k]][i]))
        if (!is.null(status_col) && k == status_col && v %in% c("Met", "Below")) {
          rect(x0[k] + 0.04, y - 0.03, 0.5, rh - 0.06, if (v == "Met") "#e3f3e6" else "#fbe3e1", r = 0.06)
          text(v, x0[k] + 0.29, y - rh / 2, size - 0.6, TRUE, if (v == "Met") "#1b5e20" else "#8c1d18", just = "centre")
        } else text(cut(v, widths[k]), x0[k] + 0.05, y - rh / 2, size, k == 1 && FALSE, BRAND$ink, just = "left")
      }
      y <<- y - rh
    }
    NULL
  }

  # ======== page 1: cover, key facts, key messages, targets, mortality ========
  newpage()
  if (!is.null(logo)) grid::grid.raster(logo, u(L), u(H - 0.3), height = u(0.5), just = c("left", "top"))
  text("DASHBOARD", L + 0.03, H - 0.83, 7, TRUE, MAROON)
  text("HEALTH BRIEF", R, H - 0.45, 8.5, TRUE, MAROON, just = c("right", "top"))
  text(sprintf("Generated %s", format(Sys.Date(), "%d %B %Y")), R, H - 0.64, 7.5, col = MUTED, just = c("right", "top"))
  band_top <- H - 1.12; band_h <- 1.25
  grad <- matrix(grDevices::colorRampPalette(c("#1a1560", NAVY, "#4a1d5e", MAROON))(300), nrow = 1)
  grid::grid.raster(grad, u(L), u(band_top), width = u(CW), height = u(band_h), just = c("left", "top"), interpolate = TRUE)
  grid::grid.circle(u(R - 0.3), u(band_top + 0.1), u(0.9), gp = gp(fill = grDevices::adjustcolor("white", 0.06), col = NA))
  text(b$area$name, L + 0.3, band_top - 0.22, 24, TRUE, "white")
  text(b$period$label, L + 0.3, band_top - 0.72, 11, col = "#e9e8f4")
  text(sprintf("%s · %d programme areas · data extracted %s", LEVEL_LABEL[[b$area$level]], length(unique(b$ind$theme)), META$extracted),
       L + 0.3, band_top - 0.95, 7.5, col = "#cfcde6")
  y <- band_top - band_h - 0.2

  # key figures
  f <- b$facts; tg <- b$ind[!is.na(status)]
  kp <- list(c("Projected population", f$value[1]), c("Health facilities", f$value[2]),
             c("Reporting completeness", f$value[4]),
             c("Indicators below target", if (nrow(tg)) sprintf("%d of %d", sum(tg$status == "below"), nrow(tg)) else "-"))
  bw <- (CW - 3 * 0.12) / 4
  for (k in seq_along(kp)) { x <- L + (k - 1) * (bw + 0.12)
    rect(x, y, bw, 0.72, "#fcfcfb", "#e1e0d9")
    grid::grid.rect(u(x), u(y), u(0.05), u(0.72), just = c("left", "top"), gp = gp(fill = if (k == 4) MAROON else NAVY, col = NA))
    text(toupper(kp[[k]][1]), x + 0.15, y - 0.12, 6.5, TRUE, INK2)
    text(kp[[k]][2], x + 0.15, y - 0.3, 17, TRUE, if (k == 4) MAROON else NAVY) }
  y <- y - 0.95

  # key messages (left) and targets by theme (right)
  top <- y; lw <- 3.95
  heading("Key messages")
  for (s in brief_sentences(b)) {
    grid::grid.circle(u(L + 0.06), u(y - 0.065), u(0.028), gp = gp(fill = MAROON, col = NA))
    xx <- L + 0.18; for (w in wrap(s, lw - 0.2, 9)) { text(w, xx, y, 9, col = BRAND$ink); y <- y - 0.17 }
    y <- y - 0.07 }
  left_end <- y; y <- top
  sp <- brief_score_plot(b)
  if (!is.null(sp)) {
    x2 <- L + lw + 0.25; w2 <- R - x2
    grid::grid.rect(u(x2), u(y), u(0.06), u(0.2), just = c("left", "top"), gp = gp(fill = MAROON, col = NA))
    text("Targets at a glance", x2 + 0.14, y + 0.005, 12.5, TRUE, NAVY); y <- y - 0.34
    nth <- length(unique(b$ind[!is.na(status), theme]))
    draw_plot(sp, max(1.6, 0.34 * nth + 0.7), x2, w2)
  }
  y <- min(left_end, y) - 0.15

  # maternal and child mortality
  mo <- brief_mortality(b)
  if (nrow(mo) && room() > 1.5) {
    heading("Maternal and child mortality", "facility-reported deaths; change vs previous period")
    n <- min(nrow(mo), 6L); cw <- (CW - (n - 1) * 0.1) / n; col <- theme_col("Maternal & child mortality")
    for (k in seq_len(n)) { x <- L + (k - 1) * (cw + 0.1); m <- mo[k]
      rect(x, y, cw, 1.12, "#f4f5f7", NA)
      lab <- wrap(m$indicator, cw - 0.2, 7)
      for (j in seq_along(head(lab, 3))) text(lab[j], x + 0.1, y - 0.1 - (j - 1) * 0.12, 7, TRUE, INK2)
      text(fmt_val(m$value, m$code), x + 0.1, y - 0.55, 15, TRUE, col)
      good <- !is.na(m$better) && m$better > 0; bad <- !is.na(m$better) && m$better < 0
      text(chg_text(m), x + 0.1, y - 0.86, 7, TRUE, if (good) "#1b6e1b" else if (bad) "#a3261c" else MUTED)
      text(unit_label(m$code), x + 0.1, y - 0.99, 6.2, col = MUTED) }
    y <- y - 1.3
  }
  if (!is.null(b$dist) && nrow(b$dist) && room() > 1) {
    heading("Districts and cities below target")
    dd <- b$dist[order(-n_below), .(Indicator = indicator, `Below` = as.character(n_below), `Districts / cities` = short_area(districts))]
    rest <- table(dd, c(2.65, 0.5, CW - 3.15), size = 7.2, rh = 0.18)
    if (!is.null(rest)) { text(sprintf("… and %d more indicators with districts below target: see 'Performance against targets' in the dashboard.", nrow(rest)),
                               L, y - 0.03, 6.8, col = MUTED); y <- y - 0.2 }
  }

  # ======== then, flowing onto pages 2 and 3: below target, trends, indicator summary ========
  about <- c(wrap(DATA_STATEMENT, CW - 0.24, 6.3), wrap(paste(c(BRIEF_NOTES, TARGET_NOTE, CAP_NOTE), collapse = " "), CW - 0.24, 6.3))
  about_h <- 0.5 + length(about) * 6.3 * 1.3 / 72
  ensure <- function(h) if (room() < h) { newpage(); TRUE } else FALSE
  gp_ <- brief_gap_plot(b); tp <- brief_trend_plot(b)
  gap_h <- if (!is.null(gp_)) min(5.2, 0.55 + 0.25 * sum(b$ind$status == "below", na.rm = TRUE)) else 0
  draw_gap <- function() { heading("Indicators below target", "bar = progress towards the target"); draw_plot(gp_, gap_h); y <<- y - 0.2 }
  draw_trend <- function() { heading("Trends to watch", "last 24 months; dashed red line = target")
    draw_plot(tp, max(2.6, min(3.6, room() - 0.1))); y <<- y - 0.2 }
  # use the space left on page 1: the trends fit more often than the (taller) below-target chart
  if (!is.null(gp_) && room() >= gap_h + 0.45) { draw_gap(); gp_ <- NULL }
  if (!is.null(tp) && room() >= 3.0) { draw_trend(); tp <- NULL }
  if (!is.null(gp_)) { ensure(gap_h + 0.45); draw_gap() }
  if (!is.null(tp)) { ensure(3.0); draw_trend() }

  # indicator summary: one table, grouped by programme, split across pages 2-3 if needed
  rows <- brief_summary_rows(b); rows <- rows[code %in% unlist(HEADLINE)]
  widths <- c(2.95, 0.8, 0.8, 0.85, 0.85, CW - 6.25); rh <- 0.168; size <- 7.3
  x0 <- L + c(0, cumsum(widths))[seq_along(widths)]
  floor_y <- function() BOTTOM + (if (page >= 3) about_h + 0.25 else 0)
  table_head <- function() {
    grid::grid.rect(u(L), u(y), u(CW), u(rh + 0.02), just = c("left", "top"), gp = gp(fill = BRAND$navy_soft, col = NA))
    hdr <- c("Indicator", "Value", "Previous", "Change", "Target", "Status")
    for (k in seq_along(hdr)) text(hdr[k], x0[k] + 0.05, y - (rh + 0.02) / 2, size - 0.3, TRUE, NAVY, just = "left")
    y <<- y - rh - 0.04
  }
  if (room() < 1.6) newpage()
  heading("Indicator summary", "headline indicators for each selected programme; mortality is on page 1")
  table_head()
  left <- 0L
  next_page <- function() { if (page >= 3) return(FALSE); newpage(); heading("Indicator summary", "continued"); table_head(); TRUE }
  for (th in intersect(levels(rows$theme), as.character(unique(rows$theme)))) {
    z <- brief_rows(rows[theme == th])
    if (y - rh * 2 < floor_y() && !next_page()) { left <- left + nrow(z); next }
    grid::grid.rect(u(L), u(y), u(0.05), u(rh), just = c("left", "top"), gp = gp(fill = theme_col(th), col = NA))
    text(th, L + 0.12, y - rh / 2, 7.6, TRUE, theme_col(th), just = "left"); y <- y - rh
    for (i in seq_len(nrow(z))) {
      if (y - rh < floor_y() && !next_page()) { left <- left + nrow(z) - i + 1; break }
      if (i %% 2 == 1) grid::grid.rect(u(L), u(y), u(CW), u(rh), just = c("left", "top"), gp = gp(fill = "#f7f6f3", col = NA))
      for (k in seq_along(z)) {
        v <- txt(as.character(z[[k]][i]))
        if (k == 6 && v %in% c("Met", "Below")) {
          rect(x0[k] + 0.04, y - 0.025, 0.5, rh - 0.05, if (v == "Met") "#e3f3e6" else "#fbe3e1", r = 0.05)
          text(v, x0[k] + 0.29, y - rh / 2, size - 0.6, TRUE, if (v == "Met") "#1b5e20" else "#8c1d18", just = "centre")
        } else {
          mx <- floor(widths[k] * 72 / (0.5 * size)) - 1; if (nchar(v) > mx) v <- paste0(substr(v, 1, mx - 1), "…")
          text(v, x0[k] + (if (k == 1) 0.12 else 0.05), y - rh / 2, size, FALSE, BRAND$ink, just = "left")
        }
      }
      y <- y - rh
    }
    y <- y - 0.05
  }
  text(sprintf("All %d indicators in the selection, with district and facility results, are in the dashboard%s.", nrow(b$ind),
               if (left) sprintf(" (%d rows did not fit here)", left) else ""), L, y - 0.02, 6.8, col = MUTED)
  y <- y - 0.25

  # about the data (small print), at the foot of the last page
  if (room() < about_h && page < 3) newpage()
  y <- BOTTOM + about_h
  rect(L, y, CW, about_h - 0.08, "#f7f6f3", NA)
  y <- y - 0.1
  text("About the data", L + 0.12, y, 8, TRUE, NAVY); y <- y - 0.17
  for (w in about) { text(w, L + 0.12, y, 6.3, col = INK2); y <- y - 6.3 * 1.3 / 72 }
  text(sprintf("For queries about this brief or the dashboard, contact %s.", CONTACT), L + 0.12, y - 0.03, 6.8, TRUE, NAVY)
  footer()
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
