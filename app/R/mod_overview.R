# Overview: Busoga (or any area) at a glance, one colour-coded block per programme theme.

HEADLINE <- list(
  "Maternal & child mortality"  = c("DEL07", "MCM01", "MCM02", "DEL06", "MCM03", "MCM04", "MCM05", "MCM06"),
  "Antenatal care"              = c("ANC01", "ANC03", "ANC09", "ANC14"),
  "Delivery & newborn"          = c("DEL01", "DEL02", "DEL04", "DEL11"),
  "Postnatal & family planning" = c("PNC01", "PNC03", "PNC07", "PNC08"),
  "Immunisation"                = c("EPI03", "EPI07", "EPI09", "EPI10"),
  "Child health & nutrition"    = c("CHN01", "CHN04", "CHN05", "CHN06"),
  "Malaria"                     = c("MAL01", "MAL02", "MAL09", "MAL10"),
  "HIV & PMTCT"                 = c("HIV01", "HIV03", "HIV04", "HIV05"),
  "Services & mortality"        = c("SRV01", "SRV02", "SRV03", "SRV06"))
# keep the listed live indicators (a theme may borrow indicators from another theme, e.g. the
# maternal mortality ratio in the mortality block); drop any whose series ended and top up from
# the same theme
HEADLINE <- HEADLINE[names(HEADLINE) %in% c(as.character(unique(IND$theme)), "Maternal & child mortality")]
HEADLINE <- setNames(lapply(names(HEADLINE), function(th) {
  n <- length(HEADLINE[[th]])
  live_all <- IND[is.na(ended), code]; live <- IND[theme == th & is.na(ended), code]
  head(unique(c(intersect(HEADLINE[[th]], live_all), live)), n)
}), names(HEADLINE))
HEADLINE <- HEADLINE[lengths(HEADLINE) > 0]

# switch to a page in the browser (server-side nav_select does not reach items inside nav menus here)
go_to_page <- function(value) sprintf("var t=document.querySelector('.navbar a[data-value=\"%s\"]'); if(t){bootstrap.Tab.getOrCreateInstance(t).show(); window.scrollTo(0,0);}", value)
GO_EXPLORER <- go_to_page("explorer")
EXPAND_ICON <- HTML('<svg viewBox="0 0 16 16" width="13" height="13" aria-hidden="true"><path fill="currentColor" d="M3.72 3.72a.75.75 0 0 1 .53-.22h3a.75.75 0 0 1 0 1.5H6.06l2.22 2.22a.75.75 0 0 1-1.06 1.06L5 6.06v1.19a.75.75 0 0 1-1.5 0v-3c0-.2.08-.39.22-.53Zm8.56 8.56a.75.75 0 0 1-.53.22h-3a.75.75 0 0 1 0-1.5h1.19l-2.22-2.22a.75.75 0 1 1 1.06-1.06L11 9.94V8.75a.75.75 0 0 1 1.5 0v3c0 .2-.08.39-.22.53Z"/></svg>')
expand_js <- function(input_id, code, theme, col = NULL)
  sprintf("Shiny.setInputValue('%s', {code: '%s', theme: '%s', col: '%s', nonce: Math.random()}, {priority: 'event'}); return false;",
          input_id, code, theme, if (is.null(col)) "" else col)

overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Busoga Health Forum · Dashboard", "Busoga at a glance",
              "Routine health data (HMIS/DHIS2) for every health facility in the 12 districts and cities of Busoga, alongside open datasets on population (UBOS, WorldPop), climate (CHIRPS, ERA5-Land, CAMS, ECMWF) and places (OpenStreetMap). Pick an area and period; every figure updates, and every tile opens for deeper analysis.", banner = TRUE, key = "overview"),
    filter_bar(area_ui(ns("area")), period_ui(ns("period"))),
    uiOutput(ns("hero")),
    uiOutput(ns("asrh")),
    uiOutput(ns("themes")),
    div(class = "footer-note", textOutput(ns("stamp"), inline = TRUE))
  )
}

overview_server <- function(id) moduleServer(id, function(input, output, session) {
  area <- area_server("area"); per <- period_server("period")

  output$hero <- renderUI({
    a <- area(); p <- per()
    facs <- OU[level_name == "facility"]
    if (a$level != "region") facs <- facs[uid_l3 == a$uid | uid_l4 == a$uid | uid_l5 == a$uid]
    r <- REP[dataset == "RtEYsASU7PG" & period >= p$from & period <= p$to & uid %in% facs$uid]
    compl <- if (sum(r$expected)) 100 * sum(r$actual) / sum(r$expected) else NA
    timely <- if (sum(r$expected)) 100 * sum(r$on_time) / sum(r$expected) else NA
    active <- uniqueN(r[actual > 0, uid])
    popv <- POP[uid == a$uid & year == p$to %/% 100L, pop]
    val <- function(cd) { s <- summarise_ind(cd, a$level, p$from, p$to, uids = a$uid); if (nrow(s)) s$value else NA }
    ns <- session$ns
    tile <- function(lab, v, sub, js) tags$a(href = "#", class = "hero-link", title = "Tap to enlarge", onclick = js,
                                             div(class = "h-lab", lab, span(class = "h-go", EXPAND_ICON)), div(class = "h-val", v), div(class = "h-sub", sub))
    ex <- function(cd) expand_js(ns("expand"), cd, IND[code == cd, theme])
    div(class = "hero",
      tile(a$name, if (length(popv)) formatC(popv, big.mark = ",", format = "d") else "–",
           sprintf("projected population %d", p$to %/% 100L), tile_js(ns("tile"), "population")),
      tile("Health facilities", formatC(nrow(facs), big.mark = ","), sprintf("%s reported in the period", formatC(active, big.mark = ",")), tile_js(ns("tile"), "facilities")),
      tile("Reporting completeness", if (is.na(compl)) "–" else sprintf("%.0f%%", compl), sprintf("105:01 OPD report, %.0f%% on time", timely), tile_js(ns("tile"), "reporting")),
      tile("OPD new attendances", fmt_val(val("SRV01"), "SRV01"), p$label, ex("SRV01")),
      tile("Deliveries in facilities", fmt_val(val("DEL01"), "DEL01"), p$label, ex("DEL01")),
      tile("HIV tests performed", fmt_val(val("HIV01"), "HIV01"), p$label, ex("HIV01")))
  })

  output$themes <- renderUI({
    a <- area(); p <- per()
    codes <- unlist(HEADLINE)
    cur <- summarise_ind(codes, a$level, p$from, p$to, uids = a$uid)
    pw <- previous_window(p$from, p$to)
    prev <- if (!is.null(pw)) summarise_ind(codes, a$level, pw[1], pw[2], uids = a$uid) else data.table(code = character(), value = numeric())
    s_from <- date_ym(seq(ym_date(p$to), by = "-23 months", length.out = 2)[2])
    trend <- summarise_ind(codes, a$level, max(s_from, MONTH_MIN), p$to, uids = a$uid, by = "month")
    ns <- session$ns
    ref_tr <- if (a$level != "region") summarise_ind(codes, "region", max(s_from, MONTH_MIN), p$to, by = "month") else NULL
    blocks <- lapply(names(HEADLINE), function(th) {
      col <- theme_col(th)
      tiles <- lapply(HEADLINE[[th]], function(cd) {
        v <- cur[code == cd, value]; v <- if (length(v)) v else NA
        capd <- isTRUE(cur[code == cd, capped][1])
        pv <- prev[code == cd, value]; pv <- if (length(pv)) pv else NA
        dirn <- IND[code == cd, direction]
        if (!is.null(pw) && isTRUE(SERIES_START[cd] > pw[1])) pv <- NA
        delta <- if (!is.na(v) && !is.na(pv) && pv != 0) {
          ch <- v - pv; up <- ch >= 0
          good <- (dirn == "high" && up) || (dirn == "low" && !up)
          cls <- if (dirn == "neutral") "neutral" else if (good) "good" else "bad"
          txt <- if (IND[code == cd, unit] == "%") sprintf("%+.1f pts", ch) else sprintf("%+.1f%%", 100 * ch / abs(pv))
          span(class = paste("delta-pill", cls), paste(if (up) "▲" else "▼", txt))
        } else span(class = "delta-pill neutral", "no comparison")
        tr <- trend[code == cd][order(bucket)]
        refv <- if (!is.null(ref_tr)) { r <- ref_tr[code == cd]; if (nrow(r)) mean(r$value, na.rm = TRUE) else NA } else NA
        tags$a(href = "#", class = "kpi kpi-link", style = sprintf("--th:%s", col), title = "Tap to enlarge",
               onclick = expand_js(ns("expand"), cd, th),
            div(class = "kpi-top", div(class = "kpi-label", ind_label(cd),
                    if (IND[code == cd, area_only]) span(class = "muted", title = "Population-based: area level only", " ‡")),
                span(class = "kpi-go", EXPAND_ICON)),
            div(class = "kpi-mid", div(class = "kpi-value", fmt_val(v, cd), if (capd) span(class = "cap-mark", title = CAP_NOTE, "*")), delta),
            div(class = "kpi-sub", sprintf("%s · vs previous %d months", unit_label(cd), p$n)),
            target_chip(v, cd, p$n),
            div(class = "kpi-chart", mini_chart(tr$value, tr$bucket, col, cd, refv, target = target_value_line(cd, monthly = TRUE))))
      })
      div(class = paste("theme-block", if (th == "Maternal & child mortality") "theme-wide"), style = sprintf("--th:%s", col),
          div(class = "theme-head",
              span(class = "theme-badge", fontawesome::fa(theme_icon(th), fill = "#fff", height = "1.05em")),
              h4(th),
              tags$a(href = "#", class = "theme-explore",
                     onclick = sprintf("Shiny.setInputValue('%s', {theme: '%s', nonce: Math.random()}, {priority: 'event'}); %s return false;", ns("open"), th, GO_EXPLORER),
                     "Explore all →")),
          div(class = "kpi-grid", tiles))
    })
    tagList(div(class = "theme-grid", blocks),
            info_note("Tap any tile to enlarge its chart with full axes and details; 'See details' in the enlarged view opens the indicator in the Indicator explorer, and 'Explore all' compares a whole programme. ",
                      "‡ population-based (denominator is the projected population), so areas only. ",
                      "Change pills are green when the indicator moved in the desirable direction. Charts show the last 24 months; the dotted line is Busoga and the dashed red line the target. ",
                      TARGET_NOTE, " ", CAP_NOTE))
  })
  tile_modal_server(input, output, session)
  observeEvent(input$open, {
    o <- input$open; a <- area(); p <- per()
    codes <- if (!is.null(o$code)) o$code else if (!is.null(o$codes)) unlist(o$codes) else HEADLINE[[o$theme]]
    session$userData$explore(list(codes = codes, level = a$level, uid = a$uid, nonce = runif(1)))
  })
  # ---- enlarged tile: bigger chart with axes, context and a link to the full analysis ----
  observeEvent(input$expand, {
    o <- input$expand; cd <- o$code; a <- area(); p <- per(); ns <- session$ns
    col <- if (nzchar(o$col %||% "")) o$col else theme_col(o$theme)
    if (is.na(col)) col <- BRAND$navy
    s <- summarise_ind(cd, a$level, p$from, p$to, uids = a$uid)
    v <- if (nrow(s)) s$value else NA; vr <- if (nrow(s)) s$value_raw else NA
    pw <- previous_window(p$from, p$to)
    pv <- if (!is.null(pw) && !isTRUE(SERIES_START[cd] > pw[1])) { z <- summarise_ind(cd, a$level, pw[1], pw[2], uids = a$uid); if (nrow(z)) z$value else NA } else NA
    busoga <- if (a$level != "region") { z <- summarise_ind(cd, "region", p$from, p$to); if (nrow(z)) z$value else NA } else NA
    fact <- function(lab, val, sub = NULL) div(class = "xp-fact", div(class = "xp-fact-lab", lab), div(class = "xp-fact-val", val), if (!is.null(sub)) div(class = "xp-fact-sub", sub))
    chg <- if (!is.na(v) && !is.na(pv)) { if (IND[code == cd, unit] == "%") sprintf("%+.1f pts", v - pv) else if (pv != 0) sprintf("%+.1f%%", 100 * (v - pv) / abs(pv)) else "–" } else "–"
    d <- IND[code == cd]
    defn <- if (nzchar(d$num_desc %||% "")) sprintf("Numerator: %s. Denominator: %s.", d$num_desc, d$den_desc) else d$note
    showModal(modalDialog(
      title = div(class = "xp-title", style = sprintf("--th:%s", col),
                  span(class = "theme-badge", fontawesome::fa(if (is.na(theme_icon(o$theme))) "person-half-dress" else theme_icon(o$theme), fill = "#fff", height = "1em")),
                  div(div(class = "xp-h", ind_label(cd)), div(class = "xp-sub", sprintf("%s · %s · %s", a$name, p$label, unit_label(cd))))),
      size = "xl", easyClose = TRUE, fade = TRUE,
      div(class = "xp-facts", style = sprintf("--th:%s", col),
          fact("Selected period", paste0(fmt_val(v, cd), if (isTRUE(s$capped[1])) "*" else ""), p$label),
          fact(if (!is.null(pw)) "Previous period" else "Previous period", fmt_val(pv, cd), if (!is.null(pw)) period_caption(pw[1], pw[2]) else NULL),
          fact("Change", chg, d$direction |> switch(high = "higher is better", low = "lower is better", "no preferred direction")),
          if (a$level != "region") fact("Busoga", fmt_val(busoga, cd), "same period"),
          if (!is.na(target_text(cd))) fact("Target", target_text(cd), target_chip(vr, cd, p$n))),
      card(class = "xp-card", full_screen = FALSE,
           card_header(sprintf("%s, %s: monthly trend", ind_label(cd), a$name),
                       span(class = "sub", "shaded band = selected period; use the buttons to change the time range")),
           plotlyOutput(ns("xp_trend"), height = "380px")),
      if (a$level %in% c("region", "district")) card(class = "xp-card",
           card_header(sprintf("%s by %s, %s", ind_label(cd), if (a$level == "region") "district / city" else "sub-county", p$label)),
           plotlyOutput(ns("xp_areas"), height = "320px")),
      div(class = "xp-def", tags$b("Definition. "), defn, " ", if (!is.na(target_text(cd))) tagList(tags$b("Target source. "), TGT$source[match(cd, TGT$code)])),
      footer = tagList(
        tags$button(type = "button", class = "btn btn-outline-secondary", `data-bs-dismiss` = "modal", "Close"),
        tags$button(type = "button", class = "btn btn-primary xp-go",
                    onclick = sprintf("Shiny.setInputValue('%s', {code: '%s', nonce: Math.random()}, {priority: 'event'}); bootstrap.Modal.getInstance(this.closest('.modal')).hide(); %s",
                                      ns("open"), cd, GO_EXPLORER),
                    fontawesome::fa("magnifying-glass-chart", fill = "#fff", height = "1em"), " See details in the Indicator explorer")))
    )
    xp$code <- cd; xp$col <- col; xp$nonce <- runif(1)
  })
  xp <- reactiveValues(code = NULL, col = NULL, nonce = 0)
  output$xp_trend <- renderPlotly({
    req(xp$code); xp$nonce; cd <- xp$code; col <- xp$col; a <- area(); p <- per()
    tr <- summarise_ind(cd, a$level, MONTH_MIN, MONTH_MAX, uids = a$uid, by = "month")[order(bucket)]
    validate(need(nrow(tr) > 1, "Not enough data for a trend."))
    tr[, date := ym_date(bucket)]
    g <- plot_ly(tr, x = ~date) |>
      add_lines(y = ~value, name = a$name, line = list(color = col, width = 2.4),
                hovertemplate = paste0("%{x|%b %Y}<br>", a$name, ": %{y:,.1f} ", unit_label(cd), "<extra></extra>"))
    if (a$level != "region") {
      rb <- summarise_ind(cd, "region", MONTH_MIN, MONTH_MAX, by = "month")[order(bucket)][, date := ym_date(bucket)]
      g <- g |> add_lines(data = rb, x = ~date, y = ~value, name = "Busoga", line = list(color = BRAND$muted, width = 1.4, dash = "dot"),
                          hovertemplate = paste0("%{x|%b %Y}<br>Busoga: %{y:,.1f}<extra></extra>"))
    }
    tv <- target_value_line(cd, monthly = TRUE)
    shapes <- list(list(type = "rect", xref = "x", yref = "paper", x0 = ym_date(p$from) - 15, x1 = ym_date(p$to) + 15, y0 = 0, y1 = 1,
                        fillcolor = col, opacity = 0.08, line = list(width = 0), layer = "below"))
    if (is.finite(tv)) {
      g <- g |> add_lines(x = range(tr$date), y = c(tv, tv), name = paste("Target", target_text(cd)),
                          line = list(color = "#b3261e", width = 1.4, dash = "dash"), hoverinfo = "skip")
    }
    g |> plotly_base(ytitle = unit_label(cd), xtitle = "Month") |>
      layout(shapes = shapes, margin = list(l = 10, r = 10, t = 36, b = 10), showlegend = a$level != "region" || is.finite(tv),
             xaxis = list(title = "Month", showgrid = FALSE, tickformat = "%b %Y", linecolor = BRAND$base,
                          rangeselector = list(x = 0, y = 1.12, buttons = list(
                            list(count = 12, label = "12 months", step = "month", stepmode = "backward"),
                            list(count = 24, label = "24 months", step = "month", stepmode = "backward"),
                            list(step = "all", label = "All years")))),
             yaxis = list(title = unit_label(cd), gridcolor = BRAND$grid, rangemode = "tozero", tickformat = ",.0f"))
  })
  output$xp_areas <- renderPlotly({
    req(xp$code); xp$nonce; cd <- xp$code; col <- xp$col; a <- area(); p <- per()
    lvl <- if (a$level == "region") "district" else "subcounty"
    s <- summarise_ind(cd, lvl, p$from, p$to)
    if (a$level == "district") s <- s[uid %in% OU[uid_l3 == a$uid, uid]]
    validate(need(nrow(s) > 0, "No area data for this indicator."))
    s[, name := ou_name[uid]]; setorder(s, value)
    tv <- target_value_line(cd)
    s[, st := target_status(value_raw, cd, p$n)]
    s[, fill := fifelse(is.na(st), col, fifelse(st == "below", "#c0392b", col))]
    g <- plot_ly(s, y = ~factor(name, levels = name), x = ~value, type = "bar", orientation = "h",
                 marker = list(color = ~fill), text = ~fmt_val(value, cd), textposition = "outside", cliponaxis = FALSE,
                 hovertemplate = "%{y}: %{text}<extra></extra>") |>
      plotly_base(xtitle = unit_label(cd), legend = FALSE) |>
      layout(yaxis = list(title = "", tickfont = list(size = 11, color = BRAND$ink2)), xaxis = list(title = unit_label(cd), gridcolor = BRAND$grid),
             margin = list(l = 10, r = 40, t = 10, b = 10))
    if (is.finite(tv)) g <- g |> layout(shapes = list(list(type = "line", xref = "x", yref = "paper", x0 = tv, x1 = tv, y0 = 0, y1 = 1,
                                                          line = list(color = "#b3261e", dash = "dash", width = 1.4))))
    g
  })

  # ---- Adolescent & reproductive health focus ----
  output$asrh <- renderUI({
    a <- area(); p <- per(); ns <- session$ns
    col <- "#B0306A"
    yr <- p$to %/% 100L; if (p$to %% 100L < 12L) yr <- yr - 1L          # last complete calendar year
    b <- if (!is.null(BRK)) BRK[uid == a$uid & year %in% c(yr, yr - 1L)] else data.table()
    adol_share <- function(item_, y) { z <- b[item == item_ & year == y]; if (!nrow(z) || !sum(z$value)) return(NA)
      100 * z[age %in% c("<15Yrs", "15-19Yrs", "10-19Yrs", "10-14Yrs"), sum(value)] / sum(z$value) }
    adol_n <- function(item_, y) { z <- b[item == item_ & year == y]; if (!nrow(z)) return(NA)
      z[age %in% c("<15Yrs", "15-19Yrs", "10-19Yrs", "10-14Yrs"), sum(value)] }
    pill <- function(v, pv, dirn, unit = "pts") {
      if (is.na(v) || is.na(pv)) return(span(class = "delta-pill neutral", "no comparison"))
      ch <- v - pv; up <- ch >= 0; good <- (dirn == "high" && up) || (dirn == "low" && !up)
      span(class = paste("delta-pill", if (dirn == "neutral") "neutral" else if (good) "good" else "bad"),
           paste(if (up) "▲" else "▼", if (unit == "pts") sprintf("%+.1f pts", ch) else sprintf("%+.1f%%", 100 * ch / abs(pv))))
    }
    btile <- function(label, v, pv, fmt, sub, dirn, unit = "pts")
      div(class = "kpi asrh-tile", style = sprintf("--th:%s", col),
          div(class = "kpi-label", label), div(class = "kpi-mid", div(class = "kpi-value", fmt(v)), pill(v, pv, dirn, unit)),
          div(class = "kpi-sub", sub))
    pct <- function(v) if (is.na(v)) "–" else sprintf("%.1f%%", v)
    num <- function(v) if (is.na(v)) "–" else format(round(v), big.mark = ",")
    codes <- intersect(c("ANC04", "PNC06", "PNC11", "PNC12", "PNC09", "SRV06"), IND$code)
    cur <- summarise_ind(codes, a$level, p$from, p$to, uids = a$uid)
    pw <- previous_window(p$from, p$to)
    prev <- if (!is.null(pw)) summarise_ind(codes, a$level, pw[1], pw[2], uids = a$uid) else data.table(code = character(), value = numeric())
    itile <- function(cd) { v <- cur[code == cd, value]; v <- if (length(v)) v else NA; pv <- prev[code == cd, value]; pv <- if (length(pv)) pv else NA
      tags$a(href = "#", class = "kpi kpi-link asrh-tile", style = sprintf("--th:%s", col),
             onclick = expand_js(ns("expand"), cd, "Adolescent & reproductive health", col), title = "Tap to enlarge",
             div(class = "kpi-top", div(class = "kpi-label", ind_label(cd)), span(class = "kpi-go", EXPAND_ICON)),
             div(class = "kpi-mid", div(class = "kpi-value", fmt_val(v, cd), if (isTRUE(cur[code == cd, capped][1])) span(class = "cap-mark", title = CAP_NOTE, "*")),
                 pill(v, pv, IND[code == cd, direction])),
             div(class = "kpi-sub", if (is.na(END_OF[cd])) p$label else ended_note(cd)), target_chip(v, cd, p$n)) }
    div(class = "theme-block asrh-block", style = sprintf("--th:%s", col),
        div(class = "theme-head",
            span(class = "theme-badge", fontawesome::fa("person-half-dress", fill = "#fff", height = "1.05em")),
            h4("Adolescent & reproductive health"),
            tags$a(href = "#", class = "theme-explore",
                   onclick = sprintf("Shiny.setInputValue('%s', {codes: %s, nonce: Math.random()}, {priority: 'event'}); %s return false;",
                                     ns("open"), jsonlite::toJSON(codes), GO_EXPLORER), "Explore all →")),
        div(class = "kpi-grid",
            btile(sprintf("Deliveries to adolescents (under 20), %d", yr), adol_share("Deliveries in unit", yr), adol_share("Deliveries in unit", yr - 1L),
                  pct, sprintf("%s adolescent deliveries in facilities", num(adol_n("Deliveries in unit", yr))), "low"),
            btile(sprintf("ANC 1 clients under 20, %d", yr), adol_share("ANC 1st visits", yr), adol_share("ANC 1st visits", yr - 1L),
                  pct, sprintf("%s adolescents at a first ANC visit", num(adol_n("ANC 1st visits", yr))), "low"),
            btile(sprintf("HIV tests among 10-19-year-olds, %d", yr), adol_n("HIV tests performed", yr), adol_n("HIV tests performed", yr - 1L),
                  num, sprintf("%s new positives aged 10-19", num(adol_n("New HIV positives", yr))), "high", unit = "pct"),
            lapply(codes, itile)),
        info_note("Adolescent shares use the age groups reported on the HMIS 105 form (deliveries and ANC: under 15 and 15-19; HIV testing: 10-14 and 15-19). Tap a tile to enlarge it."))
  })
  output$stamp <- renderText(sprintf("Data extracted from the national DHIS2 (hmis.health.go.ug) on %s. Latest month: %s.",
                                     META$extracted, fmt_month(MONTH_MAX)))
})
