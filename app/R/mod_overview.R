# Overview: Busoga (or any area) at a glance, one colour-coded block per programme theme.

HEADLINE <- list(
  "Antenatal care"              = c("ANC01", "ANC03", "ANC09", "ANC14"),
  "Delivery & newborn"          = c("DEL01", "DEL02", "DEL04", "DEL06"),
  "Postnatal & family planning" = c("PNC01", "PNC03", "PNC07", "PNC08"),
  "Immunisation"                = c("EPI03", "EPI07", "EPI09", "EPI10"),
  "Child health & nutrition"    = c("CHN01", "CHN04", "CHN05", "CHN06"),
  "Malaria"                     = c("MAL01", "MAL02", "MAL09", "MAL10"),
  "HIV & PMTCT"                 = c("HIV01", "HIV03", "HIV04", "HIV05"),
  "Services & mortality"        = c("SRV01", "SRV02", "SRV03", "SRV06"))
# keep four live indicators per theme: drop any whose series ended and top up from the same theme
HEADLINE <- setNames(lapply(names(HEADLINE), function(th) {
  live <- IND[theme == th & is.na(ended), code]
  head(unique(c(intersect(HEADLINE[[th]], live), live)), 4)
}), names(HEADLINE))

# switch to a page in the browser (server-side nav_select does not reach items inside nav menus here)
go_to_page <- function(value) sprintf("var t=document.querySelector('.navbar a[data-value=\"%s\"]'); if(t){bootstrap.Tab.getOrCreateInstance(t).show(); window.scrollTo(0,0);}", value)
GO_EXPLORER <- go_to_page("explorer")

overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Busoga Health Forum · Dashboard", "Busoga at a glance",
              "Routine health data (HMIS/DHIS2) for every health facility in the 12 districts and cities of Busoga, alongside open datasets on population (UBOS, WorldPop), climate (CHIRPS, ERA5-Land, CAMS, ECMWF) and places (OpenStreetMap). Pick an area and period; every figure updates, and every tile opens for deeper analysis."),
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
    tile <- function(lab, v, sub) div(div(class = "h-lab", lab), div(class = "h-val", v), div(class = "h-sub", sub))
    div(class = "hero",
      tile(a$name, if (length(popv)) formatC(popv, big.mark = ",", format = "d") else "–",
           sprintf("projected population %d", p$to %/% 100L)),
      tile("Health facilities", formatC(nrow(facs), big.mark = ","), sprintf("%s reported in the period", formatC(active, big.mark = ","))),
      tile("Reporting completeness", if (is.na(compl)) "–" else sprintf("%.0f%%", compl), sprintf("105:01 OPD report, %.0f%% on time", timely)),
      tile("OPD new attendances", fmt_val(val("SRV01"), "SRV01"), p$label),
      tile("Deliveries in facilities", fmt_val(val("DEL01"), "DEL01"), p$label),
      tile("HIV tests performed", fmt_val(val("HIV01"), "HIV01"), p$label))
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
        tags$a(href = "#", class = "kpi kpi-link", style = sprintf("--th:%s", col), title = "Open in the Indicator explorer",
               onclick = sprintf("Shiny.setInputValue('%s', {code: '%s', nonce: Math.random()}, {priority: 'event'}); %s return false;", ns("open"), cd, GO_EXPLORER),
            div(class = "kpi-top", div(class = "kpi-label", ind_label(cd),
                    if (IND[code == cd, area_only]) span(class = "muted", title = "Population-based: area level only", " ‡")),
                span(class = "kpi-go", "↗")),
            div(class = "kpi-mid", div(class = "kpi-value", fmt_val(v, cd), if (capd) span(class = "cap-mark", title = CAP_NOTE, "*")), delta),
            div(class = "kpi-sub", sprintf("%s · vs previous %d months", unit_label(cd), p$n)),
            target_chip(v, cd, p$n),
            div(class = "kpi-chart", mini_chart(tr$value, tr$bucket, col, cd, refv, target = target_value_line(cd, monthly = TRUE))))
      })
      div(class = "theme-block", style = sprintf("--th:%s", col),
          div(class = "theme-head",
              span(class = "theme-badge", fontawesome::fa(theme_icon(th), fill = "#fff", height = "1.05em")),
              h4(th),
              tags$a(href = "#", class = "theme-explore",
                     onclick = sprintf("Shiny.setInputValue('%s', {theme: '%s', nonce: Math.random()}, {priority: 'event'}); %s return false;", ns("open"), th, GO_EXPLORER),
                     "Explore all →")),
          div(class = "kpi-grid", tiles))
    })
    tagList(div(class = "theme-grid", blocks),
            info_note("Click any tile to analyse that indicator in depth, or 'Explore all' to compare a whole programme. ",
                      "‡ population-based (denominator is the projected population), so areas only. ",
                      "Change pills are green when the indicator moved in the desirable direction. Charts show the last 24 months; the dotted line is Busoga and the dashed red line the target. ",
                      TARGET_NOTE, " ", CAP_NOTE))
  })
  observeEvent(input$open, {
    o <- input$open; a <- area(); p <- per()
    codes <- if (!is.null(o$code)) o$code else if (!is.null(o$codes)) unlist(o$codes) else HEADLINE[[o$theme]]
    session$userData$explore(list(codes = codes, level = a$level, uid = a$uid, nonce = runif(1)))
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
             onclick = sprintf("Shiny.setInputValue('%s', {code: '%s', nonce: Math.random()}, {priority: 'event'}); %s return false;", ns("open"), cd, GO_EXPLORER),
             div(class = "kpi-top", div(class = "kpi-label", ind_label(cd)), span(class = "kpi-go", "↗")),
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
        info_note("Adolescent shares use the age groups reported on the HMIS 105 form (deliveries and ANC: under 15 and 15-19; HIV testing: 10-14 and 15-19). Click a tile for trends and comparisons."))
  })
  output$stamp <- renderText(sprintf("Data extracted from the national DHIS2 (hmis.health.go.ug) on %s. Latest month: %s.",
                                     META$extracted, fmt_month(MONTH_MAX)))
})
