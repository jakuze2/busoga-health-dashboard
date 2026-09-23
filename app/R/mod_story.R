# Busoga in focus: the landing page of the Busoga profile menu. A visual portrait of the region
# from the 2024 census (UBOS) and the routine health data: headline numbers that count up, ten
# facts about daily life, twelve district cards that open into district profiles, how districts
# compare on every measure, a census map of the sub-counties, who the Basoga are by age, and how
# households get their information (useful when planning health messages).

story_cen <- function(uid, table, column) {
  if (is.null(CTX)) return(NA_real_)
  x <- CTX$census; i <- which(x$uid %in% uid & x$table == table & x$column == column)   # base indexing: no column/argument clash
  if (length(i)) x$value[i[1]] else NA_real_
}
STORY_DISTRICTS <- if (!is.null(CTX)) unique(CTX$census[level == "district" & !is.na(uid), uid]) else character()
STORY_FACTS <- list(
  list(k = "water", icon = "faucet-drip", col = "#0277BD", say = function(v) sprintf("of households use an improved water source")),
  list(k = "sanit", icon = "toilet", col = "#6D4C41", say = function(v) "of households have improved sanitation"),
  list(k = "net", icon = "shield-virus", col = "#2E7D32", say = function(v) "of households own a mosquito net"),
  list(k = "grid", icon = "bolt", col = "#F9A825", say = function(v) "of households are on grid electricity"),
  list(k = "insur", icon = "file-shield", col = "#5C6BC0", say = function(v) "of people have health insurance"),
  list(k = "birthreg", icon = "id-card", col = "#8E24AA", say = function(v) "of people have a birth certificate"),
  list(k = "oos", icon = "school", col = "#C62828", say = function(v) "of children aged 6-12 are out of school"),
  list(k = "neet", icon = "user-clock", col = "#EF6C00", say = function(v) "of young people aged 18-30 are not in work, education or training"),
  list(k = "subsist", icon = "seedling", col = "#558B2F", say = function(v) "of households live in the subsistence economy"),
  list(k = "pdm", icon = "hand-holding-dollar", col = "#00838F", say = function(v) "of households have benefited from the Parish Development Model"))
STORY_AGES <- function(uid) {
  g <- function(c) story_cen(uid, "Age Groups", c)
  tot <- story_cen(uid, "Population by Sex", "Total")
  a04 <- g("Age 0-4"); a017 <- g("Age 0-17"); a1830 <- g("Age 18-30"); a18 <- g("Age 18+"); a60 <- g("Age 60+")
  data.table(group = c("Under 5", "5-17", "18-30", "31-59", "60 and over"),
             n = c(a04, a017 - a04, a1830, a18 - a1830 - a60, a60))[, pct := 100 * n / sum(n)][]
}
STORY_HMIS <- c("ANC02", "DEL02", "EPI09", "MAL10", "SRV02", "MCM01")

story_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Busoga profile · in focus", "Busoga: its people, places and health",
              "A portrait of Busoga from the 2024 National Population and Housing Census and the routine health records of every facility: who lives here, how they live, and how each of the 12 districts and cities compares. Tap a district card to open its profile.",
              banner = TRUE, key = "story"),
    uiOutput(ns("bignums")),
    div(class = "st-sec", div(class = "st-kicker", "Life in Busoga"), h3(class = "st-h", "Ten facts from the 2024 census"),
        p(class = "st-lead", "Each fact shows Busoga as a whole; the bar underneath runs from the district furthest behind to the one furthest ahead.")),
    uiOutput(ns("facts")),
    div(class = "st-sec", div(class = "st-kicker", "Twelve districts and cities"), h3(class = "st-h", "Meet the districts"),
        p(class = "st-lead", "People, households and health facilities in each district or city, with three everyday measures against Busoga (the tick). Tap a card for the district's full profile.")),
    uiOutput(ns("cards")),
    div(class = "st-sec", div(class = "st-kicker", "Side by side"), h3(class = "st-h", "Where each district stands"),
        p(class = "st-lead", "Every census measure for every district, coloured by how far it is ahead of (green) or behind (red) Busoga as a whole. Hover for the numbers.")),
    card(full_screen = TRUE, class = "xai-card", plotlyOutput(ns("heat"), height = "520px")),
    layout_columns(col_widths = c(7, 5),
      div(div(class = "st-sec", div(class = "st-kicker", "On the map"), h3(class = "st-h", "The census, sub-county by sub-county")),
          card(full_screen = TRUE, class = "xai-card",
               card_header(selectInput(ns("measure"), NULL, setNames(names(CENSUS_MEASURES), vapply(CENSUS_MEASURES, `[[`, "", "lab")), width = "100%")),
               leafletOutput(ns("map"), height = 520))),
      div(div(class = "st-sec", div(class = "st-kicker", "The Basoga"), h3(class = "st-h", "A young population")),
          card(class = "xai-card", plotlyOutput(ns("ages"), height = "300px")),
          div(class = "st-sec st-sec-tight", div(class = "st-kicker", "Reaching people"), h3(class = "st-h", "How households get their news")),
          card(class = "xai-card", plotlyOutput(ns("info"), height = "280px")))),
    info_note("Sources: UBOS National Population and Housing Census 2024 (statistics.ubos.org), for Busoga, its 12 districts and cities and its sub-counties; Busoga totals are the sum of its districts. Health facilities and health indicators: Ministry of Health DHIS2 (HMIS), last 12 months. ",
              "Photos: Wikimedia Commons (credited on each photo) and the Busoga Health Forum.")
  )
}

story_server <- function(id) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  R <- META$region_uid
  facs <- OU[level_name == "facility" & is.na(closed_date)]

  output$bignums <- renderUI({
    req(!is.null(CTX))
    pop <- story_cen(R, "Population by Sex", "Total"); hh <- story_cen(R, "Household Size", "Number of Households")
    hs <- story_cen(R, "Household Size", "Average Household Size"); u5 <- story_cen(R, "Age Groups", "Age 0-4"); u18 <- story_cen(R, "Age Groups", "Age 0-17")
    p02 <- if (!is.null(CH)) CH[area == "Busoga" & year == 2002, pop] else NA
    num <- function(icon, v, fmt, lab, sub, dec = 0, key = NULL) tags$a(href = "#", class = "st-num st-link", title = "Tap to enlarge", onclick = if (!is.null(key)) tile_js(ns("tile"), key) else "return false;",
      span(class = "st-num-icon", fontawesome::fa(icon, fill = "#fff", height = "1.1em")),
      div(class = "st-num-v", `data-count` = v, `data-dec` = dec, `data-suffix` = fmt, if (is.finite(v)) paste0(formatC(v, format = "f", digits = dec, big.mark = ","), fmt) else "–"),
      div(class = "st-num-l", lab), div(class = "st-num-s", sub), span(class = "st-go", EXPAND_ICON))
    div(class = "st-nums",
      num("people-group", pop, "", "people counted in the 2024 census", if (is.finite(p02)) sprintf("up %.0f%% since the 2002 census", 100 * (pop / p02 - 1)) else "UBOS census 2024", key = "population"),
      num("house", hh, "", "households", sprintf("%.1f people per household", hs), key = "households"),
      num("baby", 100 * u5 / pop, "%", "are children under five", sprintf("%s young children", format(round(u5), big.mark = ",")), key = "ages"),
      num("child-reaching", 100 * u18 / pop, "%", "are under 18", sprintf("%s children and teenagers", format(round(u18), big.mark = ",")), key = "ages"),
      num("map-location-dot", length(STORY_DISTRICTS), "", "districts and cities", sprintf("%d sub-counties and divisions", nrow(OU[level_name == "subcounty"])), key = "population"),
      num("hospital", nrow(facs), "", "open health facilities", sprintf("%s hospitals, %s HC IVs, %s HC IIIs", sum(grepl("Hospital|RRH", facs$grp_level)), sum(facs$grp_level == "HC IV", na.rm = TRUE), sum(facs$grp_level == "HC III", na.rm = TRUE)), key = "facilities"),
      tags$script(HTML("window.bhfCountUp && window.bhfCountUp();")))
  })

  output$facts <- renderUI({
    req(!is.null(CTX))
    div(class = "st-facts", lapply(STORY_FACTS, function(f) {
      m <- CENSUS_MEASURES[[f$k]]; v <- census_value(R, m)
      dv <- vapply(STORY_DISTRICTS, census_value, 0, m = m); dv <- dv[is.finite(dv)]
      lo <- names(dv)[which.min(dv)]; hi <- names(dv)[which.max(dv)]
      best <- if (m$good == "high") hi else lo; worst <- if (m$good == "high") lo else hi
      rng <- range(dv); pos <- function(x) 100 * (x - rng[1]) / max(1e-9, diff(rng))
      tags$a(href = "#", class = "st-fact st-link", style = sprintf("--fc:%s", f$col), title = "Tap to enlarge", onclick = tile_js(ns("tile"), "census", f$k),
          div(class = "st-fact-top", span(class = "st-fact-icon", fontawesome::fa(f$icon, fill = f$col, height = "1.15em")),
              div(class = "st-fact-v", pct_txt(v))),
          div(class = "st-fact-t", f$say(v)),
          div(class = "st-range", div(class = "st-range-bar"), div(class = "st-range-dot", style = sprintf("left:%.1f%%", pos(v)), title = "Busoga")),
          div(class = "st-range-lab", span(sprintf("%s %s", ou_name[[lo]], pct_txt(min(dv)))), span(sprintf("%s %s", ou_name[[hi]], pct_txt(max(dv))))),
          div(class = "st-fact-foot", sprintf("Furthest ahead: %s", ou_name[[best]])), span(class = "st-go", EXPAND_ICON))
    }))
  })

  output$cards <- renderUI({
    req(!is.null(CTX))
    mini <- c("water", "grid", "insur")
    ord <- STORY_DISTRICTS[order(-vapply(STORY_DISTRICTS, story_cen, 0, table = "Population by Sex", column = "Total"))]
    div(class = "st-cards", lapply(ord, function(u) {
      pop <- story_cen(u, "Population by Sex", "Total"); hh <- story_cen(u, "Household Size", "Number of Households")
      nf <- nrow(facs[uid_l3 == u])
      bars <- lapply(mini, function(k) { m <- CENSUS_MEASURES[[k]]; v <- census_value(u, m); b <- census_value(R, m)
        div(class = "st-mb", div(class = "st-mb-l", span(sub("^(Households|People) (using an |with |with a |with health )?", "", m$lab)), span(class = "st-mb-v", pct_txt(v))),
            div(class = "st-mb-track", div(class = "st-mb-fill", style = sprintf("width:%.0f%%", min(100, v))), div(class = "st-mb-tick", style = sprintf("left:%.0f%%", min(100, b))))) })
      tags$a(href = "#", class = "st-card", onclick = sprintf("Shiny.setInputValue('%s', {uid: '%s', nonce: Math.random()}, {priority: 'event'}); return false;", ns("open"), u),
             div(class = "st-card-h", span(class = "st-card-n", sub(" District$", "", ou_name[[u]])), span(class = "blk-go", EXPAND_ICON)),
             div(class = "st-card-stats",
                 div(div(class = "st-cs-v", format(round(pop), big.mark = ",")), div(class = "st-cs-l", "people")),
                 div(div(class = "st-cs-v", format(round(hh), big.mark = ",")), div(class = "st-cs-l", "households")),
                 div(div(class = "st-cs-v", nf), div(class = "st-cs-l", "facilities"))),
             bars)
    }))
  })

  # ---- district profile -------------------------------------------------------------------------
  sel <- reactiveVal(NULL)
  observeEvent(input$open, {
    u <- input$open$uid; sel(u); nm <- ou_name[[u]]
    pop <- story_cen(u, "Population by Sex", "Total"); hh <- story_cen(u, "Household Size", "Number of Households")
    to <- MONTH_MAX; from <- date_ym(seq(ym_date(to), by = "-11 months", length.out = 2)[2])
    s <- summarise_ind(intersect(STORY_HMIS, IND$code), "district", from, to, uids = u); r <- summarise_ind(intersect(STORY_HMIS, IND$code), "region", from, to)
    hm <- lapply(intersect(STORY_HMIS, IND$code), function(cd) {
      v <- s[code == cd, value]; b <- r[code == cd, value]; dn <- IND[code == cd, direction]
      better <- if (length(v) && length(b) && is.finite(v) && is.finite(b) && dn != "neutral") ((v > b) == (dn == "high")) else NA
      div(class = "st-hm", div(class = "st-hm-l", ind_label(cd)),
          div(class = "st-hm-v", fmt_val(if (length(v)) v else NA, cd),
              span(class = paste("st-hm-cmp", if (isTRUE(better)) "good" else if (isFALSE(better)) "bad" else ""),
                   sprintf("Busoga %s", fmt_val(if (length(b)) b else NA, cd)))))
    })
    fact <- function(lab, val, sub = NULL) div(class = "xp-fact", div(class = "xp-fact-lab", lab), div(class = "xp-fact-val", val), if (!is.null(sub)) div(class = "xp-fact-sub", sub))
    showModal(modalDialog(
      title = div(class = "xp-title", style = "--th:#201B6D", span(class = "theme-badge", fontawesome::fa("map-location-dot", fill = "#fff", height = "1em")),
                  div(div(class = "xp-h", nm), div(class = "xp-sub", "District profile · census 2024 and the last 12 months of health data"))),
      size = "xl", easyClose = TRUE, fade = TRUE,
      div(class = "xp-facts", style = "--th:#201B6D",
          fact("People", format(round(pop), big.mark = ","), sprintf("%.1f%% of Busoga", 100 * pop / story_cen(R, "Population by Sex", "Total"))),
          fact("Households", format(round(hh), big.mark = ","), sprintf("%.1f people each", story_cen(u, "Household Size", "Average Household Size"))),
          fact("Health facilities", nrow(facs[uid_l3 == u]), sprintf("%s sub-counties and divisions", nrow(OU[level_name == "subcounty" & uid_l3 == u]))),
          fact("Children under five", pct_txt(100 * story_cen(u, "Age Groups", "Age 0-4") / pop), "of the population")),
      layout_columns(col_widths = c(7, 5),
        card(class = "xp-card", card_header("Everyday life against Busoga", span(class = "sub", "census 2024; the black tick is Busoga")), plotlyOutput(ns("d_census"), height = "430px")),
        card(class = "xp-card", card_header("Health services", span(class = "sub", sprintf("%s; green = better than Busoga", period_caption(from, to)))), div(class = "st-hms", hm))),
      card(class = "xp-card", card_header(sprintf("Sub-counties of %s", nm), span(class = "sub", "people and households, census 2024")), plotlyOutput(ns("d_sc"), height = "320px")),
      footer = tagList(tags$button(type = "button", class = "btn btn-outline-secondary", `data-bs-dismiss` = "modal", "Close"),
                       tags$button(type = "button", class = "btn btn-primary xp-go", onclick = paste0("bootstrap.Modal.getInstance(this.closest('.modal')).hide(); ", go_to_page("deep")),
                                   fontawesome::fa("id-card", fill = "#fff", height = "1em"), " Open the full area profile"))))
  })
  output$d_census <- renderPlotly({
    u <- sel(); req(u)
    x <- rbindlist(lapply(names(CENSUS_MEASURES), function(k) { m <- CENSUS_MEASURES[[k]]
      data.table(name = m$lab, v = census_value(u, m), b = census_value(R, m), good = m$good) }))[is.finite(v)]
    x[, fill := fifelse(abs(v - b) < 1, "#a79a6d", fifelse((v > b) == (good == "high"), "#2e7d32", "#c0392b"))]; x <- x[order(v)]
    plot_ly(x, y = ~factor(name, levels = name)) |>
      add_bars(x = ~v, orientation = "h", marker = list(color = ~fill), text = ~sprintf("%.0f%%", v), textposition = "outside", cliponaxis = FALSE,
               hovertemplate = "%{y}: %{x:.1f}%<extra></extra>", name = ou_name[[u]]) |>
      add_markers(x = ~b, name = "Busoga", marker = list(symbol = "line-ns", size = 16, line = list(width = 2.5, color = BRAND$ink)), hovertemplate = "Busoga: %{x:.1f}%<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(xaxis = list(ticksuffix = "%", range = c(0, 108)), yaxis = list(title = "", tickfont = list(size = 10.5)), margin = list(l = 10, r = 30, t = 5, b = 10))
  })
  output$d_sc <- renderPlotly({
    u <- sel(); req(u)
    x <- CTX$census[level == "subcounty" & duid == u & table == "Population by Sex" & column == "Total" & !is.na(uid), .(name = ou_name[uid], pop = value)][order(-pop)][, name := make.unique(name, sep = " ")]
    validate(need(nrow(x) > 0, "No sub-county census tables for this area."))
    plot_ly(x, x = ~factor(name, levels = name), y = ~pop, type = "bar", marker = list(color = "#3949AB"), hovertemplate = "%{x}: %{y:,.0f} people<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(xaxis = list(title = "", tickangle = -35, tickfont = list(size = 10)), yaxis = list(title = "People", tickformat = ",.0f"))
  })

  # ---- comparisons --------------------------------------------------------------------------------
  output$heat <- renderPlotly({
    req(!is.null(CTX))
    ks <- names(CENSUS_MEASURES)
    M <- sapply(ks, function(k) vapply(STORY_DISTRICTS, census_value, 0, m = CENSUS_MEASURES[[k]]))
    B <- vapply(ks, function(k) census_value(R, CENSUS_MEASURES[[k]]), 0)
    sgn <- ifelse(vapply(ks, function(k) CENSUS_MEASURES[[k]]$good, "") == "high", 1, -1)
    D <- sweep(sweep(M, 2, B), 2, sgn, `*`)                       # positive = better than Busoga
    lim <- max(abs(D), na.rm = TRUE)
    rn <- sub(" District$", "", ou_name[STORY_DISTRICTS]); o <- order(rowMeans(D / apply(abs(D), 2, max, na.rm = TRUE), na.rm = TRUE))
    txt <- matrix(sprintf("%s<br>%s: <b>%.1f%%</b><br>Busoga: %.1f%%", rep(rn, ncol(M)), rep(vapply(ks, function(k) CENSUS_MEASURES[[k]]$lab, ""), each = nrow(M)), M, rep(B, each = nrow(M))), nrow(M))
    plot_ly(x = vapply(ks, function(k) CENSUS_MEASURES[[k]]$lab, ""), y = rn[o], z = D[o, ], type = "heatmap", zmin = -lim, zmax = lim,
            colorscale = list(c(0, "#b2182b"), c(.5, "#f7f7f7"), c(1, "#1b7837")), text = txt[o, ], hoverinfo = "text",
            colorbar = list(title = "points vs<br>Busoga", len = .8)) |>
      add_annotations(x = rep(vapply(ks, function(k) CENSUS_MEASURES[[k]]$lab, ""), each = nrow(M)), y = rep(rn[o], ncol(M)), text = sprintf("%.0f", M[o, ]),
                      showarrow = FALSE, font = list(size = 11, color = "#222")) |>
      plotly_base(legend = FALSE) |> layout(xaxis = list(tickangle = -30, tickfont = list(size = 10.5), side = "top"), yaxis = list(tickfont = list(size = 12)),
                                            margin = list(l = 10, r = 10, t = 120, b = 10))
  })

  output$map <- renderLeaflet({
    req(!is.null(CTX)); m <- CENSUS_MEASURES[[input$measure]]
    g <- GEO$subcounty; g$v <- vapply(g$uid, census_value, 0, m = m)
    pal <- colorNumeric(if (m$good == "high") c("#fde0dd", "#c51b8a", "#49006a") else c("#edf8e9", "#fd8d3c", "#a50f15"), g$v, na.color = "#e8e7e2")
    base_map() |>
      addPolygons(data = g, fillColor = ~pal(v), fillOpacity = .85, color = "white", weight = .7,
                  label = lapply(sprintf("<b>%s</b><br>%s: <b>%s</b>", htmlEscape(g$name), htmlEscape(m$lab), ifelse(is.finite(g$v), sprintf("%.1f%%", g$v), "no census table")), HTML),
                  highlightOptions = highlightOptions(weight = 2.5, color = BRAND$ink, bringToFront = TRUE)) |>
      addPolylines(data = GEO$district, color = BRAND$navy, weight = 1.8) |>
      addLegend("bottomright", pal = pal, values = g$v, title = htmlEscape(m$lab), labFormat = labelFormat(suffix = "%"), opacity = .9, na.label = "No table")
  })

  output$ages <- renderPlotly({ req(!is.null(CTX)); ages_plot() })
  tile_modal_server(input, output, session)
  output$info <- renderPlotly({
    req(!is.null(CTX))
    x <- CTX$census[level == "region" & table == "Information Sources" & column != "Total Households"]
    tot <- story_cen(R, "Information Sources", "Total Households")
    x <- x[, .(name = column, v = 100 * value / tot)][order(v)]
    plot_ly(x, y = ~factor(name, levels = name), x = ~v, type = "bar", orientation = "h", marker = list(color = "#00897B"),
            text = ~sprintf("%.0f%%", v), textposition = "outside", cliponaxis = FALSE, hovertemplate = "%{y}: %{x:.1f}% of households<extra></extra>") |>
      plotly_base(legend = FALSE) |> layout(xaxis = list(ticksuffix = "%", title = "main source of information, % of households", range = c(0, max(x$v) * 1.2)),
                                            yaxis = list(title = ""), margin = list(l = 10, r = 30, t = 5, b = 10))
  })
})
