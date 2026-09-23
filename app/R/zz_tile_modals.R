# Deeper views for headline tiles that are not a single HMIS indicator (population, households,
# facilities, reporting, age structure, census measures). Any module can open one:
#   onclick = tile_js(ns("tile"), "population")      in the UI
#   tile_modal_server(input, output, session)          in the server
# The modal has key facts, one or two charts and a note on the source.

tile_js <- function(input_id, key, arg = "")
  sprintf("Shiny.setInputValue('%s', {key: '%s', arg: '%s', nonce: Math.random()}, {priority: 'event'}); return false;", input_id, key, arg)

ages_plot <- function() {
  R <- META$region_uid
  x <- rbindlist(c(list(STORY_AGES(R)[, area := "Busoga"]), lapply(STORY_DISTRICTS, function(u) STORY_AGES(u)[, area := sub(" District$", "", ou_name[[u]])])))
  x[, group := factor(group, levels = c("Under 5", "5-17", "18-30", "31-59", "60 and over"))]
  ord <- x[group %in% c("Under 5", "5-17"), .(y = sum(pct)), by = area][order(y), area]; ord <- c(setdiff(ord, "Busoga"), "Busoga")
  plot_ly(x, y = ~factor(area, levels = ord), x = ~pct, color = ~group, colors = c("#B0306A", "#EF8A62", "#5C6BC0", "#3949AB", "#1a1a4e"), type = "bar", orientation = "h",
          hovertemplate = "%{y}, %{fullData.name}: %{x:.1f}%<extra></extra>") |>
    plotly_base() |> layout(barmode = "stack", xaxis = list(ticksuffix = "%", title = ""), yaxis = list(title = "", tickfont = list(size = 10.5)),
                            legend = list(orientation = "h", y = 1.12, x = 0), margin = list(l = 10, r = 10, t = 30, b = 10))
}
facility_plot <- function() {
  f <- OU[level_name == "facility" & grp_level %in% c("HC II", "HC III", "HC IV", "General Hospital", "RRH", "Clinic")]
  f[, lv := fifelse(grp_level %in% c("General Hospital", "RRH"), "Hospital", grp_level)]
  x <- f[, .N, by = .(uid = uid_l3, lv)][, name := sub(" District$", "", ou_name[uid])]
  ord <- x[, sum(N), by = name][order(V1), name]
  cols <- c(Hospital = "#75002C", `HC IV` = "#C0582B", `HC III` = "#3949AB", `HC II` = "#8E9BD8", Clinic = "#c9c6d6")
  p <- plot_ly()
  for (k in names(cols)) { z <- x[lv == k]; if (nrow(z)) p <- p |> add_bars(data = z, y = ~factor(name, levels = ord), x = ~N, name = k, orientation = "h",
                                                                            marker = list(color = cols[[k]]), hovertemplate = paste0("%{y}: %{x} ", k, "<extra></extra>")) }
  p |> plotly_base() |> layout(barmode = "stack", xaxis = list(title = "Facilities"), yaxis = list(title = ""), legend = list(orientation = "h", y = 1.08))
}
households_plot <- function() {
  d <- CTX$census[level == "district" & table == "Household Size" & column %in% c("Number of Households", "Average Household Size") & !is.na(uid)]
  d <- dcast(d, uid ~ column, value.var = "value"); setnames(d, c("uid", "size", "n"))
  d[, name := sub(" District$", "", ou_name[uid])]; setorder(d, n)
  plot_ly(d, y = ~factor(name, levels = name)) |>
    add_bars(x = ~n, name = "Households", orientation = "h", marker = list(color = "#3949AB"), hovertemplate = "%{y}: %{x:,.0f} households<extra></extra>") |>
    add_markers(x = ~size * max(d$n) / 6, name = "People per household", marker = list(color = "#B0306A", size = 11, symbol = "diamond"),
                text = ~sprintf("%.1f", size), customdata = ~size, hovertemplate = "%{y}: %{customdata:.1f} people per household<extra></extra>") |>
    plotly_base() |> layout(xaxis = list(title = "Households", tickformat = ",.0f"), yaxis = list(title = ""), legend = list(orientation = "h", y = 1.08))
}
census_rank_plot <- function(k) {
  m <- CENSUS_MEASURES[[k]]
  x <- data.table(uid = STORY_DISTRICTS, v = vapply(STORY_DISTRICTS, census_value, 0, m = m))[is.finite(v)][, name := sub(" District$", "", ou_name[uid])][order(v)]
  b <- census_value(META$region_uid, m)
  x[, col := if (m$good == "high") colorRampPalette(c("#e4acac", "#2e7d32"))(.N) else colorRampPalette(c("#2e7d32", "#e4acac"))(.N)]
  plot_ly(x, y = ~factor(name, levels = name), x = ~v, type = "bar", orientation = "h", marker = list(color = ~col),
          text = ~sprintf("%.1f%%", v), textposition = "outside", cliponaxis = FALSE, hovertemplate = "%{y}: %{text}<extra></extra>") |>
    plotly_base(xtitle = m$lab, legend = FALSE) |>
    layout(xaxis = list(ticksuffix = "%", range = c(0, max(x$v, b) * 1.18)), yaxis = list(title = ""),
           shapes = list(list(type = "line", x0 = b, x1 = b, y0 = 0, y1 = 1, yref = "paper", line = list(color = BRAND$ink, dash = "dash"))),
           annotations = list(list(x = b, y = 1.03, yref = "paper", text = sprintf("Busoga %.1f%%", b), showarrow = FALSE, font = list(size = 10.5))), margin = list(r = 40))
}
census_sc_plot <- function(k) {
  m <- CENSUS_MEASURES[[k]]
  ids <- unique(CTX$census[level == "subcounty" & !is.na(uid), uid])
  x <- data.table(uid = ids, v = vapply(ids, census_value, 0, m = m))[is.finite(v)]
  x[, `:=`(name = ou_name[uid], district = OU$district[match(uid, OU$uid)])]
  top <- rbind(x[order(v)][1:8], x[order(-v)][1:8])[, name := make.unique(sprintf("%s (%s)", name, district), sep = " ")]
  top[, grp := fifelse(v >= median(x$v), "Highest", "Lowest")]; setorder(top, v)
  plot_ly(top, y = ~factor(name, levels = name), x = ~v, type = "bar", orientation = "h", color = ~grp, colors = c(Highest = "#3949AB", Lowest = "#C0582B"),
          text = ~sprintf("%.1f%%", v), textposition = "outside", cliponaxis = FALSE, hovertemplate = "%{y}: %{text}<extra></extra>") |>
    plotly_base(xtitle = m$lab) |> layout(xaxis = list(ticksuffix = "%", range = c(0, max(top$v) * 1.2)), yaxis = list(title = "", tickfont = list(size = 10)), margin = list(r = 40))
}
reporting_plot <- function(ds = "RtEYsASU7PG", uids = NULL) {
  r <- REP[dataset == ds]; if (!is.null(uids)) r <- r[uid %in% uids]
  r <- r[, .(rec = 100 * sum(actual) / sum(expected), ont = 100 * sum(on_time) / sum(expected)), by = period][order(period)][, date := ym_date(period)]
  plot_ly(r, x = ~date) |> add_lines(y = ~rec, name = "Reports received", line = list(color = "#201B6D", width = 2.4)) |>
    add_lines(y = ~ont, name = "On time", line = list(color = "#B0306A", width = 1.6, dash = "dash")) |>
    plotly_base(ytitle = "% of expected reports") |> layout(yaxis = list(ticksuffix = "%", range = c(0, 105)), legend = list(orientation = "h", y = 1.08))
}

tile_modal_server <- function(input, output, session, input_name = "tile") {
  ns <- session$ns; cur <- reactiveVal(NULL)
  fact <- function(lab, val, sub = NULL) div(class = "xp-fact", div(class = "xp-fact-lab", lab), div(class = "xp-fact-val", val), if (!is.null(sub)) div(class = "xp-fact-sub", sub))
  crd <- function(title, sub, out, h = "420px") card(class = "xp-card", card_header(title, if (!is.null(sub)) span(class = "sub", sub)), plotlyOutput(ns(out), height = h))
  observeEvent(input[[input_name]], {
    o <- input[[input_name]]; k <- o$key; cur(list(key = k, arg = o$arg, nonce = runif(1)))
    R <- META$region_uid; pop <- story_cen(R, "Population by Sex", "Total")
    body <- switch(k,
      population = list(title = "Busoga's population: every census and the years ahead", icon = "people-group",
        facts = list(fact("Census 2024", format(pop, big.mark = ","), "10 May 2024"), fact("Census 2014", format(CH[area == "Busoga" & year == 2014, pop], big.mark = ","), "27 August 2014"),
                     fact("Growth 2014-24", sprintf("%.1f%% a year", tail(census_rates("Busoga")$rate, 1)), sprintf("Uganda %.1f%%", tail(census_rates("Uganda")$rate, 1))),
                     fact("Central projection 2030", format(round(busoga_projection(2030)[grepl("^Central", scenario) & year == 2030, pop], -3), big.mark = ","), "see the assumptions below")),
        cards = list(crd("Census counts since 1980 and three projections", "hover for the numbers", "tm_1", "440px")),
        note = proj_assumptions()),
      households = list(title = "Households in Busoga", icon = "house",
        facts = list(fact("Households", format(story_cen(R, "Household Size", "Number of Households"), big.mark = ","), "census 2024"),
                     fact("People per household", sprintf("%.1f", story_cen(R, "Household Size", "Average Household Size")), sprintf("Uganda %.1f", SUBREG[subregion == "Uganda", hh_size]))),
        cards = list(crd("Households by district, and their average size", "bars = households; diamonds = people per household", "tm_1")), note = "UBOS National Population and Housing Census 2024."),
      ages = list(title = "A young population", icon = "baby",
        facts = list(fact("Under five", sprintf("%.1f%%", 100 * story_cen(R, "Age Groups", "Age 0-4") / pop), format(story_cen(R, "Age Groups", "Age 0-4"), big.mark = ",")),
                     fact("Under 18", sprintf("%.1f%%", 100 * story_cen(R, "Age Groups", "Age 0-17") / pop), format(story_cen(R, "Age Groups", "Age 0-17"), big.mark = ",")),
                     fact("60 and over", sprintf("%.1f%%", 100 * story_cen(R, "Age Groups", "Age 60+") / pop), format(story_cen(R, "Age Groups", "Age 60+"), big.mark = ","))),
        cards = list(crd("Age structure of each district and city", "share of the population, census 2024", "tm_1")), note = "UBOS National Population and Housing Census 2024 (age groups as published)."),
      facilities = list(title = "Health facilities in Busoga", icon = "hospital",
        facts = list(fact("Facilities", format(nrow(OU[level_name == "facility"]), big.mark = ","), "in the DHIS2 register"),
                     fact("Hospitals and HC IVs", nrow(OU[level_name == "facility" & grp_level %in% c("HC IV", "General Hospital", "RRH")]), "referral level"),
                     fact("HC IIIs", nrow(OU[level_name == "facility" & grp_level == "HC III"]), "one per sub-county is the national standard"),
                     fact("Per 10,000 people", sprintf("%.1f", 1e4 * nrow(OU[level_name == "facility"]) / pop), "census 2024")),
        cards = list(crd("Facilities by district and level", NULL, "tm_1")),
        note = "Ministry of Health DHIS2 organisation-unit register. The Place page (Busoga profile) maps every facility with distances to the nearest hospital or HC IV."),
      reporting = list(title = "Monthly reporting (HMIS 105:01)", icon = "clipboard-check",
        facts = list(fact("Facilities that report", format(uniqueN(REP[dataset == "RtEYsASU7PG" & actual > 0 & period >= DEFAULT_FROM, uid]), big.mark = ","), "last 12 months")),
        cards = list(crd("Reports received and on time, every month", NULL, "tm_1", "380px")), note = "Completeness = reports received / reports expected; on time = received by the 7th of the following month."),
      census = { m <- CENSUS_MEASURES[[o$arg]]; list(title = m$lab, icon = "house",
        facts = list(fact("Busoga", pct_txt(census_value(R, m)), "census 2024"), fact("Better is", if (m$good == "high") "higher" else "lower", NULL)),
        cards = list(layout_columns(col_widths = c(6, 6), crd("Districts and cities", "dashed line = Busoga", "tm_1", "420px"),
                                    crd("The 8 highest and 8 lowest sub-counties", NULL, "tm_2", "420px"))),
        note = "UBOS National Population and Housing Census 2024.") },
      NULL)
    req(body)
    showModal(modalDialog(
      title = div(class = "xp-title", style = "--th:#201B6D", span(class = "theme-badge", fontawesome::fa(body$icon, fill = "#fff", height = "1em")), div(div(class = "xp-h", body$title))),
      size = "xl", easyClose = TRUE, fade = TRUE,
      div(class = "xp-facts", style = "--th:#201B6D", body$facts), body$cards,
      if (!is.null(body$note)) div(class = "xp-def", body$note),
      footer = tags$button(type = "button", class = "btn btn-outline-secondary", `data-bs-dismiss` = "modal", "Close")))
  })
  output$tm_1 <- renderPlotly({
    x <- cur(); req(x)
    switch(x$key, population = pop_history_plot(2040), households = households_plot(), ages = ages_plot(), facilities = facility_plot(),
           reporting = reporting_plot(), census = census_rank_plot(x$arg))
  })
  output$tm_2 <- renderPlotly({ x <- cur(); req(x, x$key == "census"); census_sc_plot(x$arg) })
}
