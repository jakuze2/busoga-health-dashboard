# Population history and projections for Busoga, from the censuses of 1980, 1991, 2002, 2014 and
# 2024 (UBOS), and how Busoga compares with the other sub-regions of Uganda in 2024.
#
# Projection method (every assumption is shown on the page):
#   1. The average annual growth rate between two censuses is r = ln(P2 / P1) / (years between
#      census nights). This is the rate UBOS publishes; it already includes births, deaths and
#      net migration.
#   2. Busoga's rate has been falling: 3.1% (1980-91), 3.3% (1991-2002), 2.9% (2002-14), 2.0%
#      (2014-24). The trend in the rate is the straight-line slope through the last three
#      intercensal rates, placed at the mid-point of each period.
#   3. Three scenarios start from the 2014-24 rate on census night 2024:
#        high     the 2014-24 rate carries on unchanged
#        central  the rate keeps falling at half the historical slope
#        low      the rate keeps falling at the full historical slope
#      The rate is never allowed below 0.5% a year.
#   4. Districts: each district starts from its own 2014-24 rate and changes by the same amount per
#      year as Busoga; district results are then scaled so that they add up to the Busoga total.
#   5. Age and households: the 2024 census shares (children under five, under 18) and household
#      size are held constant.

CH <- local({ f <- file.path(DATA, "census_history.csv"); if (file.exists(f)) fread(f)[, census_date := as.Date(census_date)] })
SUBREG <- local({ f <- file.path(DATA, "subregions_2024.csv"); if (file.exists(f)) fread(f) })

census_rates <- function(area_ = "Busoga") {
  x <- CH[area == area_][order(census_date)]
  if (nrow(x) < 2) return(data.table())
  x[, .(from = year[-.N], to = year[-1], yrs = as.numeric(diff(census_date)) / 365.25,
        rate = 100 * log(pop[-1] / pop[-.N]) / (as.numeric(diff(census_date)) / 365.25),
        mid = as.numeric(format(census_date[-.N], "%Y")) + (as.numeric(diff(census_date)) / 365.25) / 2)]
}
proj_defaults <- function() {
  r <- census_rates("Busoga"); if (!nrow(r)) return(list(r0 = 2, slope = -0.03))
  last3 <- tail(r, 3); fit <- lm(rate ~ mid, last3)
  list(r0 = tail(r$rate, 1), slope = unname(coef(fit)[2]), rates = r)
}
# population on 1 July of each year, from census night 2024, with a rate that changes linearly
project_pop <- function(p0, r0, slope, to_year = 2050, t0 = as.Date("2024-05-10"), floor_rate = 0.5) {
  yrs <- 2024:to_year; days <- as.Date(sprintf("%d-07-01", yrs))
  t <- as.numeric(days - t0) / 365.25
  # integrate the instantaneous rate r(s) = max(floor, r0 + slope * s) numerically (monthly steps)
  pop <- vapply(t, function(tt) {
    if (tt <= 0) return(p0 * exp(r0 / 100 * tt))
    s <- seq(0, tt, length.out = max(2, ceiling(tt * 12)))
    r <- pmax(floor_rate, r0 + slope * s) / 100
    p0 * exp(sum(diff(s) * (head(r, -1) + tail(r, -1)) / 2))
  }, 0)
  data.table(year = yrs, pop = pop, rate = pmax(floor_rate, r0 + slope * pmax(0, t)))
}
busoga_projection <- function(to_year = 2050, r0 = NULL, slope = NULL) {
  d <- proj_defaults(); r0 <- r0 %||% d$r0; slope <- slope %||% d$slope
  p0 <- CH[area == "Busoga" & year == 2024, pop]
  rbindlist(list(
    project_pop(p0, r0, 0, to_year)[, scenario := "High: 2014-24 growth continues"],
    project_pop(p0, r0, slope / 2, to_year)[, scenario := "Central: growth keeps slowing, at half the past pace"],
    project_pop(p0, r0, slope, to_year)[, scenario := "Low: growth keeps slowing at the past pace"]))
}
district_projection <- function(year_, r0 = NULL, slope = NULL) {
  d <- proj_defaults(); slope <- slope %||% d$slope
  dd <- CH[level == "district" & year %in% c(2014, 2024)]
  dd <- dcast(dd, area ~ year, value.var = "pop"); setnames(dd, c("area", "p14", "p24"))
  dd[, rate := 100 * log(p24 / p14) / (as.numeric(as.Date("2024-05-10") - as.Date("2014-08-27")) / 365.25)]
  dd[, proj := vapply(seq_len(.N), function(i) { x <- project_pop(p24[i], rate[i], slope / 2, year_); x[year == year_, pop] }, 0)]
  tot <- busoga_projection(year_, r0, slope)[grepl("^Central", scenario) & year == year_, pop]
  dd[, proj := proj * tot / sum(proj)][]
}
PROJ_COLS <- c("High: 2014-24 growth continues" = "#C17D11", "Central: growth keeps slowing, at half the past pace" = "#B0306A",
               "Low: growth keeps slowing at the past pace" = "#3949AB")

# the census-and-projection chart, shared by the Place page and the Busoga in focus page
pop_history_plot <- function(to_year = 2050, r0 = NULL, slope = NULL, show_refs = TRUE) {
  h <- CH[area == "Busoga"][order(year)]
  pr <- busoga_projection(to_year, r0, slope)
  band <- dcast(pr, year ~ scenario, value.var = "pop"); nm <- names(PROJ_COLS)
  r <- census_rates("Busoga")
  p <- plot_ly() |>
    add_ribbons(data = band, x = ~year, ymin = band[[nm[3]]], ymax = band[[nm[1]]], name = "Range of the three scenarios",
                fillcolor = "rgba(176,48,106,.12)", line = list(width = 0), hoverinfo = "skip")
  for (s in nm) p <- p |> add_lines(data = pr[scenario == s], x = ~year, y = ~pop, name = s,
                                    line = list(color = PROJ_COLS[[s]], width = if (grepl("^Central", s)) 3 else 1.6, dash = if (grepl("^Central", s)) "solid" else "dash"),
                                    hovertemplate = paste0(s, "<br>%{x}: %{y:,.0f}<extra></extra>"))
  if (show_refs && exists("CEN") && !is.null(CEN)) {
    m <- CEN$region[source == "DHIS2 107a projection (MoH)"][order(year)]
    if (nrow(m)) p <- p |> add_lines(data = m, x = ~year, y = ~pop, name = "Ministry of Health projection used in DHIS2 (107a)",
                                     line = list(color = "#9b9a94", width = 1.4, dash = "dot"), hovertemplate = "MoH 107a %{x}: %{y:,.0f}<extra></extra>")
  }
  p <- p |> add_trace(data = h, x = ~year, y = ~pop, type = "scatter", mode = "lines+markers+text", name = "Census count (UBOS)",
                      line = list(color = "#201B6D", width = 2.6), marker = list(symbol = "diamond", size = 13, color = "#201B6D", line = list(color = "#fff", width = 1.5)),
                      text = ~sprintf("%.2fm", pop / 1e6), textposition = "top left", textfont = list(size = 11, color = "#201B6D"),
                      hovertemplate = "Census %{x}: %{y:,.0f}<extra></extra>")
  ann <- lapply(seq_len(nrow(r)), function(i) list(x = r$mid[i], y = mean(h$pop[i + 0:1]), text = sprintf("%.1f%% a year", r$rate[i]),
                                                   showarrow = FALSE, yshift = -22, font = list(size = 10.5, color = "#6a6879")))
  p |> plotly_base(ytitle = "People", xtitle = "Year") |>
    layout(yaxis = list(tickformat = ",.1s", rangemode = "tozero"), xaxis = list(dtick = 5), annotations = ann,
           shapes = list(list(type = "line", x0 = 2024.4, x1 = 2024.4, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#bbb", dash = "dot"))),
           legend = list(orientation = "h", y = -0.2, font = list(size = 10.5)))
}
proj_assumptions <- function(r0 = NULL, slope = NULL) {
  d <- proj_defaults(); r0 <- r0 %||% d$r0; slope <- slope %||% d$slope
  r <- d$rates; u <- census_rates("Uganda")
  tagList(
    tags$ol(class = "pa-list",
      tags$li(tags$b("Census counts are the starting point. "), sprintf("Busoga had %s people on census night in 1980, %s in 1991, %s in 2002, %s in 2014 and %s on 10 May 2024 (UBOS). All figures are for today's Busoga area: the 12 districts and cities, which were 5 districts in 1980.",
              format(CH[area == "Busoga" & year == 1980, pop], big.mark = ","), format(CH[area == "Busoga" & year == 1991, pop], big.mark = ","),
              format(CH[area == "Busoga" & year == 2002, pop], big.mark = ","), format(CH[area == "Busoga" & year == 2014, pop], big.mark = ","),
              format(CH[area == "Busoga" & year == 2024, pop], big.mark = ","))),
      tags$li(tags$b("Growth between censuses. "), sprintf("Average annual growth, r = ln(P2/P1) / years between census nights: %s. Uganda as a whole grew at %.1f%% a year in 2014-24, so Busoga is growing more slowly than the country (the 2024 census recorded about 84,000 more people moving out of Busoga than moving in, net migration of -1.7%%).",
              paste(sprintf("%.1f%% in %d-%s", r$rate, r$from, substr(r$to, 3, 4)), collapse = ", "), tail(u$rate, 1))),
      tags$li(tags$b("Growth keeps slowing. "), sprintf("The growth rate has fallen by about %.2f percentage points a year since the 1990s (straight line through the last three intercensal rates). Projections start at the 2014-24 rate, %.2f%% a year.", -d$slope, d$r0)),
      tags$li(tags$b("Three scenarios. "), sprintf("High: %.2f%% a year continues. Central: the rate falls by %.3f points a year (half the past pace). Low: it falls by %.3f points a year (the past pace). The rate never goes below 0.5%%.",
              r0, -slope / 2, -slope)),
      tags$li(tags$b("What is held constant. "), "Children under five and under 18 keep their 2024 census shares, and households keep the 2024 average size of 4.4 people. In reality both are likely to fall slowly as fertility declines, so child numbers are probably slight overestimates in later years."),
      tags$li(tags$b("Districts. "), "Each district starts from its own 2014-24 growth rate (with the 2014 census re-counted on today's boundaries for Bugweri and Jinja City) and slows at the same pace as Busoga; the district figures are then scaled to add up to the central Busoga projection."),
      tags$li(tags$b("Not a substitute for official figures. "), "UBOS will publish official projections on the 2024 census base; the Ministry of Health's 107a figures (the dotted grey line) are what DHIS2 uses as denominators. This projection is for planning discussions and shows how sensitive the future is to the pace at which growth slows.")))
}
