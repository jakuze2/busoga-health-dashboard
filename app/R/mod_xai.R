# Explainable AI ("What drives the numbers?"). Results are computed in the monthly refresh
# (R/09_explain.R in the pipeline) and only displayed here. Every sentence on the page is written
# from those results, so the page updates by itself every month. Written for a lay reader first;
# the technical detail sits in the expandable sections at the bottom.

XAI <- if (file.exists(file.path(DATA, "xai.rds"))) readRDS(file.path(DATA, "xai.rds")) else NULL
XAI_COL  <- "#5B3E96"
XAI_UP   <- "#C0582B"      # pushed the number up
XAI_DOWN <- "#2F6FAE"      # pushed the number down (colours carry no good/bad judgement)
XAI_OWN  <- c("usual", "prev")

# plain-language names for the model inputs and outcomes
XAI_FACTOR <- c(usual = "What is usual for the area", prev = "Last month's level", month = "Time of year",
                rain0 = "Rain this month", rain1 = "Rain last month", rain2 = "Rain two months ago",
                spi3 = "Wet or dry spell (last 3 months)", tmax0 = "Daytime temperature this month",
                tmax1 = "Daytime temperature last month", heat = "Very hot days (above 32 °C)",
                complete = "How complete reporting was")
XAI_ICON <- c(usual = "house-medical", prev = "clock-rotate-left", month = "calendar-days", rain0 = "cloud-rain",
              rain1 = "cloud-rain", rain2 = "cloud-rain", spi3 = "droplet", tmax0 = "temperature-half",
              tmax1 = "temperature-half", heat = "sun", complete = "clipboard-check")
XAI_OUT <- list(
  malaria    = list(name = "Malaria cases", noun = "malaria cases", more = c("more", "fewer"), unit = "cases per 1,000 people a month"),
  positivity = list(name = "Malaria test positivity", noun = "malaria test positivity", more = c("higher", "lower"), unit = "of malaria tests positive"),
  opd        = list(name = "Outpatient visits", noun = "outpatient visits", more = c("more", "fewer"), unit = "visits per 1,000 people a month"),
  deliveries = list(name = "Births in health facilities", noun = "births in health facilities", more = c("more", "fewer"), unit = "births per 1,000 people a month"))

xai_pct <- function(phi) 100 * (exp(phi) - 1)                    # log-scale contribution -> % up or down
xai_agg <- function(sh, feats) { w <- pmax(sh$pop, 1)
  data.table(feature = feats, phi = vapply(feats, function(f) sum(sh[[f]] * w) / sum(w), 0)) }
xai_strength <- function(p) fcase(p >= 10, "Strong", p >= 4, "Moderate", default = "Small")
xai_lc <- function(x) sub("^(\\w)", "\\L\\1", x, perl = TRUE)

# one plain sentence about the direction of a factor
xai_direction <- function(f, d, O) {
  mo <- O$more; n <- O$noun
  if (d == "mixed") return("Matters, but not always in the same direction: its effect depends on the month and the place.")
  k <- if (d == "up") 1 else 2
  switch(f,
    spi3     = sprintf("Wetter spells go with %s %s; drier spells with %s.", mo[k], n, mo[3 - k]),
    rain0 = , rain1 = , rain2 = sprintf("More rain %s goes with %s %s.", sub("^rain ", "", xai_lc(XAI_FACTOR[[f]])), mo[k], n),
    tmax0 = , tmax1 = sprintf("Warmer days go with %s %s.", mo[k], n),
    heat     = sprintf("More very hot days go with %s %s.", mo[k], n),
    complete = sprintf("When more facilities report, %s %s are recorded: a reporting effect rather than a real change.", mo[k], n),
    month    = sprintf("Some months of the year have %s %s than others.", mo[1], n),
    sprintf("Higher values go with %s %s.", mo[k], n))
}

xai_ui <- function(id) {
  ns <- NS(id)
  if (is.null(XAI)) return(page_head("Insights", "What drives the numbers?", "Results appear after the next monthly data refresh.", key = "xai"))
  outs <- intersect(names(XAI_OUT), names(XAI$outcomes))
  tagList(
    page_head("AI insights · explainable AI", "What drives the numbers?",
              "A computer model studies five years of monthly data from every sub-county in Busoga to find which things (rain, heat, the time of year, and how complete reporting is) tend to go with rises and falls in key health figures. It is refreshed every month with the latest data, and everything on this page is rewritten from the new results.", key = "xai"),
    filter_bar(
      selectInput(ns("out"), "Show me", setNames(outs, vapply(XAI_OUT[outs], `[[`, "", "name")), width = "260px"),
      selectInput(ns("area"), "Where", c("All of Busoga" = "Busoga"), width = "200px"),
      selectInput(ns("month"), "Month", NULL, width = "160px")),
    uiOutput(ns("glance")),
    div(class = "xai-section",
        h3(class = "xai-h", "What is pushing the numbers"),
        p(class = "xai-lead", textOutput(ns("drivers_lead"), inline = TRUE)),
        uiOutput(ns("drivers"))),
    layout_columns(col_widths = c(6, 6),
      card(full_screen = TRUE, class = "xai-card",
           card_header(div(class = "xai-ch", textOutput(ns("wf_title"), inline = TRUE),
                           div(class = "xai-sub", "Bars to the right pushed the number up and bars to the left pushed it down, compared with a typical month."))),
           plotlyOutput(ns("wf"), height = "360px")),
      card(full_screen = TRUE, class = "xai-card",
           card_header(div(class = "xai-ch", textOutput(ns("ts_title"), inline = TRUE),
                           div(class = "xai-sub", "How the main factors pushed each of the last 12 months up or down."))),
           plotlyOutput(ns("ts"), height = "360px"))),
    layout_columns(col_widths = c(7, 5),
      card(full_screen = TRUE, class = "xai-card",
           card_header(div(class = "xai-ch", "Look closer at one factor", div(class = "xai-sub", textOutput(ns("dep_sub"), inline = TRUE))),
                       selectInput(ns("feat"), NULL, NULL, width = "250px")),
           plotlyOutput(ns("dep"), height = "320px"),
           checkboxInput(ns("pts"), "Show every sub-county month", FALSE)),
      card(full_screen = TRUE, class = "xai-card",
           card_header(div(class = "xai-ch", "Which factors matter most overall",
                           div(class = "xai-sub", "Average size of each factor's push over the last 12 months. Paler bars are less certain."))),
           plotlyOutput(ns("imp"), height = "320px"),
           checkboxInput(ns("own"), "Include the area's own past levels", FALSE))),
    div(class = "xai-caveat-card",
        div(class = "xai-caveat-icon", fontawesome::fa("scale-balanced", fill = "#7a4b00", height = "1.4em")),
        div(tags$b("Patterns, not proof. "),
            "The model shows which factors tend to move together with the numbers. It does not prove that one causes the other. Things the model cannot see, such as medicine stock-outs, health campaigns, bed-net distributions or people travelling for care, can also change the numbers. Use these findings to ask questions and to plan follow-up, together with local knowledge.")),
    accordion(id = ns("more"), open = FALSE, class = "xai-accordion",
      accordion_panel("How to read this page", icon = fontawesome::fa("book-open"),
        tags$dl(class = "xai-glossary",
          tags$dt("Push"), tags$dd("How much a factor moved a month's figure up or down, in percent, compared with a typical month. For example, a push of -20% means the factor on its own lowered the figure by about a fifth."),
          tags$dt("Typical month"), tags$dd("The model's average across all sub-counties and months. Each month starts from this level and every factor pushes it up or down."),
          tags$dt("Strong, moderate, small influence"), tags$dd("Strong: the factor usually moves the figure by 10% or more; moderate: 4-10%; small: under 4%."),
          tags$dt("Consistent pattern / less certain"), tags$dd("The model was rebuilt five times on different mixes of sub-counties. A pattern is 'consistent' when the factor came out among the three most important in at least four of the five; otherwise it is 'less certain'."),
          tags$dt("Wet or dry spell"), tags$dd("The Standardised Precipitation Index over the last three months (SPI-3): 0 is normal rainfall for the time of year, above 1 is unusually wet, below -1 is unusually dry."),
          tags$dt("What is usual for the area, last month's level"), tags$dd("Each sub-county's own average over the previous 12 months, and its figure in the month before. They explain most of the differences between places, so the cards above focus on the other factors."))),
      accordion_panel("How reliable is it?", icon = fontawesome::fa("gauge-high"), uiOutput(ns("reliability"))),
      accordion_panel("Technical details", icon = fontawesome::fa("gears"), uiOutput(ns("method"))))
  )
}

xai_server <- function(id) moduleServer(id, function(input, output, session) {
  if (is.null(XAI)) return(invisible())
  o  <- reactive({ req(input$out); XAI$outcomes[[input$out]] })
  O  <- reactive(XAI_OUT[[input$out]])
  observeEvent(o(), {
    x <- o(); d <- sort(unique(x$shap$district))
    updateSelectInput(session, "area", choices = c("All of Busoga" = "Busoga", setNames(d, d)),
                      selected = if (isTruthy(input$area) && input$area %in% c("Busoga", d)) input$area else "Busoga")
    m <- sort(unique(x$shap$period), decreasing = TRUE)
    updateSelectInput(session, "month", choices = setNames(m, format(ym_date(m), "%B %Y")), selected = m[1])
    ord <- x$importance[!feature %in% XAI_OWN, feature]
    updateSelectInput(session, "feat", choices = setNames(ord, XAI_FACTOR[ord]), selected = ord[1])
  })
  sh_area  <- reactive({ s <- o()$shap; if (isTruthy(input$area) && input$area != "Busoga") s <- s[district == input$area]; s })
  sh_month <- reactive({ req(input$month); sh_area()[period == as.integer(input$month)] })
  area_name <- reactive(if (isTruthy(input$area) && input$area != "Busoga") input$area else "Busoga")
  month_lab <- reactive(if (isTruthy(input$month)) format(ym_date(as.integer(input$month)), "%B %Y") else "")
  fmt_out <- function(v) if (input$out == "positivity") sprintf("%.1f%%", v) else format(round(v, 1), nsmall = 1)
  rating <- reactive({ x <- o(); best <- max(x$r2_usual, x$r2_prev)
    adds <- x$r2 >= best + 0.02
    list(level = if (x$r2 >= 0.75 && adds) "Good" else if (x$r2 >= 0.5) "Fair" else "Limited", adds = adds, r2 = x$r2) })

  # ---- at a glance ----
  output$glance <- renderUI({
    OO <- O(); m <- sh_month(); req(nrow(m))
    w <- pmax(m$pop, 1)
    obs <- if (input$out == "positivity") sum(m$y * w) / sum(w) else 1000 * sum(m$num) / sum(m$pop)
    usual <- sum(m$usual_level * w) / sum(w)
    diff <- if (is.finite(usual) && usual > 0) 100 * (obs - usual) / usual else NA
    ag <- xai_agg(m, names(XAI_FACTOR))[!feature %in% XAI_OWN][, pct := xai_pct(phi)][order(-abs(pct))]
    top <- ag[1]; r <- rating()
    rel_txt <- switch(r$level,
      Good = "It tracked most of the ups and downs in the last 12 months, which it had not seen before, and did better than simple rules of thumb.",
      Fair = "It tracked many of the ups and downs in the last 12 months, which it had not seen before.",
      "It tracked only some of the ups and downs in the last 12 months, so treat the findings with care.")
    if (!r$adds) rel_txt <- paste(rel_txt, "It predicts little better than repeating recent levels, so read it as a description of patterns, not a forecast.")
    div(class = "xai-glance",
      div(class = "xai-g-main",
        div(class = "xai-g-eyebrow", sprintf("%s · %s", area_name(), month_lab())),
        div(class = "xai-g-title", OO$name),
        div(class = "xai-g-big", fmt_out(obs), span(class = "xai-g-unit", OO$unit)),
        if (is.finite(diff)) div(class = "xai-g-line",
          span(class = paste("xai-pill", if (diff >= 0) "up" else "down"), sprintf("%s %.0f%%", if (diff >= 0) "▲" else "▼", abs(diff))),
          sprintf(" %s than usual for %s (the average of the previous 12 months).", if (diff >= 0) "Higher" else "Lower", area_name())),
        if (nrow(top) && abs(top$pct) >= 1) div(class = "xai-g-why",
          span(class = "xai-g-why-icon", fontawesome::fa(XAI_ICON[[top$feature]], fill = "#fff", height = "1em")),
          span(sprintf("Biggest factor this month: %s, which pushed the figure %s by about %.0f%%.", xai_lc(XAI_FACTOR[[top$feature]]),
                       if (top$pct >= 0) "up" else "down", abs(top$pct))))),
      div(class = "xai-g-side",
        div(class = "xai-g-eyebrow", "How reliable is the model?"),
        div(class = paste("xai-rating", tolower(r$level)), r$level),
        div(class = "xai-meter", div(class = "xai-meter-fill", style = sprintf("width:%.0f%%", 100 * max(0, min(1, r$r2))))),
        div(class = "xai-g-small", rel_txt),
        div(class = "xai-g-small xai-g-faint", sprintf("Model refreshed %s.", XAI$built))))
  })

  # ---- driver cards ----
  output$drivers_lead <- renderText(sprintf(
    "Most of the difference between places comes from what is usual for each sub-county and from last month's level. Beyond those, these are the factors that most often move %s up or down in %s.",
    O()$noun, area_name()))
  output$drivers <- renderUI({
    x <- o(); OO <- O(); s <- sh_area(); m <- sh_month()
    ext <- x$importance[!feature %in% XAI_OWN]
    avg <- data.table(feature = ext$feature, pct = vapply(ext$feature, function(f) xai_pct(mean(abs(s[[f]]))), 0))
    ext <- merge(ext, avg, by = "feature")[order(-pct)]
    pick <- head(rbind(ext[stable %in% TRUE], ext[!stable %in% TRUE]), 3)
    now <- if (nrow(m)) xai_agg(m, pick$feature)[, pct := xai_pct(phi)] else NULL
    cards <- lapply(seq_len(nrow(pick)), function(i) {
      f <- pick$feature[i]; st <- xai_strength(pick$pct[i]); ok <- isTRUE(pick$stable[i])
      nv <- if (!is.null(now)) now[feature == f, pct] else NA
      div(class = "xai-driver",
        div(class = "xai-d-top",
            div(class = "xai-d-icon", fontawesome::fa(XAI_ICON[[f]], fill = XAI_COL, height = "1.3em")),
            div(class = "xai-d-rank", sprintf("#%d", i))),
        div(class = "xai-d-name", XAI_FACTOR[[f]]),
        div(class = "xai-d-text", xai_direction(f, pick$direction[i], OO)),
        div(class = "xai-d-badges",
            span(class = paste("xai-badge", tolower(st)), sprintf("%s influence", st)),
            span(class = paste("xai-badge", if (ok) "ok" else "unsure"), if (ok) "Consistent pattern" else "Less certain")),
        if (length(nv) && is.finite(nv)) div(class = "xai-d-now", sprintf("In %s: ", month_lab()),
            if (abs(nv) < 1) span("little effect") else
            span(class = if (nv >= 0) "up" else "down", sprintf("pushed %s %.0f%%", if (nv >= 0) "up" else "down", abs(nv)))))
    })
    div(class = "xai-drivers", cards)
  })

  # ---- this month, factor by factor (diverging bars) ----
  output$wf_title <- renderText(sprintf("%s, %s: factor by factor", area_name(), month_lab()))
  output$wf <- renderPlotly({
    m <- sh_month(); validate(need(nrow(m) > 0, "No data for this month."))
    ag <- xai_agg(m, setdiff(names(XAI_FACTOR), XAI_OWN))[, pct := xai_pct(phi)]
    ag[, label := XAI_FACTOR[feature]]; setorder(ag, pct)
    ag[, `:=`(col = fifelse(pct >= 0, XAI_UP, XAI_DOWN), txt = fifelse(abs(pct) < 0.5, "0%", sprintf("%+.0f%%", pct)))]
    rng <- c(min(ag$pct, 0) * 1.25 - 3, max(ag$pct, 0) * 1.25 + 3)
    plot_ly(ag, y = ~factor(label, levels = label), x = ~pct, type = "bar", orientation = "h", marker = list(color = ~col),
            text = ~txt, textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "%{y}: pushed the figure %{text}<extra></extra>") |>
      plotly_base(xtitle = "← pushed down     compared with a typical month     pushed up →", legend = FALSE) |>
      layout(yaxis = list(title = "", tickfont = list(size = 12, color = BRAND$ink)),
             xaxis = list(ticksuffix = "%", zeroline = TRUE, zerolinecolor = BRAND$ink2, zerolinewidth = 1.5, range = rng),
             margin = list(l = 10, r = 40, t = 10, b = 10))
  })

  # ---- month by month ----
  output$ts_title <- renderText(sprintf("Month by month in %s", area_name()))
  output$ts <- renderPlotly({
    x <- o(); s <- sh_area(); validate(need(nrow(s) > 0, "No data."))
    feats <- head(x$importance[!feature %in% XAI_OWN, feature], 4)
    d <- rbindlist(lapply(split(s, s$period), function(z) xai_agg(z, feats)[, period := z$period[1]]))
    d[, `:=`(pct = xai_pct(phi), date = ym_date(period))]
    pal <- c("#5B3E96", "#2F9E8F", "#D9A13B", "#B5476B")
    g <- plot_ly()
    for (k in seq_along(feats)) g <- g |> add_bars(data = d[feature == feats[k]], x = ~date, y = ~pct, name = XAI_FACTOR[[feats[k]]],
                                                   marker = list(color = pal[k]), hovertemplate = "%{x|%B %Y}: %{y:+.0f}%<extra>%{fullData.name}</extra>")
    g |> plotly_base(ytitle = "Push up or down") |>
      layout(barmode = "relative", xaxis = list(tickformat = "%b %Y"), yaxis = list(ticksuffix = "%", zeroline = TRUE, zerolinecolor = BRAND$ink2),
             legend = list(orientation = "h", y = -0.18, font = list(size = 11)))
  })

  # ---- one factor ----
  unit_of <- function(f) switch(f, rain0 = , rain1 = , rain2 = "mm of rain", spi3 = "wet/dry index, 0 = normal", tmax0 = , tmax1 = "°C",
                                heat = "hot days", complete = "% of reports received", month = "month of the year", "")
  dep_data <- reactive({
    req(input$feat); x <- o(); f <- input$feat; s <- sh_area(); v <- x$values[s, on = .(uid, period)]
    d <- data.table(val = v[[f]], pct = xai_pct(s[[f]]), district = s$district, month = fmt_month(s$period))[is.finite(val)]
    br <- unique(quantile(d$val, seq(0, 1, 0.1), na.rm = TRUE))
    bm <- if (length(br) > 2) d[, .(val = median(val), pct = mean(pct), lo = quantile(pct, 0.2), hi = quantile(pct, 0.8)),
                                 by = .(b = cut(val, br, include.lowest = TRUE))][order(val)] else NULL
    list(d = d, bm = bm, f = f)
  })
  output$dep_sub <- renderText({
    z <- dep_data(); bm <- z$bm; req(!is.null(bm), nrow(bm) > 1); OO <- O()
    lo <- bm[which.min(pct)]; hi <- bm[which.max(pct)]
    if (max(abs(bm$pct)) < 1.5) return(sprintf("This factor makes little difference to %s: its push stays within about 1%% either way.", OO$noun))
    fv <- function(v) if (z$f == "month") month.name[max(1, min(12, round(v)))] else
      sprintf("%s %s", format(round(v, if (z$f == "spi3") 1 else 0), nsmall = if (z$f == "spi3") 1 else 0), sub(",.*", "", unit_of(z$f)))
    sprintf("At about %s, %s tend to be %.0f%% %s than in a typical month; at about %s, %.0f%% %s.",
            fv(hi$val), OO$noun, abs(hi$pct), if (hi$pct >= 0) "higher" else "lower", fv(lo$val), abs(lo$pct), if (lo$pct >= 0) "higher" else "lower")
  })
  output$dep <- renderPlotly({
    z <- dep_data(); validate(need(nrow(z$d) > 5 && !is.null(z$bm), "Not enough data.")); bm <- z$bm
    g <- plot_ly(bm, x = ~val) |>
      add_ribbons(ymin = ~lo, ymax = ~hi, fillcolor = "rgba(91,62,150,0.15)", line = list(width = 0), name = "most sub-county months", hoverinfo = "skip") |>
      add_lines(y = ~pct, line = list(color = XAI_COL, width = 3.5), name = "average push",
                hovertemplate = "around %{x:,.1f}: %{y:+.0f}%<extra></extra>")
    if (isTRUE(input$pts)) g <- g |> add_markers(data = z$d, x = ~val, y = ~pct, marker = list(color = XAI_COL, opacity = 0.18, size = 5),
                                                 text = ~sprintf("%s, %s", district, month), name = "sub-county months",
                                                 hovertemplate = "%{text}: %{y:+.0f}%<extra></extra>", inherit = FALSE)
    g |> plotly_base(xtitle = sprintf("%s (%s)", XAI_FACTOR[[z$f]], unit_of(z$f)), ytitle = "Push up or down") |>
      layout(yaxis = list(ticksuffix = "%", zeroline = TRUE, zerolinecolor = BRAND$ink2), legend = list(orientation = "h", y = -0.25))
  })

  # ---- overall importance ----
  output$imp <- renderPlotly({
    x <- o(); s <- sh_area()
    feats <- if (isTRUE(input$own)) names(XAI_FACTOR) else setdiff(names(XAI_FACTOR), XAI_OWN)
    ag <- data.table(feature = feats, v = vapply(feats, function(f) mean(abs(s[[f]])), 0))
    ag <- merge(ag, x$importance[, .(feature, stable)], by = "feature")
    ag[, `:=`(pct = xai_pct(v), label = XAI_FACTOR[feature])]; setorder(ag, pct)
    ag[, col := fifelse(stable %in% TRUE | feature %in% XAI_OWN, XAI_COL, "#cfc6e6")]
    plot_ly(ag, y = ~factor(label, levels = label), x = ~pct, type = "bar", orientation = "h", marker = list(color = ~col),
            text = ~sprintf("%.0f%%", pct), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "%{y}: moves the figure by about %{text} on average<extra></extra>") |>
      plotly_base(xtitle = "Average push, up or down", legend = FALSE) |>
      layout(yaxis = list(title = "", tickfont = list(size = 11.5, color = BRAND$ink)), xaxis = list(ticksuffix = "%"),
             margin = list(l = 10, r = 40, t = 10, b = 10))
  })

  output$reliability <- renderUI({
    x <- o(); r <- rating()
    tagList(
      p(sprintf("Before it was used, the model was tested on the last 12 months (%s to %s), which were hidden from it while it learned. It explained %.0f%% of the differences between sub-counties and months in that period, where 100%% would be perfect. For comparison, simply assuming each sub-county stays at its usual level explains %.0f%%, and assuming it repeats last month explains %.0f%%.",
                fmt_month(x$test_from), fmt_month(x$to), 100 * x$r2, 100 * x$r2_usual, 100 * x$r2_prev)),
      p(sprintf("Rating: %s. %s", r$level, if (r$adds) "The model does better than both simple rules, so the factors it picks out add real information."
                else "The model does little better than the simple rules, so the factors are best read as a description of past patterns.")),
      p("Each factor was also checked by rebuilding the model five times on different mixes of sub-counties. Factors marked 'Consistent pattern' came out near the top in at least four of the five."))
  })

  output$method <- renderUI({
    x <- o()
    tagList(
      p(sprintf("Data: %s sub-county months from %s to %s (%d sub-counties); HMIS monthly reports (DHIS2), CHIRPS rainfall and ERA5-Land temperature. Outcome: %s.",
                format(x$n_rows, big.mark = ","), fmt_month(x$from), fmt_month(x$to), x$n_subcounties, x$meta$label)),
      p("Model: a random forest of 300 decision trees fitted to log(1 + value), so factor effects multiply. Inputs: ", paste(XAI_FACTOR, collapse = "; "), "."),
      p("Explanations: Shapley (SHAP) values, estimated by permutation sampling, split each month's prediction into the contribution of each input relative to the model's average; a contribution c is shown as a push of 100 x (exp(c) - 1) percent. Areas and Busoga are population-weighted averages of their sub-counties."),
      p("Checks: out-of-sample R² (log scale) on the last 12 months against two benchmarks (usual level; last month's level). Stability: importance ranks among the factors other than the area's own past levels in 5 refits on sub-counties resampled with replacement; 'consistent' = top three in at least 4 of 5."),
      p("The model and all text on this page are rebuilt automatically in each monthly refresh (pipeline script R/09_explain.R)."))
  })
})
