# Forecast: seasonal time-series projections of any indicator, any area, with honest uncertainty.
# Model: seasonal ARIMA chosen by AIC from a small candidate set (base R stats::arima), fitted
# on log(1 + x) for counts. Optional climate covariate: monthly rainfall (lagged), using the
# ECMWF seasonal outlook for the forecast months. Accuracy: rolling-origin back-test.

SARIMA_CANDIDATES <- list(
  list(order = c(1, 0, 0), seasonal = c(0, 1, 1)), list(order = c(0, 1, 1), seasonal = c(0, 1, 1)),
  list(order = c(1, 0, 0), seasonal = c(1, 0, 0)), list(order = c(1, 1, 1), seasonal = c(0, 1, 1)),
  list(order = c(2, 0, 0), seasonal = c(1, 0, 0)), list(order = c(1, 0, 1), seasonal = c(0, 0, 0)))

fit_best <- function(y, xreg = NULL, candidates = SARIMA_CANDIDATES) {
  best <- NULL
  for (cand in candidates) {
    f <- tryCatch(suppressWarnings(arima(y, order = cand$order, seasonal = list(order = cand$seasonal, period = 12),
                                         xreg = xreg, method = "ML")), error = function(e) NULL)
    if (!is.null(f) && is.finite(f$aic) && (is.null(best) || f$aic < best$aic)) best <- f
  }
  best
}

forecast_series <- function(v, h = 6, log_scale = TRUE, xreg = NULL, xreg_new = NULL, candidates = SARIMA_CANDIDATES) {
  y <- if (log_scale) log1p(pmax(v, 0)) else v
  y <- ts(y, frequency = 12)
  f <- fit_best(y, xreg, candidates)
  if (is.null(f)) return(NULL)
  p <- predict(f, n.ahead = h, newxreg = xreg_new)
  back <- if (log_scale) function(z) pmax(expm1(z), 0) else identity
  list(fit = f, mean = back(p$pred), lo80 = back(p$pred - 1.2816 * p$se), hi80 = back(p$pred + 1.2816 * p$se),
       lo95 = back(p$pred - 1.96 * p$se), hi95 = back(p$pred + 1.96 * p$se), model = sprintf("ARIMA(%s)(%s)[12]%s",
       paste(f$arma[c(1, 6, 2)], collapse = ","), paste(f$arma[c(3, 7, 4)], collapse = ","), if (!is.null(xreg)) " + rainfall" else ""))
}

# rolling-origin back-test: forecast each of the last k months from the data before it,
# re-fitting the selected model specification each time
backtest <- function(v, fit, k = 12, log_scale = TRUE) {
  n <- length(v); if (n < 36) return(NA_real_)
  spec <- list(list(order = fit$arma[c(1, 6, 2)], seasonal = fit$arma[c(3, 7, 4)]))
  errs <- vapply(seq_len(k), function(i) {
    cut <- n - k + i - 1
    f <- forecast_series(v[seq_len(cut)], h = 1, log_scale = log_scale, candidates = spec)
    if (is.null(f)) NA_real_ else abs(f$mean[1] - v[cut + 1]) / max(abs(v[cut + 1]), 1)
  }, 0)
  100 * mean(errs, na.rm = TRUE)
}

forecast_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Forecast", "What the next months are likely to look like",
              "Seasonal time-series models project any indicator for any area, with 80% and 95% prediction intervals. The back-test shows how accurate the model has been on the last 12 months, so you can judge how much to trust it."),
    filter_bar(
      selectInput(ns("ind"), "Indicator", indicator_choices(), selected = "MAL09", width = "320px"),
      area_ui(ns("area"), depth = 4),
      selectInput(ns("h"), "Months ahead", c(3, 6, 9, 12), selected = 6, width = "120px"),
      checkboxInput(ns("rain"), "Use rainfall as a leading signal (malaria)", FALSE)),
    layout_columns(col_widths = c(9, 3),
      card(full_screen = TRUE, card_header(textOutput(ns("title"), inline = TRUE)), plotlyOutput(ns("plot"), height = 460)),
      card(card_header("Model"), uiOutput(ns("model")))),
    info_note("Forecasts extend recent patterns and seasonality; they cannot anticipate stock-outs, campaigns, outbreaks or changes in reporting. ",
              "The most recent month is excluded from fitting because late reports are still arriving.")
  )
}

forecast_server <- function(id) moduleServer(id, function(input, output, session) {
  area <- area_server("area")
  series <- reactive({
    a <- area(); cd <- input$ind
    if (a$level == "facility" && IND[code == cd, area_only]) return(NULL)
    s <- summarise_ind(cd, a$level, MONTH_MIN, MONTH_MAX, uids = a$uid, by = "month")[order(bucket)]
    s <- s[bucket < MONTH_MAX]                              # drop the still-incomplete last month
    full <- data.table(bucket = month_seq(MONTH_MIN, max(s$bucket, MONTH_MIN)))
    s <- merge(full, s[, .(bucket, value)], by = "bucket", all.x = TRUE)
    s[, value := nafill(value, "locf")]; s <- s[!is.na(value)]
    s
  })
  fc <- reactive({
    s <- series(); req(s, nrow(s) >= 30)
    cd <- input$ind; h <- as.integer(input$h)
    log_scale <- IND[code == cd, unit] %in% c("count", "ratio")
    xr <- NULL; xn <- NULL
    if (isTRUE(input$rain) && exists("CLIM") && !is.null(CLIM)) {
      a <- area()
      cu <- if (a$level == "facility") OU$uid_l5[OU$uid == a$uid] else a$uid
      cl <- CLIM[uid == cu][order(period)]
      if (nrow(cl)) {
        rain <- cl$rain_mm[match(s$bucket, cl$period)]
        lag1 <- c(NA, head(rain, -1))
        ok <- !is.na(lag1)
        if (sum(ok) >= 30) {
          s <- s[ok]; xr <- log1p(lag1[ok])
          dist_u <- if (a$level %in% c("region", "district")) a$uid else OU$uid_l3[OU$uid == a$uid]
          fut <- if (exists("OUTLOOK") && !is.null(OUTLOOK)) OUTLOOK[uid == dist_u][order(period)] else NULL
          last_rain <- tail(rain[!is.na(rain)], 1)
          fr <- c(last_rain, if (!is.null(fut) && nrow(fut)) fut$rain_mm else rep(NA, h))[seq_len(h)]
          fr[is.na(fr)] <- mean(rain, na.rm = TRUE)
          xn <- log1p(fr)
        }
      }
    }
    f <- forecast_series(s$value, h, log_scale, xr, xn)
    req(f)
    list(s = s, f = f, h = h, mape = backtest(s$value, f$fit, 12, log_scale),
         future = date_ym(seq(ym_date(max(s$bucket)), by = "month", length.out = h + 1)[-1]))
  })
  output$title <- renderText(sprintf("%s · %s · next %s months", ind_label(input$ind), area()$name, input$h))
  output$plot <- renderPlotly({
    d <- fc(); col <- theme_col(IND[code == input$ind, theme])
    fx <- ym_date(d$future); hx <- ym_date(d$s$bucket)
    plot_ly() |>
      add_ribbons(x = fx, ymin = d$f$lo95, ymax = d$f$hi95, name = "95% interval", fillcolor = paste0(col, "22"),
                  line = list(width = 0), hoverinfo = "skip") |>
      add_ribbons(x = fx, ymin = d$f$lo80, ymax = d$f$hi80, name = "80% interval", fillcolor = paste0(col, "44"),
                  line = list(width = 0), hoverinfo = "skip") |>
      add_lines(x = hx, y = d$s$value, name = "Observed", line = list(color = BRAND$ink2, width = 1.8),
                hovertemplate = "%{x|%b %Y}: %{y:,.1f}<extra>Observed</extra>") |>
      add_lines(x = c(tail(hx, 1), fx), y = c(tail(d$s$value, 1), d$f$mean), name = "Forecast",
                line = list(color = col, width = 2.5, dash = "dash"), marker = list(size = 8),
                hovertemplate = "%{x|%b %Y}: %{y:,.1f}<extra>Forecast</extra>") |>
      plotly_base(ytitle = unit_label(input$ind)) |> layout(hovermode = "x unified")
  })
  output$model <- renderUI({
    d <- fc()
    acc <- if (is.na(d$mape)) "not enough history" else sprintf("%.0f%%", d$mape)
    grade <- if (is.na(d$mape)) "" else if (d$mape < 10) "good" else if (d$mape < 25) "fair" else "weak: treat as indicative only"
    tagList(
      div(class = "stat-row", style = "flex-direction:column;gap:.6rem",
          div("Model", tags$b(style = "font-size:.95rem", d$f$model)),
          div("Back-test error (last 12 months, one step ahead)", tags$b(acc), span(class = "muted", grade)),
          div(sprintf("Forecast for %s", fmt_month(d$future[1])), tags$b(fmt_val(d$f$mean[1], input$ind)),
              span(class = "muted", sprintf("80%%: %s to %s", fmt_val(d$f$lo80[1], input$ind), fmt_val(d$f$hi80[1], input$ind)))),
          div(sprintf("Forecast for %s", fmt_month(tail(d$future, 1))), tags$b(fmt_val(tail(d$f$mean, 1), input$ind)),
              span(class = "muted", sprintf("80%%: %s to %s", fmt_val(tail(d$f$lo80, 1), input$ind), fmt_val(tail(d$f$hi80, 1), input$ind))))),
      downloadButton(session$ns("dl"), "Download forecast", class = "btn-sm btn-outline-secondary mt-2"))
  })
  output$dl <- downloadHandler(
    filename = function() sprintf("busoga_forecast_%s.csv", input$ind),
    content = function(f) { d <- fc()
      fwrite(data.table(area = area()$name, indicator = ind_label(input$ind), month = fmt_month(d$future),
                        forecast = round(d$f$mean, 2), lo80 = round(d$f$lo80, 2), hi80 = round(d$f$hi80, 2),
                        lo95 = round(d$f$lo95, 2), hi95 = round(d$f$hi95, 2), model = d$f$model), f) })
})
