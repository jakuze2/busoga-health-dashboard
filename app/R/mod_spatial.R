# Spatial analysis: the pattern-finding tools of a GIS, applied to routine health data for the
# sub-counties (or DLGs) of Busoga, each with a plain-language reading of what the map shows.
#   * Hot spot analysis (Getis-Ord Gi*): where high or low values cluster, with confidence levels
#   * Cluster and outlier analysis (Anselin Local Moran's I, 999 permutations): high-high and
#     low-low clusters, and areas that differ sharply from their neighbours
#   * Emerging hot spots: Gi* for every calendar year plus a Mann-Kendall trend test, giving new,
#     intensifying, persistent, diminishing, sporadic and historical hot and cold spots
#   * Change: where the indicator improved or worsened against the previous period, and where the
#     worsening is clustered
#   * Bivariate: the indicator against a possible driver (census 2024 living conditions, rainfall,
#     heat, facilities per head), with a 3 x 3 bivariate map and a rank correlation
# Neighbours are areas that share a border (queen contiguity); islands are joined to their three
# nearest areas. Everything is computed here in base R and sf, so no extra packages are needed.

SP_LEVELS <- c("Sub-counties / divisions" = "subcounty", "DLGs / municipalities" = "dlg")
SP_W <- local({
  mk <- function(g) {
    old <- sf::sf_use_s2(); suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(old)))
    nb <- suppressMessages(sf::st_intersects(g, g)); n <- nrow(g); W <- matrix(0, n, n, dimnames = list(g$uid, g$uid))
    for (i in seq_len(n)) W[i, setdiff(nb[[i]], i)] <- 1
    cen <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(g))))
    for (i in which(rowSums(W) == 0)) {
      d <- sqrt((cen[, 1] - cen[i, 1])^2 + (cen[, 2] - cen[i, 2])^2); d[i] <- Inf
      k <- order(d)[1:3]; W[i, k] <- 1; W[k, i] <- 1
    }
    W
  }
  list(subcounty = mk(GEO$subcounty), dlg = mk(GEO$dlg))
})

# ---- statistics ---------------------------------------------------------------------------------
gi_star <- function(x, W) {
  n <- length(x); Ws <- W; diag(Ws) <- 1
  xb <- mean(x); S <- sqrt(sum(x^2) / n - xb^2); if (!is.finite(S) || S == 0) return(rep(0, n))
  sw <- rowSums(Ws); sw2 <- rowSums(Ws^2)
  as.numeric((Ws %*% x - xb * sw) / (S * sqrt((n * sw2 - sw^2) / (n - 1))))
}
gi_class <- function(z) cut(z, c(-Inf, -2.576, -1.960, -1.645, 1.645, 1.960, 2.576, Inf),
                            labels = c("Cold spot, 99% confidence", "Cold spot, 95% confidence", "Cold spot, 90% confidence", "Not significant",
                                       "Hot spot, 90% confidence", "Hot spot, 95% confidence", "Hot spot, 99% confidence"))
GI_COLS <- c("Cold spot, 99% confidence" = "#2166ac", "Cold spot, 95% confidence" = "#67a9cf", "Cold spot, 90% confidence" = "#d1e5f0",
             "Not significant" = "#f2f1ee", "Hot spot, 90% confidence" = "#fddbc7", "Hot spot, 95% confidence" = "#ef8a62", "Hot spot, 99% confidence" = "#b2182b")
local_moran <- function(x, W, nsim = 999, seed = 42) {
  set.seed(seed); n <- length(x); z <- (x - mean(x)) / sd(x); k <- rowSums(W); Wr <- W / k
  lag <- as.numeric(Wr %*% z); Ii <- z * lag
  p <- vapply(seq_len(n), function(i) {
    sims <- replicate(nsim, mean(sample(z[-i], k[i])))
    (sum(abs(z[i] * sims) >= abs(Ii[i])) + 1) / (nsim + 1)
  }, 0)
  q <- ifelse(z > 0 & lag > 0, "High-high cluster", ifelse(z < 0 & lag < 0, "Low-low cluster",
       ifelse(z > 0 & lag < 0, "High outlier among low", "Low outlier among high")))
  gI <- sum(z * lag) / sum(z^2)
  gsim <- replicate(nsim, { zz <- sample(z); sum(zz * as.numeric(Wr %*% zz)) / sum(zz^2) })
  list(z = z, lag = lag, I = Ii, p = p, class = ifelse(p < 0.05, q, "Not significant"),
       global = gI, global_p = (sum(abs(gsim - mean(gsim)) >= abs(gI - mean(gsim))) + 1) / (nsim + 1))
}
LM_COLS <- c("High-high cluster" = "#b2182b", "Low-low cluster" = "#2166ac", "High outlier among low" = "#f4a582",
             "Low outlier among high" = "#92c5de", "Not significant" = "#f2f1ee")
mann_kendall <- function(y) {
  y <- y[is.finite(y)]; n <- length(y); if (n < 4) return(0)
  s <- sum(vapply(1:(n - 1), function(i) sum(sign(y[(i + 1):n] - y[i])), 0))
  v <- n * (n - 1) * (2 * n + 5) / 18
  if (s == 0) 0 else (s - sign(s)) / sqrt(v)
}
emerging_class <- function(zs) {                 # zs: Gi* z by year, oldest first
  one <- function(hot) {
    n <- length(hot); fin <- hot[n]; share <- mean(hot); prior <- hot[-n]
    run <- rev(cumprod(rev(hot))); run_len <- sum(run)
    if (fin) {
      if (!any(prior)) return("New")
      if (run_len >= 2 && run_len < n && !any(hot[seq_len(n - run_len)])) return("Consecutive")
      if (share >= 0.9) return("Persistent")
      return("Sporadic")
    }
    if (mean(prior) >= 0.9) return("Historical")
    NA_character_
  }
  hot <- zs >= 1.96; cold <- zs <= -1.96; tr <- mann_kendall(zs)
  h <- one(hot); c <- one(cold)
  if (!is.na(h)) { if (h == "Persistent" && tr >= 1.96) h <- "Intensifying"; if (h == "Persistent" && tr <= -1.96) h <- "Diminishing"; return(paste(h, "hot spot")) }
  if (!is.na(c)) { if (c == "Persistent" && tr <= -1.96) c <- "Intensifying"; if (c == "Persistent" && tr >= 1.96) c <- "Diminishing"; return(paste(c, "cold spot")) }
  "No pattern"
}
EM_COLS <- c("New hot spot" = "#fcbba1", "Consecutive hot spot" = "#fb6a4a", "Intensifying hot spot" = "#a50f15", "Persistent hot spot" = "#cb181d",
             "Diminishing hot spot" = "#fc9272", "Sporadic hot spot" = "#fee0d2", "Historical hot spot" = "#d9b3a6",
             "New cold spot" = "#c6dbef", "Consecutive cold spot" = "#6baed6", "Intensifying cold spot" = "#08306b", "Persistent cold spot" = "#2171b5",
             "Diminishing cold spot" = "#9ecae1", "Sporadic cold spot" = "#deebf7", "Historical cold spot" = "#b3c3d6", "No pattern" = "#f2f1ee")
BIV_COLS <- c("1-1" = "#e8e8e8", "2-1" = "#e4acac", "3-1" = "#c85a5a", "1-2" = "#b0d5df", "2-2" = "#ad9ea5", "3-2" = "#985356",
              "1-3" = "#64acbe", "2-3" = "#627f8c", "3-3" = "#574249")

# possible drivers for the bivariate view, per area uid
sp_driver_choices <- function() c(
  setNames(paste0("census:", names(CENSUS_MEASURES)), paste("Census 2024:", vapply(CENSUS_MEASURES, `[[`, "", "lab"))),
  "Average monthly rainfall (mm)" = "rain", "Days above 35°C per month" = "heat", "Health facilities per 10,000 people" = "facdens")
sp_driver <- function(key, lvl, uids, p) {
  if (startsWith(key, "census:")) {
    m <- CENSUS_MEASURES[[sub("census:", "", key)]]
    return(vapply(uids, function(u) census_value(u, m), 0))
  }
  if (key %in% c("rain", "heat")) {
    cl <- readRDS_cached("climate.rds")
    col <- if (key == "rain") "rain_mm" else "heat_days"
    x <- cl[level == lvl & period >= p$from & period <= p$to, .(v = mean(get(col), na.rm = TRUE)), by = uid]
    return(x$v[match(uids, x$uid)])
  }
  if (key == "facdens") {
    f <- OU[level_name == "facility" & is.na(closed_date)]
    n <- f[, .N, by = .(uid = if (lvl == "subcounty") uid_l5 else uid_l4)]
    pp <- POP[level == lvl & year == p$to %/% 100L, .(uid, pop)]
    x <- merge(n, pp, by = "uid"); x[, v := 1e4 * N / pop]
    return(x$v[match(uids, x$uid)])
  }
  rep(NA_real_, length(uids))
}
readRDS_cached <- local({ cache <- list(); function(f) { if (is.null(cache[[f]])) cache[[f]] <<- readRDS(file.path(DATA, f)); cache[[f]] } })

SP_TOOLS <- c("Hot spot analysis" = "hot", "Cluster and outlier analysis" = "lisa", "Emerging hot spots (trend over years)" = "emerging",
              "Change since the previous period" = "change", "Compare with a possible driver (bivariate)" = "biv")
SP_ABOUT <- list(
  hot = "Getis-Ord Gi* compares each area and its neighbours with Busoga as a whole. A hot spot is a group of neighbouring areas with high values, a cold spot a group with low values; the confidence level says how unlikely the grouping is to be chance.",
  lisa = "Anselin Local Moran's I finds clusters (an area and its neighbours are all high, or all low) and outliers (an area that is high while its neighbours are low, or the reverse). Significance comes from 999 random reshuffles of the map.",
  emerging = "Hot spot analysis is repeated for every complete calendar year, and a Mann-Kendall test checks whether the hot spot score is rising or falling. New, intensifying, persistent, diminishing, sporadic and historical patterns follow the definitions used in GIS software.",
  change = "Each area's value in the selected period is compared with the period of the same length just before it. Areas outlined in black sit in a significant cluster of worsening (Gi*, 95% confidence).",
  biv = "Two measures on one map: the indicator (left to right in the key) and a possible driver (bottom to top), each split into thirds. Dark purple areas are high on both, grey low on both. The correlation shows how strongly they move together; it does not show cause.")

spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Maps · spatial analysis", "Spatial analysis: hot spots, clusters and trends",
              "The pattern-finding tools of a GIS, applied to routine health data: where high and low values cluster, which areas stand out from their neighbours, where patterns are emerging or fading over the years, where things are getting worse, and what else varies with them. Each map comes with a plain-language reading generated from the statistics.", key = "spatial"),
    filter_bar(
      selectInput(ns("tool"), "Analysis", SP_TOOLS, selected = "hot", width = "300px"),
      selectInput(ns("ind"), "Indicator", indicator_choices(), selected = "MAL10", width = "320px"),
      selectInput(ns("level"), "Areas", SP_LEVELS, width = "200px"),
      period_ui(ns("period")),
      conditionalPanel(sprintf("input['%s'] == 'biv'", ns("tool")),
                       selectInput(ns("driver"), "Possible driver", sp_driver_choices(), selected = "census:water", width = "330px"))),
    layout_columns(col_widths = c(7, 5), class = "sp-grid",
      card(full_screen = TRUE, class = "sp-map-card",
           card_header(div(class = "xai-ch", textOutput(ns("title"), inline = TRUE), div(class = "xai-sub", textOutput(ns("subtitle"), inline = TRUE)))),
           leafletOutput(ns("map"), height = 640),
           div(class = "map-legend-note", "Boundaries: DHIS2 (Ministry of Health). Basemaps © Esri. Click an area for its trend.")),
      div(class = "sp-side",
          div(class = "sp-insight", div(class = "sp-insight-h", fontawesome::fa("wand-magic-sparkles", fill = "#fff", height = ".95em"), "What the map shows"),
              uiOutput(ns("insight")),
              div(class = "sp-insight-foot", "Generated automatically from the statistics on this page. Check against local knowledge before quoting.")),
          uiOutput(ns("stats")),
          card(class = "xai-card", card_header(textOutput(ns("chart_title"), inline = TRUE)), plotlyOutput(ns("chart"), height = "330px")))),
    layout_columns(col_widths = c(7, 5),
      card(class = "xai-card", card_header("Areas that stand out", span(class = "sub", "significant areas first; click a row's area on the map for its trend")),
           tableOutput(ns("table"))),
      card(class = "xai-card", card_header(textOutput(ns("sel_title"), inline = TRUE)), plotlyOutput(ns("trend"), height = "300px"))),
    div(class = "sp-method", tags$b("Method. "), textOutput(ns("about"), inline = TRUE),
        " Neighbours are areas sharing a border (islands are linked to their three nearest areas). Areas with no data for the period are left out of the calculation.")
  )
}

spatial_server <- function(id) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  per <- period_server("period")
  selected <- reactiveVal(NULL)
  session$userData$go_spatial <- function(cd) updateSelectInput(session, "ind", selected = cd)
  dirn <- reactive(IND[code == input$ind, direction])

  base <- reactive({
    p <- per(); lvl <- input$level; cd <- input$ind
    g <- GEO[[lvl]]; W <- SP_W[[lvl]]
    s <- summarise_ind(cd, lvl, p$from, p$to)
    d <- data.table(uid = g$uid)[s[, .(uid, value)], on = "uid", value := i.value]
    d[, name := ou_name[uid]][, district := OU$district[match(uid, OU$uid)]]
    list(p = p, lvl = lvl, cd = cd, g = g, W = W, d = d)
  })

  res <- reactive({
    b <- base(); tool <- input$tool; d <- copy(b$d); p <- b$p
    ok <- which(is.finite(d$value)); validate(need(length(ok) >= 8, "Too few areas with data for a spatial analysis."))
    W <- b$W[ok, ok, drop = FALSE]; keep <- rowSums(W) > 0; ok <- ok[keep]; W <- W[keep, keep, drop = FALSE]
    x <- d$value[ok]
    out <- list(b = b, tool = tool, d = d)
    if (tool == "hot") {
      d[ok, z := gi_star(x, W)]; d[, cls := as.character(gi_class(z))]; d[is.na(cls), cls := "No data"]
      lm <- local_moran(x, W, nsim = 499); out$global <- lm$global; out$global_p <- lm$global_p
    } else if (tool == "lisa") {
      lm <- local_moran(x, W); d[ok, `:=`(z = lm$z, lag = lm$lag, pval = lm$p, cls = lm$class)]; d[is.na(cls), cls := "No data"]
      out$global <- lm$global; out$global_p <- lm$global_p
    } else if (tool == "emerging") {
      yrs <- summarise_ind(b$cd, b$lvl, MONTH_MIN, MONTH_MAX, by = "year")
      yrs <- yrs[bucket <= (MONTH_MAX %/% 100L) - (MONTH_MAX %% 100L < 12L)]            # complete years only
      yl <- sort(unique(yrs$bucket)); validate(need(length(yl) >= 4, "At least four complete years are needed."))
      Z <- sapply(yl, function(y) { v <- yrs[bucket == y][match(d$uid, uid), value]; z <- rep(NA_real_, length(v))
        okk <- which(is.finite(v)); WW <- b$W[okk, okk, drop = FALSE]; kk <- rowSums(WW) > 0; okk <- okk[kk]
        if (length(okk) >= 8) z[okk] <- gi_star(v[okk], b$W[okk, okk, drop = FALSE]); z })
      colnames(Z) <- yl
      d[, cls := apply(Z, 1, function(r) if (sum(is.finite(r)) < 4) "No data" else emerging_class(r[is.finite(r)]))]
      d[, trend := apply(Z, 1, mann_kendall)]
      out$Z <- Z; out$years <- yl
    } else if (tool == "change") {
      pw <- previous_window(p$from, p$to); validate(need(!is.null(pw), "No earlier period of the same length."))
      s0 <- summarise_ind(b$cd, b$lvl, pw[1], pw[2]); d[s0, on = "uid", before := i.value]
      d[, chg := if (IND[code == b$cd, unit] == "%") value - before else 100 * (value - before) / abs(before)]
      d[!is.finite(chg), chg := NA]
      d[, worse := if (dirn() == "high") -chg else chg]
      okc <- which(is.finite(d$worse)); Wc <- b$W[okc, okc, drop = FALSE]; kc <- rowSums(Wc) > 0; okc <- okc[kc]
      if (length(okc) >= 8) d[okc, zw := gi_star(worse[okc], b$W[okc, okc, drop = FALSE])]
      d[, cls := fifelse(is.na(chg), "No data", fifelse(worse > 0, "Worsened", "Improved"))]
      out$pw <- pw
    } else if (tool == "biv") {
      d[, drv := sp_driver(input$driver, b$lvl, uid, p)]
      okb <- which(is.finite(d$value) & is.finite(d$drv)); validate(need(length(okb) >= 8, "Too few areas have both measures (census measures are for sub-counties and districts)."))
      t3 <- function(v) as.integer(cut(v, unique(quantile(v, c(0, 1/3, 2/3, 1), na.rm = TRUE)), include.lowest = TRUE, labels = FALSE))
      d[okb, `:=`(bx = t3(value), bd = t3(drv))]
      d[, cls := fifelse(is.na(bx), "No data", paste0(bx, "-", bd))]
      out$rho <- suppressWarnings(cor(d$value[okb], d$drv[okb], method = "spearman"))
      out$rho_p <- suppressWarnings(cor.test(d$value[okb], d$drv[okb], method = "spearman", exact = FALSE)$p.value)
      out$driver_lab <- sub("^Census 2024: ", "", names(sp_driver_choices())[match(input$driver, sp_driver_choices())])
    }
    out$d <- d; out
  })

  output$title <- renderText({ b <- base(); sprintf("%s · %s", names(SP_TOOLS)[match(input$tool, SP_TOOLS)], ind_label(b$cd)) })
  output$subtitle <- renderText({ b <- base(); sprintf("%s · %s · %s", LEVEL_PLURAL[[b$lvl]], b$p$label, if (dirn() == "high") "higher is better" else if (dirn() == "low") "lower is better" else "no preferred direction") })
  output$about <- renderText(SP_ABOUT[[input$tool]])

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light grey") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite imagery") |>
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") |>
      add_grey_labels("Place names") |>
      addPolylines(data = GEO$district, color = BRAND$navy, weight = 1.8, opacity = .9, group = "District lines") |>
      addLayersControl(baseGroups = c("Light grey", "Satellite imagery", "Topographic"), overlayGroups = c("Place names", "District lines", "Health facilities"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      addCircleMarkers(data = FAC_PTS, lng = ~lon, lat = ~lat, radius = 2.4, stroke = FALSE, fillColor = BRAND$ink, fillOpacity = .55, group = "Health facilities", label = ~name) |>
      hideGroup("Health facilities") |>
      fitBounds(32.82, -0.4, 33.99, 1.48)
  })
  observe({
    req(input$map_zoom); r <- res(); d <- r$d; b <- r$b; cd <- b$cd
    g <- merge(b$g[, "uid"], d, by = "uid", all.x = TRUE)
    m <- leafletProxy(ns("map")) |> clearGroup("data") |> clearControls()
    lab <- function(extra) lapply(sprintf("<b>%s</b> <span style='color:#898781'>%s</span><br>%s: <b>%s</b><br>%s",
                                          htmlEscape(g$name), htmlEscape(g$district %||% ""), htmlEscape(ind_label(cd)), fmt_val(g$value, cd), extra), HTML)
    if (r$tool %in% c("hot", "lisa", "emerging")) {
      cols <- switch(r$tool, hot = GI_COLS, lisa = LM_COLS, emerging = EM_COLS)
      extra <- switch(r$tool, hot = sprintf("Gi* z-score: %s<br><b>%s</b>", ifelse(is.na(g$z), "–", sprintf("%.2f", g$z)), g$cls),
                      lisa = sprintf("<b>%s</b>%s", g$cls, ifelse(is.na(g$pval), "", sprintf(" (p = %.3f)", g$pval))),
                      emerging = sprintf("<b>%s</b><br>trend in hot spot score: %s", g$cls, ifelse(is.na(g$trend), "–", sprintf("%+.2f", g$trend))))
      fill <- unname(ifelse(g$cls %in% names(cols), cols[g$cls], "#dcdbd6"))
      m <- m |> addPolygons(data = g, fillColor = fill, fillOpacity = .86, color = "white", weight = .8, layerId = ~uid, group = "data",
                            label = lab(extra), highlightOptions = highlightOptions(weight = 2.5, color = BRAND$ink, bringToFront = TRUE))
      used <- intersect(names(cols), unique(g$cls))
      m <- m |> addLegend("bottomright", colors = unname(cols[used]), labels = used, opacity = .95, title = names(SP_TOOLS)[match(r$tool, SP_TOOLS)])
    } else if (r$tool == "change") {
      unit <- if (IND[code == cd, unit] == "%") " pts" else "%"
      lim <- max(abs(g$worse), na.rm = TRUE); if (!is.finite(lim) || lim == 0) lim <- 1
      pal <- colorNumeric(c("#1b7837", "#a6dba0", "#f7f7f7", "#f4a582", "#b2182b"), c(-lim, lim), na.color = "#dcdbd6")
      m <- m |> addPolygons(data = g, fillColor = ~pal(worse), fillOpacity = .88, color = "white", weight = .8, layerId = ~uid, group = "data",
                            label = lab(sprintf("Before (%s): %s<br>Change: <b>%s</b> (%s)", period_caption(r$pw[1], r$pw[2]), fmt_val(g$before, cd),
                                                ifelse(is.na(g$chg), "–", sprintf("%+.1f%s", g$chg, unit)), g$cls)),
                            highlightOptions = highlightOptions(weight = 2.5, color = BRAND$ink, bringToFront = TRUE))
      hot <- g[!is.na(g$zw) & g$zw >= 1.96, ]
      if (nrow(hot)) m <- m |> addPolylines(data = hot, color = "#111", weight = 2.6, opacity = 1, group = "data")
      m <- m |> addLegend("bottomright", colors = c("#1b7837", "#a6dba0", "#f7f7f7", "#f4a582", "#b2182b", "#ffffff"),
                          labels = c("Improved a lot", "Improved", "No change", "Worsened", "Worsened a lot", "Black outline: cluster of worsening"), opacity = .95, title = "Change")
    } else if (r$tool == "biv") {
      fill <- unname(ifelse(g$cls %in% names(BIV_COLS), BIV_COLS[g$cls], "#dcdbd6"))
      m <- m |> addPolygons(data = g, fillColor = fill, fillOpacity = .9, color = "white", weight = .8, layerId = ~uid, group = "data",
                            label = lab(sprintf("%s: <b>%s</b>", htmlEscape(r$driver_lab), ifelse(is.na(g$drv), "–", sprintf("%.1f", g$drv)))),
                            highlightOptions = highlightOptions(weight = 2.5, color = BRAND$ink, bringToFront = TRUE))
      cell <- function(k) sprintf("<div style='width:22px;height:22px;background:%s'></div>", BIV_COLS[[k]])
      grid <- paste0("<div style='display:grid;grid-template-columns:repeat(3,22px);gap:1px'>",
                     paste(vapply(c("1-3", "2-3", "3-3", "1-2", "2-2", "3-2", "1-1", "2-1", "3-1"), cell, ""), collapse = ""), "</div>")
      m <- m |> addControl(HTML(sprintf("<div class='biv-key'><div class='biv-y'>%s &rarr;</div>%s<div class='biv-x'>%s &rarr;</div></div>",
                                        htmlEscape(r$driver_lab), grid, htmlEscape(ind_label(cd)))), position = "bottomright")
    }
    m
  })
  observeEvent(input$map_shape_click, selected(input$map_shape_click$id))

  # ---- automated reading ------------------------------------------------------------------------
  output$insight <- renderUI({
    r <- res(); d <- r$d; cd <- r$b$cd; lab <- ind_label(cd); dn <- dirn()
    lst <- function(x, n = 4) { x <- unique(x[!is.na(x)]); if (!length(x)) return("none"); if (length(x) > n) paste0(paste(x[1:n], collapse = ", "), " and ", length(x) - n, " more") else if (length(x) > 1) paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)]) else x }
    by_dist <- function(sel) { if (r$b$lvl != "subcounty") return(""); t <- sort(table(d$district[sel]), decreasing = TRUE); if (!length(t)) return(""); sprintf(" They are concentrated in %s.", lst(sprintf("%s (%d)", names(t), as.integer(t)), 3)) }
    good_high <- dn == "high"; concern <- if (dn == "neutral") NULL else if (good_high) "low" else "high"
    glob <- function() if (!is.null(r$global)) sprintf("Across Busoga the values are %s (Moran's I = %.2f, p = %.3f).",
                       if (r$global_p < 0.05 && r$global > 0) "clearly clustered: neighbouring areas tend to be alike" else if (r$global_p < 0.05) "dispersed: neighbouring areas tend to differ" else "not clustered more than chance would give", r$global, r$global_p)
    s <- switch(r$tool,
      hot = { hot <- grepl("^Hot spot, (95|99)", d$cls); cold <- grepl("^Cold spot, (95|99)", d$cls)
        c(glob(),
          if (any(hot)) sprintf("%d %s form a hot spot of high %s (95%% confidence or more): %s.%s", sum(hot), LEVEL_PLURAL[[r$b$lvl]], tolower(lab), lst(d$name[hot]), by_dist(hot)) else "There is no hot spot of high values at 95% confidence.",
          if (any(cold)) sprintf("%d form a cold spot of low values: %s.%s", sum(cold), lst(d$name[cold]), by_dist(cold)) else "There is no cold spot of low values at 95% confidence.",
          if (!is.null(concern)) sprintf("For this indicator %s values are the concern, so the %s are where attention is most needed.", concern, if (concern == "high") "hot spots" else "cold spots")) },
      lisa = { cl <- d$cls; c(glob(),
          sprintf("%d high-high and %d low-low clusters stand out.", sum(cl == "High-high cluster"), sum(cl == "Low-low cluster")),
          if (any(grepl("outlier", cl))) sprintf("Outliers, areas unlike their neighbours: %s. These are worth checking first, both for real differences and for data problems.", lst(d$name[grepl("outlier", cl)], 5)),
          if (!is.null(concern)) sprintf("Clusters of %s values (%s) are the priority here: %s.", concern, if (concern == "high") "high-high" else "low-low",
                                         lst(d$name[cl == if (concern == "high") "High-high cluster" else "Low-low cluster"]))) },
      emerging = { cl <- d$cls; tab <- sort(table(cl[cl != "No pattern" & cl != "No data"]), decreasing = TRUE)
        c(sprintf("Hot spot analysis was repeated for each year from %d to %d.", min(r$years), max(r$years)),
          if (length(tab)) sprintf("Patterns found: %s.", lst(sprintf("%s (%d)", tolower(names(tab)), as.integer(tab)), 6)) else "No area shows a consistent pattern over the years.",
          if (any(grepl("^(New|Intensifying|Consecutive) hot", cl))) sprintf("Growing hot spots of high %s: %s.", tolower(lab), lst(d$name[grepl("^(New|Intensifying|Consecutive) hot", cl)])),
          if (any(grepl("^(New|Intensifying|Consecutive) cold", cl))) sprintf("Growing cold spots of low values: %s.", lst(d$name[grepl("^(New|Intensifying|Consecutive) cold", cl)])),
          if (any(grepl("^(Diminishing|Historical)", cl))) sprintf("Fading patterns: %s.", lst(d$name[grepl("^(Diminishing|Historical)", cl)]))) },
      change = { w <- d[is.finite(worse)]; unit <- if (IND[code == cd, unit] == "%") " points" else "%"
        c(sprintf("Compared with %s, %d %s improved and %d worsened.", period_caption(r$pw[1], r$pw[2]), sum(w$worse < 0), LEVEL_PLURAL[[r$b$lvl]], sum(w$worse > 0)),
          if (nrow(w)) sprintf("Largest worsening: %s.", lst(w[order(-worse)][1:min(3, .N), sprintf("%s (%+.1f%s)", name, chg, unit)], 3)),
          if (nrow(w)) sprintf("Largest improvement: %s.", lst(w[order(worse)][1:min(3, .N), sprintf("%s (%+.1f%s)", name, chg, unit)], 3)),
          if (any(d$zw >= 1.96, na.rm = TRUE)) sprintf("The worsening is clustered around %s.%s", lst(d$name[which(d$zw >= 1.96)]), by_dist(which(d$zw >= 1.96))) else "The worsening is scattered rather than clustered in one part of Busoga.") },
      biv = { strength <- abs(r$rho); w <- if (strength >= .5) "strong" else if (strength >= .3) "moderate" else if (strength >= .1) "weak" else "no clear"
        c(if (w == "no clear") sprintf("There is no clear relationship between %s and %s (Spearman's rho = %.2f, p = %.3f).", tolower(lab), tolower(r$driver_lab), r$rho, r$rho_p)
          else sprintf("There is a %s %s relationship between %s and %s (Spearman's rho = %.2f, p = %.3f): %s.", w, if (r$rho >= 0) "positive" else "negative", tolower(lab), tolower(r$driver_lab), r$rho, r$rho_p,
                       if (r$rho >= 0) "areas with more of one tend to have more of the other" else "areas with more of one tend to have less of the other"),
          sprintf("High on both: %s.", lst(d$name[d$cls == "3-3"])),
          sprintf("High %s but low %s: %s.", tolower(lab), tolower(r$driver_lab), lst(d$name[d$cls == "3-1"])),
          "A relationship across areas does not show cause; other differences between areas may explain both.") })
    if (IND[code == cd, unit] == "count") s <- c(s, "This indicator is a count, so areas with more people or more facilities have bigger numbers. A rate or percentage usually gives a fairer picture of where the problem is.")
    tags$ul(class = "sp-insight-list", lapply(s[!vapply(s, is.null, TRUE)], tags$li))
  })

  output$stats <- renderUI({
    r <- res(); d <- r$d
    tiles <- switch(r$tool,
      hot = list(c("Hot spots (95%+)", sum(grepl("^Hot spot, (95|99)", d$cls))), c("Cold spots (95%+)", sum(grepl("^Cold spot, (95|99)", d$cls))), c("Moran's I", sprintf("%.2f", r$global))),
      lisa = list(c("High-high", sum(d$cls == "High-high cluster")), c("Low-low", sum(d$cls == "Low-low cluster")), c("Outliers", sum(grepl("outlier", d$cls)))),
      emerging = list(c("Hot spot patterns", sum(grepl("hot spot", d$cls))), c("Cold spot patterns", sum(grepl("cold spot", d$cls))), c("Years analysed", length(r$years))),
      change = list(c("Improved", sum(d$worse < 0, na.rm = TRUE)), c("Worsened", sum(d$worse > 0, na.rm = TRUE)), c("In a worsening cluster", sum(d$zw >= 1.96, na.rm = TRUE))),
      biv = list(c("Spearman's rho", sprintf("%.2f", r$rho)), c("p-value", sprintf("%.3f", r$rho_p)), c("Areas compared", sum(d$cls != "No data"))))
    div(class = "sp-stats", lapply(tiles, function(t) div(class = "sp-stat", div(class = "sp-stat-v", t[2]), div(class = "sp-stat-l", t[1]))))
  })

  output$chart_title <- renderText(switch(input$tool, hot = "Gi* z-scores: the strongest hot and cold spots", lisa = "Moran scatterplot: each area against its neighbours",
                                          emerging = "Hot spot score by year (areas with a pattern)", change = "Biggest changes", biv = "The indicator against the driver"))
  output$chart <- renderPlotly({
    r <- res(); d <- r$d[cls != "No data"]; cd <- r$b$cd
    if (r$tool == "hot") {
      x <- d[is.finite(z)][order(-abs(z))][1:min(20, .N)][order(z)]
      return(plot_ly(x, y = ~factor(name, levels = name), x = ~z, type = "bar", orientation = "h", marker = list(color = unname(GI_COLS[x$cls]), line = list(color = "#999", width = .5)),
                     hovertemplate = "%{y}: z = %{x:.2f}<extra></extra>") |> plotly_base(xtitle = "Gi* z-score", legend = FALSE) |>
               layout(yaxis = list(title = "", tickfont = list(size = 10)), shapes = lapply(c(-1.96, 1.96), function(v) list(type = "line", x0 = v, x1 = v, y0 = 0, y1 = 1, yref = "paper", line = list(dash = "dot", color = BRAND$muted)))))
    }
    if (r$tool == "lisa") {
      return(plot_ly(d, x = ~z, y = ~lag, type = "scatter", mode = "markers", color = ~cls, colors = LM_COLS, text = ~name,
                     marker = list(size = 9, line = list(color = "#fff", width = 1)), hovertemplate = "%{text}<br>value (z) %{x:.2f}, neighbours %{y:.2f}<extra></extra>") |>
               plotly_base(xtitle = "Area's value (standardised)", ytitle = "Neighbours' average (standardised)") |>
               layout(shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper", line = list(color = BRAND$grid)),
                                    list(type = "line", y0 = 0, y1 = 0, x0 = 0, x1 = 1, xref = "paper", line = list(color = BRAND$grid)))))
    }
    if (r$tool == "emerging") {
      sel <- which(!r$d$cls %in% c("No pattern", "No data")); validate(need(length(sel) > 0, "No area shows a pattern."))
      Z <- r$Z[sel, , drop = FALSE]; o <- order(rowMeans(Z, na.rm = TRUE)); Z <- Z[o, , drop = FALSE]; nm <- r$d$name[sel][o]
      return(plot_ly(x = as.character(r$years), y = nm, z = Z, type = "heatmap", colors = c("#2166ac", "#f7f7f7", "#b2182b"), zmid = 0,
                     hovertemplate = "%{y}, %{x}: z = %{z:.2f}<extra></extra>", colorbar = list(title = "Gi* z", len = .7)) |>
               plotly_base() |> layout(yaxis = list(tickfont = list(size = 9)), xaxis = list(type = "category")))
    }
    if (r$tool == "change") {
      x <- d[is.finite(worse)][order(-abs(worse))][1:min(16, .N)][order(chg)]
      return(plot_ly(x, y = ~factor(name, levels = name)) |>
               add_segments(x = ~before, xend = ~value, yend = ~factor(name, levels = name), line = list(color = "#bbb", width = 2), showlegend = FALSE, hoverinfo = "skip") |>
               add_markers(x = ~before, name = "Before", marker = list(color = "#9e9ba8", size = 8), hovertemplate = "%{y}: before %{x:.1f}<extra></extra>") |>
               add_markers(x = ~value, name = "Now", marker = list(color = ifelse(x$worse > 0, "#b2182b", "#1b7837"), size = 10), hovertemplate = "%{y}: now %{x:.1f}<extra></extra>") |>
               plotly_base(xtitle = unit_label(cd)) |> layout(yaxis = list(title = "", tickfont = list(size = 10))))
    }
    if (r$tool == "biv") {
      x <- d[is.finite(drv) & is.finite(value)]
      fit <- lm(value ~ drv, x); xs <- range(x$drv)
      return(plot_ly(x, x = ~drv, y = ~value, type = "scatter", mode = "markers", text = ~name, marker = list(size = 9, color = unname(BIV_COLS[x$cls]), line = list(color = "#555", width = .6)),
                     hovertemplate = "%{text}<br>%{x:.1f} / %{y:.1f}<extra></extra>", showlegend = FALSE) |>
               add_lines(x = xs, y = predict(fit, data.frame(drv = xs)), line = list(color = BRAND$ink2, dash = "dash"), hoverinfo = "skip", inherit = FALSE, showlegend = FALSE) |>
               plotly_base(xtitle = r$driver_lab, ytitle = ind_label(cd), legend = FALSE))
    }
  })

  output$table <- renderTable({
    r <- res(); d <- r$d[cls != "No data"]; cd <- r$b$cd
    d <- switch(r$tool,
      hot = d[order(-abs(z))][, .(Area = name, District = district, Value = fmt_val(value, cd), `Gi* z` = sprintf("%.2f", z), Result = cls)],
      lisa = d[order(pval)][, .(Area = name, District = district, Value = fmt_val(value, cd), `p-value` = sprintf("%.3f", pval), Result = cls)],
      emerging = d[order(cls == "No pattern", -abs(trend))][, .(Area = name, District = district, Value = fmt_val(value, cd), Trend = sprintf("%+.2f", trend), Result = cls)],
      change = d[is.finite(worse)][order(-worse)][, .(Area = name, District = district, Before = fmt_val(before, cd), Now = fmt_val(value, cd),
                                                      Change = sprintf("%+.1f%s", chg, if (IND[code == cd, unit] == "%") " pts" else "%"), Result = fifelse(!is.na(zw) & zw >= 1.96, paste(cls, "(clustered)"), cls))],
      biv = d[order(-bx, -bd)][, .(Area = name, District = district, Value = fmt_val(value, cd), Driver = sprintf("%.1f", drv),
                                   Result = c("low", "middle", "high")[bx] |> paste("indicator,", c("low", "middle", "high")[bd], "driver"))])
    head(d, 15)
  }, striped = TRUE, spacing = "s", width = "100%")

  output$sel_title <- renderText({ u <- selected(); if (is.null(u)) "Click an area on the map to see its trend" else sprintf("%s: quarterly trend", ou_name[[u]]) })
  output$trend <- renderPlotly({
    u <- selected(); cd <- input$ind
    validate(need(!is.null(u), "Click an area on the map."))
    lvl <- ou_level[[u]]
    s <- summarise_ind(cd, lvl, MONTH_MIN, MONTH_MAX, uids = u, by = "quarter"); r <- summarise_ind(cd, "region", MONTH_MIN, MONTH_MAX, by = "quarter")
    validate(need(nrow(s) > 0, "No data for this area."))
    col <- theme_col(IND[code == cd, theme])
    plot_ly() |>
      add_lines(data = r, x = ~bucket_date(bucket, "quarter"), y = ~value, name = "Busoga", line = list(color = BRAND$muted, width = 2, dash = "dot")) |>
      add_lines(data = s, x = ~bucket_date(bucket, "quarter"), y = ~value, name = ou_name[[u]], line = list(color = col, width = 2.5)) |>
      plotly_base(ytitle = unit_label(cd))
  })
})
