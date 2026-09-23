# Health system building blocks (WHO framework): what routine and open data say about each of the
# six building blocks for Busoga, a district or a sub-county, plus the living conditions that shape
# health (UBOS census 2024). Every figure is a tile: tap it to enlarge it into a deeper view (trend,
# comparison between areas, breakdowns) with a link to the page that analyses it in full.
# Sources: HMIS/DHIS2 (monthly; medicines and management from the 105:06-09 report, built by
# R/03c_extract_supplies.R), UBOS NPHC 2024, World Bank and WHO GHO (built by R/08b_context.R).

CTX <- if (file.exists(file.path(DATA, "context.rds"))) readRDS(file.path(DATA, "context.rds")) else NULL
SUP <- if (file.exists(file.path(DATA, "supplies.rds"))) readRDS(file.path(DATA, "supplies.rds")) else NULL
if (!is.null(SUP)) {
  # reports received come from the reporting-rate table (complete for every year)
  SUP$reports <- unique(REP[dataset == "VDhwrW9DiC1" & actual > 0, .(uid, period)])
  setkey(SUP$reports, uid, period); setkey(SUP$stock, uid, period)
}
SUP_TRACER <- if (!is.null(SUP)) SUP$items[tracer == 1, item] else character()
SUP_COLS <- c("Malaria" = "#C0582B", "Maternal, newborn and family planning" = "#B0306A", "Immunisation and child health" = "#2E7D32",
              "HIV" = "#A3214A", "Laboratory and blood" = "#5C6BC0", "Tuberculosis" = "#8E44AD", "Emergency and hospital care" = "#C17D11",
              "Non-communicable diseases" = "#00897B", "Mental health" = "#37474F")
MG_SUPERVISION <- c("Supervision by the Ministry of Health", "Supervision by regional teams", "Supervision by the local government",
                    "Supervision by the health sub-district", "Other support supervision")

# census 2024 measures: numerator and denominator (table, column); pct = already a percentage
CENSUS_MEASURES <- list(
  water    = list(lab = "Households using an improved water source", icon = "faucet-drip", num = c("Water & Sanitation", "Improved Water"), den = c("Water & Sanitation", "Total Households"), good = "high"),
  sanit    = list(lab = "Households with improved sanitation", icon = "toilet", num = c("Water & Sanitation", "Improved Sanitation"), den = c("Water & Sanitation", "Total Households"), good = "high"),
  opendef  = list(lab = "Households practising open defecation", icon = "triangle-exclamation", num = c("Water & Sanitation", "Open Defecation"), den = c("Water & Sanitation", "Total Households"), good = "low"),
  grid     = list(lab = "Households with grid electricity", icon = "bolt", num = c("Lighting", "Grid Electricity"), den = c("Lighting", "Total Households"), good = "high"),
  net      = list(lab = "Households with a mosquito net", icon = "shield-virus", num = c("Health Indicators", "Households with Mosquito Net"), den = c("Household Size", "Number of Households"), good = "high"),
  insur    = list(lab = "People with health insurance", icon = "file-shield", num = c("Health Indicators", "Persons with Health Insurance"), den = c("Household Size", "Household Population"), good = "high"),
  birthreg = list(lab = "People with a birth certificate", icon = "id-card", num = c("Birth Registration", "With Certificate"), den = c("Birth Registration", "Household Population"), good = "high"),
  oos      = list(lab = "Children aged 6-12 out of school", icon = "school", num = c("Children Out of School", "Out of School (6-12yr)"), den = c("Children Out of School", "Population (6-12yr)"), good = "low"),
  neet     = list(lab = "Young people 18-30 not in work, education or training", icon = "user-clock", pct = c("Unemployment & NEET", "NEET 18-30 (%)"), good = "low"),
  unemp    = list(lab = "Unemployment, age 15 and over", icon = "briefcase", pct = c("Unemployment & NEET", "Unemployment 15+ (%)"), good = "low"),
  subsist  = list(lab = "Households in the subsistence economy", icon = "seedling", num = c("Subsistence & PDM", "Subsistence Economy"), den = c("Subsistence & PDM", "Total Households"), good = "low"),
  pdm      = list(lab = "Households that benefited from the Parish Development Model", icon = "hand-holding-dollar", num = c("Subsistence & PDM", "Benefited from PDM"), den = c("Subsistence & PDM", "Total Households"), good = "high"))

census_value <- function(area_uid, m) {
  if (is.null(CTX) || is.null(area_uid) || is.na(area_uid)) return(NA_real_)
  x <- CTX$census[CTX$census$uid %in% area_uid]
  if (!nrow(x)) return(NA_real_)
  g <- function(tc) { v <- x[table == tc[1] & column == tc[2], value]; if (length(v)) v[1] else NA_real_ }
  if (!is.null(m$pct)) return(g(m$pct))
  n <- g(m$num); d <- g(m$den); if (is.na(n) || is.na(d) || d <= 0) NA_real_ else 100 * n / d
}
# census areas: sub-county if matched, else its district, else Busoga
census_uid <- function(a) {
  if (is.null(CTX)) return(NULL)
  has <- unique(CTX$census$uid)
  if (a$uid %in% has) return(a$uid)
  d <- OU$uid_l3[match(a$uid, OU$uid)]; if (!is.na(d) && d %in% has) return(d)
  META$region_uid
}
national_latest <- function(codes) {
  if (is.null(CTX) || !nrow(CTX$national)) return(NULL)
  x <- CTX$national[code %in% codes][order(-year)]
  if (!nrow(x)) NULL else x[1]
}

# ---- facilities of an area, and the areas one level down used for comparisons ------------------
area_facs <- function(a) {
  f <- OU[level_name == "facility"]
  if (a$level != "region") f <- f[uid_l3 == a$uid | uid_l4 == a$uid | uid_l5 == a$uid | uid == a$uid]
  f
}
kids_of <- function(a) {
  f <- OU[level_name == "facility"]
  switch(a$level,
    region    = list(col = "uid_l3", facs = f, lab = "district / city"),
    district  = list(col = "uid_l5", facs = f[uid_l3 == a$uid], lab = "sub-county"),
    dlg       = list(col = "uid_l5", facs = f[uid_l4 == a$uid], lab = "sub-county"),
    subcounty = list(col = "uid", facs = f[uid_l5 == a$uid], lab = "facility"),
    facility  = { s <- OU$uid_l5[match(a$uid, OU$uid)]; list(col = "uid", facs = f[uid_l5 == s], lab = "facility in the same sub-county") })
}

# ---- medicines and management (HMIS 105:06-09) -------------------------------------------------
# A facility "reports" an item in a month when it fills in the days-out-of-stock box (0 or more);
# the stock-out rate is the share of those facility-months with one or more days out of stock.
sup_frame <- function(fu, from, to) {
  if (is.null(SUP)) return(NULL)
  rp <- SUP$reports[uid %in% fu & period >= from & period <= to]
  st <- SUP$stock[uid %in% fu & period >= from & period <= to][rp, on = .(uid, period), nomatch = 0]
  list(rp = rp, st = st)
}
sup_summary <- function(fu, from, to) {
  s <- sup_frame(fu, from, to); if (is.null(s) || !nrow(s$st)) return(NULL)
  tr <- s$st[item %in% SUP_TRACER]
  fm <- tr[, .(out = any(days > 0)), by = .(uid, period)]
  per_item <- s$st[, .(n = .N, out = sum(days > 0)), by = item][n >= 5][, rate := 100 * out / n][order(-rate)]
  list(n = nrow(fm), facs = uniqueN(fm$uid), any = 100 * mean(fm$out), avg = 100 * mean(tr$days > 0), items = per_item)
}
# management: share of facility monthly reports with at least one held, plus the counts
mg_summary <- function(fu, from, to, whats) {
  if (is.null(SUP)) return(NULL)
  rp <- SUP$reports[uid %in% fu & period >= from & period <= to]; if (!nrow(rp)) return(NULL)
  x <- SUP$mgmt[uid %in% fu & period >= from & period <= to & what %in% whats]
  held <- unique(x[conducted > 0, .(uid, period)])[rp, on = .(uid, period), nomatch = 0]
  list(planned = sum(x$planned), conducted = sum(x$conducted), facs = uniqueN(held$uid), reports = nrow(rp),
       share = 100 * nrow(held) / nrow(rp),
       per_fac_year = sum(x$conducted) / max(1, uniqueN(rp$uid)) / max(1, uniqueN(rp$period)) * 12,
       pct = if (sum(x$planned) > 0) 100 * sum(x$conducted) / sum(x$planned) else NA_real_)
}
mg_whats <- function(key) if (key == "supervision") MG_SUPERVISION else setdiff(unique(SUP$mgmt$what), MG_SUPERVISION)

BLOCKS <- list(
  service   = list(title = "Service delivery", icon = "stethoscope", col = "#3949AB",
                   what = "Whether people get the essential services they need, of good quality, when they need them."),
  workforce = list(title = "Health workforce", icon = "user-doctor", col = "#00897B",
                   what = "Enough trained health workers, in the right places, working well."),
  info      = list(title = "Health information", icon = "chart-simple", col = "#5C6BC0",
                   what = "Timely, reliable data that are reported, checked and used for decisions."),
  medicines = list(title = "Medicines and supplies", icon = "pills", col = "#A3214A",
                   what = "Essential medicines, vaccines and supplies available when needed."),
  financing = list(title = "Health financing", icon = "coins", col = "#C17D11",
                   what = "Enough money for health, raised in ways that protect people from financial hardship."),
  governance = list(title = "Leadership and governance", icon = "landmark", col = "#37474F",
                   what = "Policies, oversight, accountability and the stewardship of the whole system."))

bx_js <- function(input_id, kind, key, block)
  sprintf("Shiny.setInputValue('%s', {kind: '%s', key: '%s', block: '%s', nonce: Math.random()}, {priority: 'event'}); return false;",
          input_id, kind, key, block)
pct_txt <- function(v) if (is.null(v) || !length(v) || is.na(v)) "–" else if (v < 10) sprintf("%.1f%%", v) else sprintf("%.0f%%", v)

blocks_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Analyse · WHO health system framework", "Health system building blocks",
              "The World Health Organization describes a health system through six building blocks. This page brings together what routine data (HMIS), the 2024 census (UBOS) and international open data (World Bank, WHO) say about each block, for Busoga, a district or a sub-county. Tap any figure to open it in more depth.", key = "blocks"),
    filter_bar(area_ui(ns("area")), period_ui(ns("period"))),
    div(class = "blocks-nav", lapply(names(BLOCKS), function(k) tags$a(href = paste0("#", ns(paste0("b_", k))), class = "blocks-chip",
        style = sprintf("--bc:%s", BLOCKS[[k]]$col), fontawesome::fa(BLOCKS[[k]]$icon, fill = BLOCKS[[k]]$col, height = ".9em"), BLOCKS[[k]]$title)),
        tags$a(href = paste0("#", ns("living")), class = "blocks-chip", style = "--bc:#6a5a2b", fontawesome::fa("house", fill = "#6a5a2b", height = ".9em"), "Living conditions")),
    uiOutput(ns("blocks")),
    div(id = ns("living"), class = "xai-section",
        h3(class = "xai-h", "Living conditions that shape health"),
        p(class = "xai-lead", textOutput(ns("living_lead"), inline = TRUE)),
        uiOutput(ns("living_tiles"))),
    div(id = ns("rank_card"), card(full_screen = TRUE, class = "xai-card",
         card_header(div(class = "xai-ch", textOutput(ns("rank_title"), inline = TRUE),
                         div(class = "xai-sub", "UBOS National Population and Housing Census 2024; dashed line = Busoga")),
                     selectInput(ns("measure"), NULL, setNames(names(CENSUS_MEASURES), vapply(CENSUS_MEASURES, `[[`, "", "lab")), width = "330px")),
         plotlyOutput(ns("rank"), height = "440px"))),
    info_note("Tap any figure to enlarge it: you get its trend, how areas compare and a breakdown, with a link to the page that analyses it in full. ",
              "Sources: HMIS/DHIS2 (Uganda Ministry of Health), updated monthly; UBOS National Population and Housing Census 2024 (statistics.ubos.org); World Bank World Development Indicators and WHO Global Health Observatory, national figures updated yearly. ",
              "Census figures are for the 2024 census and do not change month to month. National figures are for Uganda as a whole, shown where no district data exist.")
  )
}

blocks_server <- function(id) moduleServer(id, function(input, output, session) {
  area <- area_server("area"); per <- period_server("period")
  ns <- session$ns

  metric <- function(label, value, sub = NULL, source, chip = NULL, cmp = NULL, open = NULL, block = "service") {
    body <- tagList(div(class = "blk-m-top", div(class = "blk-m-lab", label),
                        div(class = "blk-m-tags", span(class = "blk-src", source), if (!is.null(open)) span(class = "blk-go", EXPAND_ICON))),
                    div(class = "blk-m-val", value, if (!is.null(cmp)) span(class = "blk-cmp", cmp)),
                    if (!is.null(sub)) div(class = "blk-m-sub", sub), chip)
    if (is.null(open)) div(class = "blk-metric", body) else
      tags$a(href = "#", class = "blk-metric blk-link", title = "Tap to enlarge", onclick = bx_js(ns("bx"), open[1], open[2], block), body)
  }
  gap <- function(...) div(class = "blk-gap", fontawesome::fa("circle-info", fill = "#8a8799", height = ".85em"), span(...))
  natl <- function(codes, fmt = function(v) sprintf("%.1f", v), label, block) {
    x <- national_latest(codes); if (is.null(x)) return(NULL)
    metric(label, fmt(x$value), sub = sprintf("Uganda, %d", x$year), source = paste(x$source, "· national"),
           open = c("national", paste(codes, collapse = "|")), block = block)
  }

  output$blocks <- renderUI({
    a <- area(); p <- per()
    hmis <- function(cd) { s <- summarise_ind(cd, a$level, p$from, p$to, uids = a$uid); if (nrow(s)) s else NULL }
    reg  <- function(cd) { if (a$level == "region") return(NULL); s <- summarise_ind(cd, "region", p$from, p$to); if (nrow(s)) s$value else NULL }
    hm <- function(cd, label = ind_label(cd)) {
      if (!cd %in% IND$code) return(NULL)
      s <- hmis(cd); v <- if (!is.null(s)) s$value else NA
      r <- reg(cd)
      metric(label, fmt_val(v, cd), sub = unit_label(cd), source = "HMIS · monthly",
             cmp = if (!is.null(r)) sprintf("Busoga %s", fmt_val(r, cd)), chip = target_chip(if (!is.null(s)) s$value_raw else NA, cd, p$n),
             open = c("hmis", cd), block = "service")
    }
    facs <- area_facs(a)
    rep_of <- function(ds) { r <- REP[dataset == ds & period >= p$from & period <= p$to & uid %in% facs$uid]
      if (sum(r$expected)) c(100 * sum(r$actual) / sum(r$expected), 100 * sum(r$on_time) / sum(r$expected)) else c(NA, NA) }
    opd <- rep_of("RtEYsASU7PG"); sup <- rep_of("VDhwrW9DiC1")
    dq <- if (!is.null(DQF)) mean(DQF[uid %in% facs$uid, dq_score], na.rm = TRUE) else NA
    cu <- census_uid(a); cu_name <- if (!is.null(cu)) (if (cu == META$region_uid) "Busoga" else ou_name[[cu]]) else ""
    cen <- function(k, block) { m <- CENSUS_MEASURES[[k]]; v <- census_value(cu, m); b <- census_value(META$region_uid, m)
      metric(m$lab, pct_txt(v), sub = sprintf("%s, census 2024", cu_name), source = "UBOS · census 2024",
             cmp = if (!is.null(cu) && cu != META$region_uid && is.finite(b)) sprintf("Busoga %s", pct_txt(b)), open = c("census", k), block = block) }
    wf <- national_latest("HWF_0001"); nm <- national_latest("HWF_0006")
    dens <- if (!is.null(wf) && !is.null(nm)) wf$value + nm$value else NA
    ss <- sup_summary(facs$uid, p$from, p$to)
    ssb <- if (a$level != "region") sup_summary(OU[level_name == "facility", uid], p$from, p$to)
    sv <- mg_summary(facs$uid, p$from, p$to, MG_SUPERVISION)
    dr <- mg_summary(facs$uid, p$from, p$to, "Maternal and perinatal death reviews")
    hu <- mg_summary(facs$uid, p$from, p$to, c("Health unit management committee / board meetings", "Community accountability and client feedback meetings"))
    qi <- mg_summary(facs$uid, p$from, p$to, "Quality improvement meetings")
    held_sub <- function(m) if (is.null(m)) NULL else sprintf("share of facility monthly reports with at least one held · %s held%s · %s", format(m$conducted, big.mark = ","),
                                                               if (m$planned > 0) sprintf(" against %s planned", format(m$planned, big.mark = ",")) else "", p$label)
    card_of <- function(k, ...) { b <- BLOCKS[[k]]
      div(id = ns(paste0("b_", k)), class = "blk-card", style = sprintf("--bc:%s", b$col),
          div(class = "blk-head", span(class = "blk-icon", fontawesome::fa(b$icon, fill = "#fff", height = "1.05em")),
              div(div(class = "blk-title", b$title), div(class = "blk-what", b$what))),
          div(class = "blk-body", ...)) }
    div(class = "blk-grid",
      card_of("service", hm("ANC02"), hm("DEL02"), hm("EPI09"), hm("SRV02"),
              natl("UHC_INDEX_REPORTED", function(v) sprintf("%.0f / 100", v), "UHC service coverage index", "service")),
      card_of("workforce",
              natl("HWF_0001", function(v) sprintf("%.1f", v), "Medical doctors per 10,000 people", "workforce"),
              natl("HWF_0006", function(v) sprintf("%.1f", v), "Nurses and midwives per 10,000 people", "workforce"),
              if (is.finite(dens)) metric("Doctors, nurses and midwives per 10,000", sprintf("%.1f", dens),
                                          sub = "WHO threshold for the SDGs: 44.5 per 10,000", source = "WHO GHO · national",
                                          chip = span(class = paste("tgt-chip", if (dens >= 44.5) "met" else "below"), if (dens >= 44.5) "Above threshold" else "Below threshold"),
                                          open = c("national", "HWF_0001|HWF_0006|density"), block = "workforce"),
              gap("No district data: the HMIS staffing form (107c) was last reported in 2020/21. District human-resource data (iHRIS) can be added through the staffing template on the Facilities page.")),
      card_of("info",
              metric("Monthly OPD reports received", pct_txt(opd[1]), sub = sprintf("%s on time · %s", pct_txt(opd[2]), p$label), source = "HMIS · monthly", open = c("report", "RtEYsASU7PG"), block = "info"),
              metric("Supplies and outreach reports received (105:06-09)", pct_txt(sup[1]), sub = sprintf("%s on time", pct_txt(sup[2])), source = "HMIS · monthly", open = c("report", "VDhwrW9DiC1"), block = "info"),
              metric("Average data quality score of facilities", if (is.finite(dq)) sprintf("%.0f / 100", dq) else "–", sub = "consistency, outliers and completeness checks", source = "HMIS · monthly", open = c("dq", "dq"), block = "info"),
              cen("birthreg", "info")),
      card_of("medicines",
              if (!is.null(ss)) tagList(
                metric("Tracer medicines and supplies available, on average", pct_txt(100 - ss$avg),
                       sub = sprintf("share of the %d tracer items with no day out of stock in the month, in facilities that stock them · %s", length(SUP_TRACER), p$label),
                       cmp = if (!is.null(ssb)) sprintf("Busoga %s", pct_txt(100 - ssb$avg)), source = "HMIS · monthly", open = c("stock", "avg"), block = "medicines"),
                metric("Facility reports with at least one tracer item out of stock", pct_txt(ss$any),
                       sub = sprintf("of %s facility monthly reports · %s", format(ss$n, big.mark = ","), p$label),
                       cmp = if (!is.null(ssb)) sprintf("Busoga %s", pct_txt(ssb$any)), source = "HMIS · monthly", open = c("stock", "any"), block = "medicines"),
                if (nrow(ss$items)) metric("Most often out of stock", as.character(ss$items$item[1]),
                       sub = sprintf("out of stock in %s of the facility reports that list it; next: %s", pct_txt(ss$items$rate[1]), paste(head(as.character(ss$items$item[-1]), 2), collapse = ", ")),
                       source = "HMIS · monthly", open = c("stock", "items"), block = "medicines"))
              else gap("Stock-out data from the HMIS 105:06-09 report are not available for this selection."),
              metric("Supplies reports received (105:06-09)", pct_txt(sup[1]), sub = "the HMIS report that carries stock information", source = "HMIS · monthly", open = c("report", "VDhwrW9DiC1"), block = "medicines"),
              cen("net", "medicines")),
      card_of("financing", cen("insur", "financing"),
              natl("SH.XPD.CHEX.PC.CD", function(v) sprintf("US$ %.0f", v), "Health spending per person", "financing"),
              natl(c("SH.XPD.OOPC.CH.ZS", "GHED_OOPSCHE_SHA2011"), function(v) sprintf("%.0f%%", v), "Paid out of pocket (share of health spending)", "financing"),
              natl(c("SH.XPD.GHED.CH.ZS", "GHED_GGHE_DCHE_SHA2011"), function(v) sprintf("%.0f%%", v), "Paid by government (share of health spending)", "financing"),
              natl(c("SH.XPD.EHEX.CH.ZS", "GHED_EXTCHE_SHA2011"), function(v) sprintf("%.0f%%", v), "Paid by donors (share of health spending)", "financing"),
              cen("pdm", "financing")),
      card_of("governance",
              if (!is.null(sv)) metric("Facilities receiving support supervision in a month", pct_txt(sv$share),
                                       sub = sprintf("%s visits in total, about %.1f per facility a year (Ministry, region, district, health sub-district, others) · %s", format(sv$conducted, big.mark = ","), sv$per_fac_year, p$label),
                                       source = "HMIS · monthly", open = c("mgmt", "supervision"), block = "governance"),
              if (!is.null(dr)) metric("Maternal and perinatal death reviews held", format(dr$conducted, big.mark = ","),
                                       sub = sprintf("%s planned · by %s facilities · %s", format(dr$planned, big.mark = ","), format(dr$facs, big.mark = ","), p$label), source = "HMIS · monthly", open = c("mgmt", "meetings"), block = "governance"),
              if (!is.null(qi)) metric("Facilities holding a quality improvement meeting in a month", pct_txt(qi$share), sub = held_sub(qi), source = "HMIS · monthly", open = c("mgmt", "meetings"), block = "governance"),
              if (!is.null(hu)) metric("Facilities holding a management committee or community accountability meeting", pct_txt(hu$share), sub = held_sub(hu), source = "HMIS · monthly", open = c("mgmt", "meetings"), block = "governance"),
              natl("GE.EST", function(v) sprintf("%+.2f", v), "Government effectiveness (-2.5 to +2.5)", "governance"),
              natl("CC.EST", function(v) sprintf("%+.2f", v), "Control of corruption (-2.5 to +2.5)", "governance"),
              natl("SDGIHR2021", function(v) sprintf("%.0f%%", v), "International Health Regulations core capacity", "governance"),
              metric("Facilities that sent the monthly report", sprintf("%s of %s", format(uniqueN(REP[dataset == "RtEYsASU7PG" & period >= p$from & period <= p$to & uid %in% facs$uid & actual > 0, uid]), big.mark = ","), format(nrow(facs), big.mark = ",")),
                     sub = "a basic measure of oversight and accountability", source = "HMIS · monthly", open = c("report", "RtEYsASU7PG"), block = "governance")))
  })

  output$living_lead <- renderText({
    a <- area(); cu <- census_uid(a)
    nm <- if (is.null(cu)) "" else if (cu == META$region_uid) "Busoga" else ou_name[[cu]]
    sprintf("From the 2024 census for %s%s. Arrows compare with Busoga as a whole. Tap a tile to compare every area.", nm,
            if (!is.null(cu) && cu != a$uid && a$level != "region") sprintf(" (the census is not published for %s, so its district is shown)", a$name) else "")
  })
  output$living_tiles <- renderUI({
    a <- area(); cu <- census_uid(a); req(!is.null(cu))
    tiles <- lapply(names(CENSUS_MEASURES), function(k) {
      m <- CENSUS_MEASURES[[k]]; v <- census_value(cu, m); b <- census_value(META$region_uid, m)
      better <- if (is.finite(v) && is.finite(b) && cu != META$region_uid) { d <- v - b; if (abs(d) < 1) NA else if ((d > 0) == (m$good == "high")) TRUE else FALSE } else NA
      tags$a(href = "#", class = "liv-tile liv-link", title = "Tap to enlarge", onclick = bx_js(ns("bx"), "census", k, "living"),
          div(class = "liv-top", span(class = "liv-icon", fontawesome::fa(m$icon, fill = "#6a5a2b", height = "1em")), div(class = "liv-lab", m$lab),
              span(class = "blk-go", EXPAND_ICON)),
          div(class = "liv-val", pct_txt(v)),
          div(class = "liv-bar", div(class = "liv-fill", style = sprintf("width:%.0f%%", if (is.finite(v)) min(100, v) else 0))),
          div(class = "liv-cmp", if (cu == META$region_uid) "Busoga as a whole" else if (is.finite(b)) tagList(
            span(class = if (isTRUE(better)) "good" else if (isFALSE(better)) "bad" else "", if (isTRUE(better)) "▲ better" else if (isFALSE(better)) "▼ worse" else "about the same"),
            sprintf(" %s Busoga (%s)", if (is.na(better)) "as" else "than", pct_txt(b)))))
    })
    div(class = "liv-grid", tiles)
  })

  # census ranking of the areas one level down
  census_rank <- function(a, m) {
    if (a$level == "region") ids <- unique(CTX$census[level == "district" & !is.na(uid), uid])
    else { d <- OU$uid_l3[match(a$uid, OU$uid)]; ids <- unique(CTX$census[level == "subcounty" & duid == d & !is.na(uid), uid]) }
    x <- data.table(uid = ids, v = vapply(ids, census_value, 0, m = m))[is.finite(v)]
    x[, name := ou_name[uid]]; setorder(x, v); x
  }
  rank_plot <- function(x, a, m, col = "#6a5a2b", light = "#c9b98a") {
    x[, fill := fifelse(uid == a$uid | uid == OU$uid_l3[match(a$uid, OU$uid)], col, light)]
    b <- census_value(META$region_uid, m)
    plot_ly(x, y = ~factor(name, levels = name), x = ~v, type = "bar", orientation = "h", marker = list(color = ~fill),
            text = ~sprintf("%.0f%%", v), textposition = "outside", cliponaxis = FALSE, hovertemplate = "%{y}: %{text}<extra></extra>") |>
      plotly_base(xtitle = m$lab, legend = FALSE) |>
      layout(xaxis = list(ticksuffix = "%", range = c(0, max(x$v, b, na.rm = TRUE) * 1.15)), yaxis = list(title = "", tickfont = list(size = 11)),
             shapes = if (is.finite(b)) list(list(type = "line", xref = "x", yref = "paper", x0 = b, x1 = b, y0 = 0, y1 = 1,
                                                  line = list(color = BRAND$ink2, dash = "dash", width = 1.2))),
             margin = list(l = 10, r = 40, t = 10, b = 10))
  }
  output$rank_title <- renderText({ a <- area()
    if (a$level == "region") "Districts and cities compared" else sprintf("Sub-counties of %s compared", ou_name[[OU$uid_l3[match(a$uid, OU$uid)]]] %||% a$name) })
  output$rank <- renderPlotly({
    req(!is.null(CTX)); a <- area(); m <- CENSUS_MEASURES[[input$measure]]
    x <- census_rank(a, m); validate(need(nrow(x) > 0, "No census data for this area."))
    rank_plot(x, a, m)
  })

  # ======================= enlarged tile: deeper view ==========================================
  bx <- reactiveValues(kind = NULL, key = NULL, col = NULL, nonce = 0)
  fact <- function(lab, val, sub = NULL) div(class = "xp-fact", div(class = "xp-fact-lab", lab), div(class = "xp-fact-val", val), if (!is.null(sub)) div(class = "xp-fact-sub", sub))
  xcard <- function(title, sub, out, h = "340px", extra = NULL)
    card(class = "xp-card", full_screen = FALSE, card_header(div(title, if (!is.null(sub)) span(class = "sub", sub)), extra), plotlyOutput(ns(out), height = h))
  shade <- function(p, col) list(list(type = "rect", xref = "x", yref = "paper", x0 = ym_date(p$from) - 15, x1 = ym_date(p$to) + 15, y0 = 0, y1 = 1,
                                      fillcolor = col, opacity = 0.08, line = list(width = 0), layer = "below"))
  range_x <- list(title = "Month", showgrid = FALSE, tickformat = "%b %Y", linecolor = BRAND$base,
                  rangeselector = list(x = 0, y = 1.12, buttons = list(
                    list(count = 12, label = "12 months", step = "month", stepmode = "backward"),
                    list(count = 24, label = "24 months", step = "month", stepmode = "backward"),
                    list(step = "all", label = "All years"))))
  hbar <- function(d, xtitle, col, suffix = "%", fmt = function(v) sprintf("%.0f%%", v), ref = NULL) {
    setorder(d, v)
    plot_ly(d, y = ~factor(name, levels = unique(name)), x = ~v, type = "bar", orientation = "h", marker = list(color = if ("fill" %in% names(d)) ~fill else col),
            text = ~fmt(v), textposition = "outside", cliponaxis = FALSE, hovertemplate = "%{y}: %{text}<extra></extra>") |>
      plotly_base(xtitle = xtitle, legend = FALSE) |>
      layout(yaxis = list(title = "", tickfont = list(size = 11, color = BRAND$ink2)),
             xaxis = list(title = xtitle, gridcolor = BRAND$grid, ticksuffix = suffix, range = c(0, max(c(d$v, ref), na.rm = TRUE) * 1.18)),
             shapes = if (!is.null(ref) && is.finite(ref)) list(list(type = "line", xref = "x", yref = "paper", x0 = ref, x1 = ref, y0 = 0, y1 = 1,
                                                                    line = list(color = BRAND$ink2, dash = "dash", width = 1.2))),
             margin = list(l = 10, r = 40, t = 10, b = 10))
  }
  footer_btn <- function(label, icon, js, extra_input = NULL)
    tags$button(type = "button", class = "btn btn-primary xp-go",
                onclick = paste0(if (!is.null(extra_input)) extra_input else "", "bootstrap.Modal.getInstance(this.closest('.modal')).hide(); ", js),
                fontawesome::fa(icon, fill = "#fff", height = "1em"), " ", label)

  observeEvent(input$bx, {
    o <- input$bx; a <- area(); p <- per(); k <- o$kind; key <- o$key
    col <- if (o$block == "living") "#6a5a2b" else BLOCKS[[o$block]]$col %||% BRAND$navy
    icon <- if (o$block == "living") "house" else BLOCKS[[o$block]]$icon
    facs <- area_facs(a); kd <- kids_of(a)
    title <- sub <- NULL; facts <- NULL; body <- NULL; note <- NULL; go <- NULL
    if (k == "hmis") {
      cd <- key; s <- summarise_ind(cd, a$level, p$from, p$to, uids = a$uid)
      v <- if (nrow(s)) s$value else NA; vr <- if (nrow(s)) s$value_raw else NA
      pw <- previous_window(p$from, p$to)
      pv <- if (!is.null(pw) && !isTRUE(SERIES_START[cd] > pw[1])) { z <- summarise_ind(cd, a$level, pw[1], pw[2], uids = a$uid); if (nrow(z)) z$value else NA } else NA
      bs <- if (a$level != "region") { z <- summarise_ind(cd, "region", p$from, p$to); if (nrow(z)) z$value else NA } else NA
      d <- IND[code == cd]
      title <- ind_label(cd); sub <- sprintf("%s · %s · %s", a$name, p$label, unit_label(cd))
      facts <- list(fact("Selected period", fmt_val(v, cd), p$label), fact("Previous period", fmt_val(pv, cd), if (!is.null(pw)) period_caption(pw[1], pw[2])),
                    if (a$level != "region") fact("Busoga", fmt_val(bs, cd), "same period"),
                    if (!is.na(target_text(cd))) fact("Target", target_text(cd), target_chip(vr, cd, p$n)))
      body <- list(xcard(sprintf("%s, %s: monthly trend", ind_label(cd), a$name), "shaded band = selected period", "bx_p1", "380px"),
                   if (a$level %in% c("region", "district", "dlg")) xcard(sprintf("By %s, %s", kd$lab, p$label), NULL, "bx_p2", "320px"))
      note <- tagList(tags$b("Definition. "), if (nzchar(d$num_desc %||% "")) sprintf("Numerator: %s. Denominator: %s.", d$num_desc, d$den_desc) else d$note)
      go <- footer_btn("See details in the Indicator explorer", "magnifying-glass-chart", GO_EXPLORER,
                       sprintf("Shiny.setInputValue('%s', {code: '%s', nonce: Math.random()}, {priority: 'event'}); ", ns("go_explore"), cd))
    } else if (k == "census") {
      m <- CENSUS_MEASURES[[key]]; cu <- census_uid(a); v <- census_value(cu, m); b <- census_value(META$region_uid, m)
      dd <- census_rank(list(uid = META$region_uid, level = "region"), m)
      title <- m$lab; sub <- sprintf("%s · UBOS National Population and Housing Census 2024", if (cu == META$region_uid) "Busoga" else ou_name[[cu]])
      facts <- list(fact(if (cu == META$region_uid) "Busoga" else ou_name[[cu]], pct_txt(v), "census 2024"),
                    if (cu != META$region_uid) fact("Busoga", pct_txt(b), "all 12 districts and cities"),
                    if (nrow(dd)) fact(if (m$good == "high") "Best district" else "Lowest district", pct_txt(if (m$good == "high") dd[.N, v] else dd[1, v]), if (m$good == "high") dd[.N, name] else dd[1, name]),
                    if (nrow(dd)) fact(if (m$good == "high") "Furthest behind" else "Highest district", pct_txt(if (m$good == "high") dd[1, v] else dd[.N, v]), if (m$good == "high") dd[1, name] else dd[.N, name]))
      body <- list(xcard(if (a$level == "region") "Districts and cities compared" else sprintf("Sub-counties of %s compared", ou_name[[OU$uid_l3[match(a$uid, OU$uid)]]] %||% a$name),
                         "dashed line = Busoga", "bx_p1", "420px"),
                   xcard(sprintf("Every census measure: %s against Busoga", if (cu == META$region_uid) "Busoga" else ou_name[[cu]]), "green = better than Busoga, red = worse", "bx_p2", "440px"))
      note <- tagList(tags$b("Source. "), "UBOS National Population and Housing Census 2024, profile tables published on statistics.ubos.org. ",
                      if (m$good == "high") "Higher is better." else "Lower is better.")
      go <- footer_btn("Compare on this page", "chart-bar", sprintf("document.getElementById('%s').scrollIntoView({behavior:'smooth'});", ns("rank_card")),
                       sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); ", ns("set_measure"), key))
    } else if (k == "national") {
      codes <- strsplit(key, "|", fixed = TRUE)[[1]]; dens <- "density" %in% codes; codes <- setdiff(codes, "density")
      x <- CTX$national[code %in% codes]
      lab <- if (dens) "Doctors, nurses and midwives per 10,000 people" else x$label[1]
      ser <- if (dens) x[, .(value = sum(value), n = .N), by = year][n == 2] else x[source == x[order(-year)]$source[1]]
      setorder(ser, year)
      title <- lab; sub <- sprintf("Uganda · %s · national figures, published yearly", paste(unique(x$source), collapse = " and "))
      fmt <- function(v) if (is.na(v)) "–" else if (abs(v) < 10) sprintf("%.2f", v) else format(round(v, 1), big.mark = ",")
      first <- ser[year >= max(year) - 10][1]
      facts <- list(fact("Latest", fmt(ser[.N, value]), as.character(ser[.N, year])),
                    fact("Ten years earlier", fmt(first$value), as.character(first$year)),
                    fact("Change", if (nrow(ser) > 1) sprintf("%+.1f%%", 100 * (ser[.N, value] - first$value) / abs(first$value)) else "–", sprintf("%d to %d", first$year, ser[.N, year])),
                    if (dens) fact("WHO threshold", "44.5", "per 10,000, for the SDG health targets"))
      body <- list(xcard(sprintf("%s, Uganda, %d to %d", lab, min(x$year), max(x$year)), if (length(unique(x$source)) > 1) "each line is one source" else NULL, "bx_p1", "380px"))
      note <- tagList(tags$b("Why national? "), "These measures are published only for Uganda as a whole. They set the context for Busoga; district figures appear here as soon as a regularly updated source publishes them.")
    } else if (k == "report") {
      ds <- key; r <- REP[dataset == ds & period >= p$from & period <= p$to & uid %in% facs$uid]
      rb <- REP[dataset == ds & period >= p$from & period <= p$to]
      pc <- function(r) if (sum(r$expected)) 100 * sum(r$actual) / sum(r$expected) else NA
      nm <- if (ds == "RtEYsASU7PG") "Monthly OPD report (105:01)" else "Supplies, outreach and management report (105:06-09)"
      title <- sprintf("Reporting: %s", nm); sub <- sprintf("%s · %s", a$name, p$label)
      facts <- list(fact("Reports received", pct_txt(pc(r)), sprintf("%s of %s expected", format(sum(r$actual), big.mark = ","), format(sum(r$expected), big.mark = ","))),
                    fact("On time", pct_txt(if (sum(r$expected)) 100 * sum(r$on_time) / sum(r$expected) else NA), "by the 7th of the next month"),
                    fact("Facilities reporting", format(uniqueN(r[actual > 0, uid]), big.mark = ","), sprintf("of %s expected", format(uniqueN(r$uid), big.mark = ","))),
                    if (a$level != "region") fact("Busoga", pct_txt(pc(rb)), "reports received"))
      body <- list(xcard(sprintf("Reports received and on time, %s", a$name), "share of expected reports, monthly; shaded band = selected period", "bx_p1", "380px"),
                   xcard(sprintf("Reports received by %s, %s", kd$lab, p$label), NULL, "bx_p2", "340px"))
      go <- footer_btn("Open the Data quality page", "clipboard-check", go_to_page("dq"))
    } else if (k == "dq") {
      d <- DQF[uid %in% facs$uid]
      title <- "Data quality score of facilities"; sub <- sprintf("%s · last 12 months", a$name)
      facts <- list(fact("Average score", sprintf("%.0f / 100", mean(d$dq_score, na.rm = TRUE)), sprintf("%s facilities", format(sum(!is.na(d$dq_score)), big.mark = ","))),
                    fact("Scoring 80 or more", pct_txt(100 * mean(d$dq_score >= 80, na.rm = TRUE)), "of facilities"),
                    fact("Regular reporters", format(sum(d$report_status == "Regular reporter", na.rm = TRUE), big.mark = ","), "facilities"),
                    fact("Outliers flagged", format(sum(d$outliers_12m, na.rm = TRUE), big.mark = ","), "values, last 12 months"))
      body <- list(xcard("How facility scores are spread", "number of facilities by data quality score", "bx_p1", "300px"),
                   xcard(sprintf("Average score by %s", kd$lab), NULL, "bx_p2", "340px"))
      note <- tagList(tags$b("How the score is made. "), "The average of four checks over the last 12 months: reports received, reports on time, consistency of related numbers, and values that are not outliers.")
      go <- footer_btn("Open the Data quality page", "clipboard-check", go_to_page("dq"))
    } else if (k == "stock") {
      s <- sup_summary(facs$uid, p$from, p$to); req(!is.null(s))
      sb <- if (a$level != "region") sup_summary(OU[level_name == "facility", uid], p$from, p$to)
      title <- "Medicines and supplies: availability and stock-outs"; sub <- sprintf("%s · %s · HMIS 105:06-09 monthly report", a$name, p$label)
      facts <- list(fact("Tracer items available", pct_txt(100 - s$avg), sprintf("on average, of %d tracer items", length(SUP_TRACER))),
                    fact("Reports with a tracer stock-out", pct_txt(s$any), sprintf("%s facility reports", format(s$n, big.mark = ","))),
                    fact("Most often out of stock", as.character(s$items$item[1]) %||% "–", if (nrow(s$items)) sprintf("%s of reports", pct_txt(s$items$rate[1]))),
                    if (!is.null(sb)) fact("Busoga", pct_txt(100 - sb$avg), "tracer items available"))
      its <- SUP$items[, .(item, group)]
      ch <- c(list("Summary" = c("Any tracer item" = "__any")), split(setNames(its$item, its$item), its$group))
      body <- list(
        div(class = "bx-pick", selectInput(ns("bx_item"), "Show one item or the whole tracer basket", ch, selected = "__any", width = "420px")),
        xcard("Availability over time", "green = share of facility reports with the item(s) in stock all month; red = out of stock at some point; shaded band = selected period", "bx_p1", "360px"),
        xcard(sprintf("Every item, both sides: in stock and out of stock, %s", p$label), "share of the facility reports that list each item; sorted from least to most available; bold = tracer item", "bx_p2", "820px"),
        layout_columns(col_widths = c(7, 5),
          xcard(sprintf("By %s", kd$lab), "selected item or basket", "bx_p3", "340px"),
          xcard("By facility level", "selected item or basket", "bx_p4", "340px")),
        card(class = "xp-card", card_header("Facilities with the most days out of stock", span(class = "sub", "selected item or basket, selected period")),
             tableOutput(ns("bx_tab"))))
      note <- tagList(tags$b("How to read this. "), "A stock-out is one or more days without the item in the month, as reported by the facility on the HMIS 105:06-09 form. ",
                      "An item counts for a facility in a month only if the facility had it in stock in at least one of the three months before, because many facilities report 30 days out of stock, month after month, for items they never hold. ",
                      "Stock-out figures from routine reports are known to be noisy; use them to spot patterns and follow up, not as audited availability. ",
                      "The 2019 and revised forms are combined item by item.")
    } else if (k == "mgmt") {
      whats <- mg_whats(key)
      m <- mg_summary(facs$uid, p$from, p$to, whats); req(!is.null(m))
      mb <- if (a$level != "region") mg_summary(OU[level_name == "facility", uid], p$from, p$to, whats)
      title <- if (key == "supervision") "Support supervision" else "Management, review and accountability meetings"
      sub <- sprintf("%s · %s · HMIS 105:06-09 monthly report", a$name, p$label)
      facts <- list(fact("Reports with at least one held", pct_txt(m$share), sprintf("of %s facility monthly reports", format(m$reports, big.mark = ","))),
                    fact("Held", format(m$conducted, big.mark = ","), if (m$planned > 0) sprintf("%s planned", format(m$planned, big.mark = ",")) else "planned numbers not reported"),
                    fact("Facilities involved", format(m$facs, big.mark = ","), "with at least one held"),
                    if (!is.null(mb)) fact("Busoga", pct_txt(mb$share), "reports with at least one held"))
      body <- list(xcard("Month by month", "bars = number held; line = share of facility reports with at least one; shaded band = selected period", "bx_p1", "360px"),
                   layout_columns(col_widths = c(6, 6),
                     xcard("By type", sprintf("share of facility reports with at least one held, %s", p$label), "bx_p2", "320px"),
                     xcard(sprintf("By %s", kd$lab), sprintf("share of facility reports with at least one held, %s", p$label), "bx_p3", "320px")))
      note <- tagList(tags$b("Source. "), "Numbers planned and held, reported by each facility on the HMIS 105:06-09 form. The share uses the facility reports received as its base, so facilities that report more often are not over-counted.")
    }
    showModal(modalDialog(
      title = div(class = "xp-title", style = sprintf("--th:%s", col),
                  span(class = "theme-badge", fontawesome::fa(icon, fill = "#fff", height = "1em")),
                  div(div(class = "xp-h", title), div(class = "xp-sub", sub))),
      size = "xl", easyClose = TRUE, fade = TRUE,
      div(class = "xp-facts", style = sprintf("--th:%s", col), facts),
      body,
      if (!is.null(note)) div(class = "xp-def", note),
      footer = tagList(tags$button(type = "button", class = "btn btn-outline-secondary", `data-bs-dismiss` = "modal", "Close"), go)))
    bx$kind <- k; bx$key <- key; bx$col <- col; bx$nonce <- runif(1)
  })
  observeEvent(input$go_explore, session$userData$explore(list(codes = input$go_explore$code, level = area()$level, uid = area()$uid, nonce = runif(1))))
  observeEvent(input$set_measure, updateSelectInput(session, "measure", selected = input$set_measure))

  # selected item (stock view)
  sel_items <- reactive({ i <- input$bx_item %||% "__any"; if (i == "__any") SUP_TRACER else i })
  stock_rate <- function(fu, from, to, items, by = NULL) {
    s <- sup_frame(fu, from, to); if (is.null(s) || !nrow(s$st)) return(data.table())
    x <- s$st[item %in% items, .(out = any(days > 0)), by = .(uid, period)]
    if (!is.null(by)) x <- merge(x, by, by = "uid")
    x[, .(v = 100 * mean(out), n = .N), by = c(if (!is.null(by)) "grp" else "period")]
  }
  mg_rate <- function(fu, from, to, whats, by = NULL, per = "grp") {
    rp <- SUP$reports[uid %in% fu & period >= from & period <= to]
    held <- unique(SUP$mgmt[uid %in% fu & period >= from & period <= to & what %in% whats & conducted > 0, .(uid, period)])
    rp <- copy(rp)[, held := FALSE][held, held := TRUE, on = .(uid, period)]
    if (!is.null(by)) rp <- merge(rp, by, by = "uid")
    rp[, .(v = 100 * mean(held), n = .N), by = per]
  }

  output$bx_p1 <- renderPlotly({
    req(bx$kind); bx$nonce; a <- area(); p <- per(); k <- bx$kind; col <- bx$col; facs <- area_facs(a)
    if (k == "hmis") {
      cd <- bx$key
      tr <- summarise_ind(cd, a$level, MONTH_MIN, MONTH_MAX, uids = a$uid, by = "month")[order(bucket)][, date := ym_date(bucket)]
      validate(need(nrow(tr) > 1, "Not enough data for a trend."))
      g <- plot_ly(tr, x = ~date) |> add_lines(y = ~value, name = a$name, line = list(color = col, width = 2.4))
      if (a$level != "region") { rb <- summarise_ind(cd, "region", MONTH_MIN, MONTH_MAX, by = "month")[order(bucket)][, date := ym_date(bucket)]
        g <- g |> add_lines(data = rb, x = ~date, y = ~value, name = "Busoga", line = list(color = BRAND$muted, width = 1.4, dash = "dot")) }
      tv <- target_value_line(cd, monthly = TRUE)
      if (is.finite(tv)) g <- g |> add_lines(x = range(tr$date), y = c(tv, tv), name = paste("Target", target_text(cd)), line = list(color = "#b3261e", width = 1.4, dash = "dash"), hoverinfo = "skip")
      return(g |> plotly_base(ytitle = unit_label(cd)) |> layout(shapes = shade(p, col), xaxis = range_x, margin = list(l = 10, r = 10, t = 36, b = 10)))
    }
    if (k == "census") {
      m <- CENSUS_MEASURES[[bx$key]]; x <- census_rank(a, m); validate(need(nrow(x) > 0, "No census data for this area."))
      return(rank_plot(x, a, m, col = "#6a5a2b"))
    }
    if (k == "national") {
      codes <- strsplit(bx$key, "|", fixed = TRUE)[[1]]; dens <- "density" %in% codes; codes <- setdiff(codes, "density")
      x <- CTX$national[code %in% codes]; validate(need(nrow(x) > 0, "No data."))
      g <- plot_ly()
      if (dens) {
        tot <- x[, .(value = sum(value), n = .N), by = year][n == 2][order(year)]
        g <- g |> add_lines(data = tot, x = ~year, y = ~value, name = "Doctors, nurses and midwives", line = list(color = col, width = 2.6), mode = "lines+markers") |>
          add_lines(data = x[code == "HWF_0006"][order(year)], x = ~year, y = ~value, name = "Nurses and midwives", line = list(color = "#80CBC4", width = 1.6)) |>
          add_lines(data = x[code == "HWF_0001"][order(year)], x = ~year, y = ~value, name = "Doctors", line = list(color = "#004D40", width = 1.6)) |>
          add_lines(x = range(x$year), y = c(44.5, 44.5), name = "WHO threshold 44.5", line = list(color = "#b3261e", dash = "dash", width = 1.3), hoverinfo = "skip")
      } else {
        srcs <- unique(x$source)
        for (i in seq_along(srcs)) g <- g |> add_trace(data = x[source == srcs[i]][order(year)], x = ~year, y = ~value, type = "scatter", mode = "lines+markers",
                                                       name = srcs[i], line = list(color = c(col, BRAND$muted)[min(i, 2)], width = if (i == 1) 2.6 else 1.4, dash = if (i == 1) "solid" else "dot"),
                                                       marker = list(size = 5, color = c(col, BRAND$muted)[min(i, 2)]))
      }
      return(g |> plotly_base(ytitle = x$label[1], xtitle = "Year") |> layout(xaxis = list(dtick = 2, title = "Year"), yaxis = list(title = if (dens) "per 10,000 people" else "")))
    }
    if (k == "report") {
      r <- REP[dataset == bx$key & uid %in% facs$uid, .(rec = 100 * sum(actual) / sum(expected), ont = 100 * sum(on_time) / sum(expected)), by = period][order(period)][, date := ym_date(period)]
      g <- plot_ly(r, x = ~date) |> add_lines(y = ~rec, name = "Received", line = list(color = col, width = 2.4)) |>
        add_lines(y = ~ont, name = "On time", line = list(color = col, width = 1.4, dash = "dash"))
      if (a$level != "region") { rb <- REP[dataset == bx$key, .(rec = 100 * sum(actual) / sum(expected)), by = period][order(period)][, date := ym_date(period)]
        g <- g |> add_lines(data = rb, x = ~date, y = ~rec, name = "Busoga, received", line = list(color = BRAND$muted, width = 1.3, dash = "dot")) }
      return(g |> plotly_base(ytitle = "% of expected reports") |> layout(shapes = shade(p, col), xaxis = range_x, yaxis = list(ticksuffix = "%", range = c(0, 105)), margin = list(l = 10, r = 10, t = 36, b = 10)))
    }
    if (k == "dq") {
      d <- DQF[uid %in% facs$uid & !is.na(dq_score)]
      return(plot_ly(d, x = ~dq_score, type = "histogram", xbins = list(start = 0, end = 100, size = 5), marker = list(color = col, line = list(color = "#fff", width = 1)),
                     hovertemplate = "Score %{x}: %{y} facilities<extra></extra>") |> plotly_base(xtitle = "Data quality score (0-100)", ytitle = "Facilities", legend = FALSE))
    }
    if (k == "stock") {
      its <- sel_items(); tr <- stock_rate(facs$uid, MONTH_MIN, MONTH_MAX, its)[order(period)][, date := ym_date(period)]
      validate(need(nrow(tr) > 1, "No stock data for this selection."))
      # both sides: reports with the item(s) in stock all month, and with a stock-out
      g <- plot_ly(tr, x = ~date) |>
        add_trace(y = ~100 - v, name = sprintf("%s: in stock all month", a$name), type = "scatter", mode = "lines", fill = "tozeroy",
                  fillcolor = "rgba(46,125,50,.12)", line = list(color = "#2E7D32", width = 2.4), customdata = ~n,
                  hovertemplate = "%{x|%b %Y}: in stock in %{y:.1f}% of %{customdata} reports<extra></extra>") |>
        add_lines(y = ~v, name = sprintf("%s: out of stock at some point", a$name), line = list(color = col, width = 2.4), customdata = ~n,
                  hovertemplate = "%{x|%b %Y}: out of stock in %{y:.1f}% of %{customdata} reports<extra></extra>")
      if (a$level != "region") { rb <- stock_rate(OU[level_name == "facility", uid], MONTH_MIN, MONTH_MAX, its)[order(period)][, date := ym_date(period)]
        g <- g |> add_lines(data = rb, x = ~date, y = ~100 - v, name = "Busoga: in stock", line = list(color = BRAND$muted, width = 1.4, dash = "dot")) }
      return(g |> plotly_base(ytitle = "% of facility reports") |> layout(shapes = shade(p, col), xaxis = range_x, yaxis = list(ticksuffix = "%", range = c(0, 100)), margin = list(l = 10, r = 10, t = 36, b = 10)))
    }
    if (k == "mgmt") {
      whats <- mg_whats(bx$key)
      x <- SUP$mgmt[uid %in% facs$uid & what %in% whats, .(conducted = sum(conducted)), by = period][order(period)][, date := ym_date(period)]
      sh <- mg_rate(facs$uid, MONTH_MIN, MONTH_MAX, whats, per = "period")[order(period)][, date := ym_date(period)]
      validate(need(nrow(x) > 0, "No data for this selection."))
      return(plot_ly(x, x = ~date) |> add_bars(y = ~conducted, name = "Number held", marker = list(color = adjustcolor(col, .45))) |>
               add_lines(data = sh, x = ~date, y = ~v, name = "% of facility reports with one held", yaxis = "y2", line = list(color = col, width = 2.4)) |>
               plotly_base(ytitle = "Number held") |>
               layout(shapes = shade(p, col), xaxis = range_x, margin = list(l = 10, r = 50, t = 36, b = 10),
                      yaxis2 = list(overlaying = "y", side = "right", ticksuffix = "%", rangemode = "tozero", showgrid = FALSE, title = "")))
    }
  })

  output$bx_p2 <- renderPlotly({
    req(bx$kind); bx$nonce; a <- area(); p <- per(); k <- bx$kind; col <- bx$col; kd <- kids_of(a); facs <- area_facs(a)
    grp <- kd$facs[, .(uid, grp = get(kd$col))]
    named <- function(d) d[, name := ou_name[grp]][!is.na(name)]
    if (k == "hmis") {
      cd <- bx$key; lvl <- if (a$level == "region") "district" else "subcounty"
      s <- summarise_ind(cd, lvl, p$from, p$to)
      if (a$level != "region") s <- s[uid %in% unique(grp$grp)]
      validate(need(nrow(s) > 0, "No area data for this indicator."))
      s[, name := ou_name[uid]]; s[, st := target_status(value_raw, cd, p$n)]; s[, fill := fifelse(!is.na(st) & st == "below", "#c0392b", col)]
      return(hbar(s[, .(name, v = value, fill)], unit_label(cd), col, suffix = "", fmt = function(v) fmt_val(v, cd), ref = target_value_line(cd)))
    }
    if (k == "census") {
      cu <- census_uid(a)
      x <- rbindlist(lapply(names(CENSUS_MEASURES), function(m) { M <- CENSUS_MEASURES[[m]]
        data.table(name = M$lab, v = census_value(cu, M), b = census_value(META$region_uid, M), good = M$good) }))[is.finite(v)]
      x[, better := fifelse(abs(v - b) < 1, NA, (v > b) == (good == "high"))]
      x[, fill := fifelse(is.na(better), "#a79a6d", fifelse(better, "#2e7d32", "#c0392b"))]
      x <- x[order(v)]
      g <- plot_ly(x, y = ~factor(name, levels = name)) |>
        add_bars(x = ~v, name = if (cu == META$region_uid) "Busoga" else ou_name[[cu]], orientation = "h", marker = list(color = ~fill),
                 text = ~sprintf("%.0f%%", v), textposition = "outside", cliponaxis = FALSE, hovertemplate = "%{y}: %{text}<extra></extra>")
      if (cu != META$region_uid) g <- g |> add_markers(x = ~b, name = "Busoga", marker = list(symbol = "line-ns", size = 16, line = list(width = 2.5, color = BRAND$ink)),
                                                       hovertemplate = "Busoga: %{x:.0f}%<extra></extra>")
      return(g |> plotly_base(xtitle = "% of households or people", legend = cu != META$region_uid) |>
               layout(xaxis = list(ticksuffix = "%", range = c(0, 105)), yaxis = list(title = "", tickfont = list(size = 11)), margin = list(l = 10, r = 40, t = 10, b = 10)))
    }
    if (k == "report") {
      r <- merge(REP[dataset == bx$key & period >= p$from & period <= p$to], grp, by = "uid")[, .(v = 100 * sum(actual) / sum(expected)), by = grp]
      validate(need(nrow(r) > 0, "No reports for this selection.")); named(r)
      rb <- REP[dataset == bx$key & period >= p$from & period <= p$to]
      return(hbar(r[, .(name, v)], "% of expected reports received", col, ref = 100 * sum(rb$actual) / sum(rb$expected)))
    }
    if (k == "dq") {
      d <- merge(DQF, grp, by = "uid")[, .(v = mean(dq_score, na.rm = TRUE)), by = grp][is.finite(v)]
      validate(need(nrow(d) > 0, "No scores for this selection.")); named(d)
      return(hbar(d[, .(name, v)], "Average data quality score", col, suffix = "", fmt = function(v) sprintf("%.0f", v), ref = mean(DQF$dq_score, na.rm = TRUE)))
    }
    if (k == "stock") {
      s <- sup_frame(facs$uid, p$from, p$to); validate(need(!is.null(s) && nrow(s$st) > 0, "No reports for this selection."))
      x <- merge(SUP$items[, .(item, group, tracer)],
                 s$st[, .(n = .N, ins = sum(days == 0), part = sum(days > 0 & days < 28), all = sum(days >= 28)), by = item][, item := as.character(item)], by = "item")[n >= 5]
      x[, `:=`(p_in = 100 * ins / n, p_part = 100 * part / n, p_all = 100 * all / n)]
      x[, name := ifelse(tracer == 1, paste0("<b>", item, "</b>"), item)]
      setorder(x, p_in)
      lv <- x$name
      mk <- function(g, col, v, lab) g |> add_bars(data = x, y = ~factor(name, levels = lv), x = x[[v]], name = lab, orientation = "h", marker = list(color = col),
                                                   customdata = ~group, hovertemplate = paste0("%{y}<br>%{customdata}<br>", lab, ": %{x:.1f}% of reports<extra></extra>"))
      g <- plot_ly() |> mk("#2E7D32", "p_in", "In stock all month") |> mk("#F4A259", "p_part", "Out for part of the month") |> mk("#B2182B", "p_all", "Out all month (28+ days)")
      return(g |> plotly_base(xtitle = "% of the facility reports that list the item") |>
               layout(barmode = "stack", yaxis = list(title = "", tickfont = list(size = 10.5)), xaxis = list(ticksuffix = "%", range = c(0, 100)),
                      legend = list(orientation = "h", y = 1.02, x = 0, yanchor = "bottom"), margin = list(l = 10, r = 20, t = 30, b = 10)))
    }
    if (k == "mgmt") {
      x <- rbindlist(lapply(mg_whats(bx$key), function(w) mg_rate(facs$uid, p$from, p$to, w, by = data.table(uid = facs$uid, grp = w))))
      validate(need(nrow(x) > 0, "No reports in this period."))
      return(hbar(x[, .(name = grp, v)], "% of facility reports with at least one held", col, fmt = function(v) sprintf("%.1f%%", v)))
    }
  })

  output$bx_p3 <- renderPlotly({
    req(bx$kind %in% c("stock", "mgmt")); bx$nonce; a <- area(); p <- per(); col <- bx$col; kd <- kids_of(a)
    grp <- kd$facs[, .(uid, grp = get(kd$col))]
    if (bx$kind == "stock") {
      x <- stock_rate(grp$uid, p$from, p$to, sel_items(), by = grp)[n >= 3]
      validate(need(nrow(x) > 0, "Not enough reports to compare areas."))
      x[, name := ou_name[grp]]
      return(hbar(x[!is.na(name), .(name, v)], "% of facility reports with a stock-out", col, fmt = function(v) sprintf("%.1f%%", v),
                  ref = { b <- stock_rate(OU[level_name == "facility", uid], p$from, p$to, sel_items(), by = data.table(uid = OU[level_name == "facility", uid], grp = "all")); b$v }))
    }
    x <- mg_rate(grp$uid, p$from, p$to, mg_whats(bx$key), by = grp)[n >= 3]
    validate(need(nrow(x) > 0, "No reports in this period."))
    x[, name := ou_name[grp]]
    hbar(x[!is.na(name), .(name, v)], "% of facility reports with at least one held", col, fmt = function(v) sprintf("%.1f%%", v),
         ref = mg_rate(OU[level_name == "facility", uid], p$from, p$to, mg_whats(bx$key), by = data.table(uid = OU[level_name == "facility", uid], grp = "all"))$v)
  })

  output$bx_p4 <- renderPlotly({
    req(bx$kind == "stock"); bx$nonce; a <- area(); p <- per(); col <- bx$col
    f <- area_facs(a)[!is.na(grp_level) & nzchar(grp_level), .(uid, grp = grp_level)]
    x <- stock_rate(f$uid, p$from, p$to, sel_items(), by = f)[n >= 3]
    validate(need(nrow(x) > 0, "Not enough reports."))
    hbar(x[, .(name = grp, v)], "% of facility reports with a stock-out", col, fmt = function(v) sprintf("%.1f%%", v))
  })

  output$bx_tab <- renderTable({
    req(bx$kind == "stock"); bx$nonce; a <- area(); p <- per()
    s <- sup_frame(area_facs(a)$uid, p$from, p$to); req(!is.null(s))
    x <- s$st[item %in% sel_items() & days > 0, .(days = sum(days), months = uniqueN(period), items = uniqueN(item)), by = uid][order(-days)][1:min(.N, 10)]
    req(nrow(x) > 0)
    x[, .(Facility = ou_name[uid], `Sub-county` = OU$subcounty[match(uid, OU$uid)], District = OU$district[match(uid, OU$uid)],
          `Days out of stock` = format(days, big.mark = ","), `Months affected` = months, `Items affected` = items)]
  }, striped = TRUE, spacing = "s", width = "100%")
})
