# What data each page is built on: the panel at the right of every page header. Each page gets its
# own four items (what it covers, how recent it is, and where it comes from), so the panel always
# describes the page it sits on. Sourced last (zz_) so every data object is loaded; page_head()
# looks the page up by its key when the UI is built.

page_status <- function(key) {
  i <- function(icon, lab, val) list(icon = icon, lab = lab, val = val)
  latest <- format(ym_date(MONTH_MAX), "%B %Y"); upd <- META$extracted
  nfac <- nrow(OU[level_name == "facility"]); nd <- nrow(OU[level_name == "district"]); nsc <- nrow(OU[level_name == "subcounty"])
  hmis <- i("database", "Source", "Ministry of Health DHIS2 (HMIS)")
  cen24 <- "UBOS census, 10 May 2024"
  wk <- if (exists("EPI") && !is.null(EPI)) max(EPI$week_start) else NA
  clim <- if (exists("CLIM") && !is.null(CLIM)) format(ym_date(max(CLIM$period)), "%B %Y") else "–"
  switch(key,
    overview   = list(i("calendar-check", "Latest month of data", latest), i("rotate", "Last updated", upd),
                      i("hospital", "Coverage", sprintf("%s facilities, %d districts and cities", format(nfac, big.mark = ","), nd)), hmis),
    facilities = list(i("hospital", "Facilities", sprintf("%s in the DHIS2 register", format(nfac, big.mark = ","))),
                      i("location-dot", "With map coordinates", format(nrow(OU[level_name == "facility" & !is.na(lat)]), big.mark = ",")),
                      i("calendar-check", "Latest month of data", latest), i("database", "Source", "DHIS2 facility register and monthly reports")),
    explorer   = list(i("chart-line", "Indicators", sprintf("%d, in %d programme areas", nrow(IND), uniqueN(IND$theme))),
                      i("calendar", "Months covered", sprintf("%s to %s", format(ym_date(MONTH_MIN), "%b %Y"), format(ym_date(MONTH_MAX), "%b %Y"))),
                      i("rotate", "Last updated", upd), hmis),
    targets    = list(i("bullseye", "Indicators with a target", nrow(TGT)), i("book", "Target source", "MoH Strategic Plan and sector reports"),
                      i("calendar-check", "Latest month of data", latest), hmis),
    blocks     = list(i("cubes", "Framework", "WHO health system building blocks"), i("pills", "Medicines and management", "HMIS 105:06-09, monthly"),
                      i("house", "Living conditions", cen24), i("globe", "National context", "World Bank and WHO, yearly")),
    scorecard  = list(i("table-cells", "Indicators scored", nrow(IND)), i("sitemap", "Areas", sprintf("%d districts, %d sub-counties", nd, nsc)),
                      i("calendar-check", "Latest month of data", latest), hmis),
    compare    = list(i("code-compare", "Compare", "any areas or facilities"), i("calendar", "Months covered", sprintf("%s to %s", format(ym_date(MONTH_MIN), "%b %Y"), format(ym_date(MONTH_MAX), "%b %Y"))),
                      i("rotate", "Last updated", upd), hmis),
    drill      = list(i("sitemap", "Levels", "Busoga, district, DLG, sub-county, facility"), i("hospital", "Facilities", format(nfac, big.mark = ",")),
                      i("calendar-check", "Latest month of data", latest), hmis),
    deep       = list(i("id-card", "Profiles", "every area and facility"), i("hospital", "Facilities", format(nfac, big.mark = ",")),
                      i("calendar-check", "Latest month of data", latest), hmis),
    breakdown  = list(i("layer-group", "Breakdowns", "ownership, level, age and sex"), i("hospital", "Facilities", format(nfac, big.mark = ",")),
                      i("calendar-check", "Latest month of data", latest), hmis),
    maps       = list(i("draw-polygon", "Boundaries", sprintf("%d districts, %d DLGs, %d sub-counties", nd, nrow(OU[level_name == "dlg"]), nsc)),
                      i("location-dot", "Facility points", format(nrow(OU[level_name == "facility" & !is.na(lat)]), big.mark = ",")),
                      i("calendar-check", "Latest month of data", latest), i("database", "Source", "DHIS2 boundaries and HMIS; Esri basemaps")),
    atlas      = list(i("earth-africa", "Layers", "facilities, roads, rivers, water, schools, markets"), i("location-dot", "Facilities mapped", format(nrow(OU[level_name == "facility" & !is.na(lat)]), big.mark = ",")),
                      i("map", "Context layers", "OpenStreetMap"), i("database", "Health data", "DHIS2 (HMIS)")),
    spatial    = list(i("fire", "Methods", "Gi* hot spots, Local Moran's I, Mann-Kendall"), i("draw-polygon", "Areas", sprintf("%d sub-counties, %d DLGs", nsc, nrow(OU[level_name == "dlg"]))),
                      i("calendar-check", "Latest month of data", latest), i("database", "Sources", "HMIS, UBOS census 2024, CHIRPS, ERA5")),
    epidemic   = list(i("virus", "Surveillance", "weekly 033B reports"), i("calendar-week", "Latest week", if (is.finite(wk)) format(wk, "Week %V, %Y (from %d %b)") else "–"),
                      i("bell", "Diseases watched", if (exists("EPI") && !is.null(EPI)) uniqueN(EPI$disease) else "–"), i("database", "Source", "DHIS2 weekly surveillance (033B)")),
    climate    = list(i("cloud-rain", "Rainfall", "CHIRPS, monthly"), i("temperature-high", "Temperature and air", "ERA5-Land and CAMS"),
                      i("calendar-check", "Latest month", clim), i("draw-polygon", "Areas", sprintf("%d sub-counties", nsc))),
    forecast   = list(i("chart-area", "Method", "seasonal time-series models"), i("calendar-check", "Trained up to", latest),
                      i("rotate", "Last updated", upd), hmis),
    xai        = list(i("wand-magic-sparkles", "Models", sprintf("%d outcomes, random forests", if (!is.null(XAI)) length(XAI$outcomes) else 0)),
                      i("calendar-check", "Data up to", latest), i("rotate", "Models built", if (!is.null(XAI)) XAI$built else "–"), i("database", "Sources", "HMIS, CHIRPS, ERA5")),
    story      = list(i("people-group", "People", sprintf("%s counted", format(CH[area == "Busoga" & year == 2024, pop], big.mark = ","))),
                      i("calendar-check", "Census night", "10 May 2024"), i("draw-polygon", "Areas", sprintf("%d districts and cities, %d sub-counties", nd, nsc)),
                      i("database", "Sources", "UBOS census 2024 and HMIS")),
    population = list(i("people-group", "Population", "MoH 107a, UBOS-based"), i("chart-simple", "Age and sex", "WorldPop R2025A"),
                      i("school", "Schools mapped", if (exists("SCH") && !is.null(SCH)) format(nrow(SCH), big.mark = ",") else "–"), i("database", "Sources", "UBOS, WorldPop, Ministry of Education")),
    region     = list(i("landmark", "Censuses", "1980, 1991, 2002, 2014 and 2024"), i("chart-line", "Projections", "to 2050, three scenarios"),
                      i("route", "Access and hazards", "HeiGIT 2026"), i("database", "Sources", "UBOS, HeiGIT, WFP, OpenStreetMap")),
    dq         = list(i("clipboard-check", "Facilities scored", if (!is.null(DQF)) format(sum(!is.na(DQF$dq_score)), big.mark = ",") else "–"),
                      i("list-check", "Checks", "completeness, timeliness, consistency, outliers"), i("calendar-check", "Latest month of data", latest), hmis),
    brief      = list(i("file-pdf", "Formats", "PDF (protected) and Word"), i("sitemap", "For", "Busoga, any district, sub-county or facility"),
                      i("calendar-check", "Latest month of data", latest), hmis),
    definitions = list(i("book", "Indicators defined", nrow(IND)), i("calculator", "Each with", "numerator, denominator and source"),
                      i("rotate", "Last updated", upd), hmis),
    about      = list(i("book-open", "Methods", "indicators, quality checks, models"), i("rotate", "Last updated", upd),
                      i("hospital", "Coverage", sprintf("%s facilities", format(nfac, big.mark = ","))), hmis),
    download   = list(i("download", "Downloads", "district-level data, CSV"), i("calendar-check", "Latest month of data", latest),
                      i("rotate", "Last updated", upd), hmis),
    NULL)
}
