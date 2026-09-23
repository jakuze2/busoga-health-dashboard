# 08b_context.R
# Context for the health-system building blocks page:
#   1. UBOS National Population and Housing Census 2024 profile tables (statistics.ubos.org, the
#      portal's own data service) for Busoga, its 12 districts and cities and every sub-county:
#      health insurance, mosquito nets, water, sanitation, electricity, birth registration,
#      ICT, schooling, employment, Parish Development Model, mental health.
#   2. National health-system indicators for Uganda that are updated every year: World Bank
#      World Development Indicators (health spending, workforce, beds, UHC index) and Worldwide
#      Governance Indicators.
# Writes app/data/context.rds. Runs in the monthly refresh; the census part is re-read only when
# its cached copy (data/open/ubos_nphc2024.rds) is missing, since the census does not change.

suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
`%||%` <- function(a, b) if (is.null(a)) b else a
log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), sprintf(...), "\n", sep = "")
get_json <- function(url, tries = 4) {
  for (i in seq_len(tries)) {
    r <- tryCatch(curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = 60, connecttimeout = 20)), error = function(e) NULL)
    if (!is.null(r) && r$status_code == 200) return(fromJSON(rawToChar(r$content), simplifyVector = FALSE))
    Sys.sleep(2 * i)
  }
  stop("could not read ", url)
}
dir.create("data/open", recursive = TRUE, showWarnings = FALSE)

# ---- 1. UBOS census 2024 ---------------------------------------------------------------------
UBOS <- "https://statistics.ubos.org/nphc/api/"
cache <- "data/open/ubos_nphc2024.rds"
if (!file.exists(cache)) {
  flat <- function(level, code) {
    j <- get_json(sprintf("%sget_profile_data.php?level=%d&location_code=%s&format=detailed", UBOS, level, code))
    rbindlist(lapply(j$data, function(t) {
      if (!length(t$records)) return(NULL)
      rec <- t$records[[1]]
      cols <- names(t$columns)
      data.table(category = t$category, table = t$table_name, col_key = cols,
                 column = vapply(t$columns, function(c) c$name, ""),
                 value = vapply(cols, function(k) { v <- rec[[k]]; if (is.null(v)) NA_real_ else as.numeric(v) }, 0),
                 ubos_name = rec$location_name)
    }))
  }
  out <- list(flat(0, "21")[, `:=`(level = "region", code = "21", district = "BUSOGA")])
  dists <- get_json(paste0(UBOS, "get_districts.php?subregion_code=21"))
  for (d in dists) {
    out[[length(out) + 1]] <- flat(1, d$code)[, `:=`(level = "district", code = d$code, district = d$name)]
    for (cty in get_json(paste0(UBOS, "get_counties.php?district_code=", d$code)))
      for (sc in get_json(paste0(UBOS, "get_subcounties.php?county_code=", cty$code))) {
        x <- tryCatch(flat(3, sc$code), error = function(e) NULL)
        if (!is.null(x) && nrow(x)) out[[length(out) + 1]] <- x[, `:=`(level = "subcounty", code = sc$code, district = d$name)]
      }
    log_msg("UBOS census 2024: %s done", d$name)
  }
  saveRDS(rbindlist(out, fill = TRUE), cache, compress = "xz")
}
census <- readRDS(cache)
# The portal's level-0 (sub-region) profile for code 21 returns a single district, not Busoga, so
# Busoga is built here from its 12 districts: counts are summed, percentages are re-derived from
# their numbers (unemployment and NEET), and household size is population / households.
census <- census[level != "region"]
reg <- census[level == "district", .(value = sum(value, na.rm = TRUE)), by = .(category, table, col_key, column)]
pc <- census[level == "district" & grepl("\\(%\\)$", column)]
if (nrow(pc)) {
  nums <- census[level == "district" & grepl("\\(Number\\)$", column), .(table, district, stem = sub(" \\(Number\\)$", "", column), num = value)]
  pc <- merge(pc[, .(table, district, column, stem = sub(" \\(%\\)$", "", column), pct = value)], nums, by = c("table", "district", "stem"))
  pcr <- pc[pct > 0, .(v = 100 * sum(num) / sum(num / (pct / 100))), by = .(table, column)]
  reg[pcr, on = .(table, column), value := i.v]
}
hh <- reg[table == "Household Size" & column %in% c("Household Population", "Number of Households")]
if (nrow(hh) == 2) reg[table == "Household Size" & column == "Average Household Size", value := hh[column == "Household Population", value] / hh[column == "Number of Households", value]]
reg[, `:=`(ubos_name = "BUSOGA", level = "region", code = "21", district = "BUSOGA")]
census <- rbind(reg, census, fill = TRUE)

# match UBOS areas to DHIS2 org units by name (within district for sub-counties)
ou <- fread("app/data/orgunits.csv", na.strings = "")
norm <- function(x) { x <- tolower(x); x <- gsub("\\b(district|sub[- ]?county|subcounty|town council|division|municipality|municipal council|tc|city)\\b", "", x)
  gsub("[^a-z]", "", x) }
dist_ou <- ou[level_name == "district", .(uid, name, n = norm(name), city = grepl("city", name, ignore.case = TRUE))]
census[level == "district", dkey := paste0(norm(district), grepl("CITY", district))]
dist_ou[, dkey := paste0(n, city)]
census[level == "district", uid := dist_ou$uid[match(dkey, dist_ou$dkey)]]
census[level == "region", uid := ou[level_name == "region", uid][1]]
dmap <- unique(census[level == "district", .(district, duid = uid)])
census[, duid := dmap$duid[match(district, dmap$district)]]
sc_ou <- ou[level_name == "subcounty", .(uid, name, duid = uid_l3, n = norm(name))]
scs <- unique(census[level == "subcounty", .(code, ubos_name, duid)])
scs[, uid := mapply(function(nm, d) {
  cand <- sc_ou[duid == d]; k <- norm(nm)
  hit <- cand[n == k, uid]; if (length(hit)) return(hit[1])
  a <- agrep(k, cand$n, max.distance = 0.15, value = FALSE); if (length(a) == 1) cand$uid[a] else NA_character_
}, ubos_name, duid)]
census[level == "subcounty", uid := scs$uid[match(code, scs$code)]]
log_msg("UBOS census 2024: %d districts matched of %d; %d sub-counties matched of %d",
        uniqueN(census[level == "district" & !is.na(uid), code]), uniqueN(census[level == "district", code]),
        uniqueN(census[level == "subcounty" & !is.na(uid), code]), uniqueN(census[level == "subcounty", code]))

# ---- 2. national indicators (World Bank WDI and WGI) ------------------------------------------
WB <- c(
  SH.XPD.CHEX.PC.CD = "Current health expenditure per person (US$)",
  SH.XPD.CHEX.GD.ZS = "Current health expenditure (% of GDP)",
  SH.XPD.GHED.CH.ZS = "Government share of health spending (% of current health expenditure)",
  SH.XPD.OOPC.CH.ZS = "Out-of-pocket spending (% of current health expenditure)",
  SH.XPD.EHEX.CH.ZS = "External (donor) share of health spending (% of current health expenditure)",
  SH.MED.PHYS.ZS    = "Medical doctors per 1,000 people",
  SH.MED.NUMW.P3    = "Nurses and midwives per 1,000 people",
  SH.MED.BEDS.ZS    = "Hospital beds per 1,000 people",
  SH.UHC.SRVS.CV.XD = "UHC service coverage index (0-100)",
  GE.EST            = "Government effectiveness (WGI, -2.5 to 2.5)",
  CC.EST            = "Control of corruption (WGI, -2.5 to 2.5)",
  RQ.EST            = "Regulatory quality (WGI, -2.5 to 2.5)")
wb_up <- TRUE                                        # stop asking once the service fails to answer
national <- rbindlist(lapply(names(WB), function(code) {
  if (!wb_up) return(NULL)
  j <- tryCatch(get_json(sprintf("https://api.worldbank.org/v2/country/UGA/indicator/%s?format=json&per_page=100&date=2000:2030", code), tries = 2),
                error = function(e) { wb_up <<- FALSE; NULL })
  if (is.null(j) || length(j) < 2 || !length(j[[2]])) return(NULL)
  rbindlist(lapply(j[[2]], function(r) if (is.null(r$value)) NULL else
    data.table(code = code, label = WB[[code]], year = as.integer(r$date), value = as.numeric(r$value))))
}))
if (!nrow(national)) national <- data.table(code = character(), label = character(), year = integer(), value = numeric())
log_msg("World Bank: %d indicators, %d values", uniqueN(national$code), nrow(national))
# WHO Global Health Observatory: the same health-system measures from WHO's own databases (used
# for any indicator the World Bank service did not return, and for the WHO-only ones)
GHO <- c(
  GHED_CHE_pc_US_SHA2011   = "Current health expenditure per person (US$)",
  GHED_CHEGDP_SHA2011      = "Current health expenditure (% of GDP)",
  GHED_GGHE_DCHE_SHA2011   = "Government share of health spending (% of current health expenditure)",
  GHED_OOPSCHE_SHA2011     = "Out-of-pocket spending (% of current health expenditure)",
  GHED_EXTCHE_SHA2011      = "External (donor) share of health spending (% of current health expenditure)",
  HWF_0001                 = "Medical doctors per 10,000 people",
  HWF_0006                 = "Nurses and midwives per 10,000 people",
  UHC_INDEX_REPORTED       = "UHC service coverage index (0-100)",
  SDGIHR2021               = "International Health Regulations core capacity (average, %)")
gho <- rbindlist(lapply(names(GHO), function(code) {
  j <- tryCatch(get_json(sprintf("https://ghoapi.azureedge.net/api/%s?$filter=SpatialDim%%20eq%%20%%27UGA%%27", code)), error = function(e) NULL)
  if (is.null(j) || !length(j$value)) return(NULL)
  x <- rbindlist(lapply(j$value, function(r) data.table(year = as.integer(r$TimeDim), value = as.numeric(r$NumericValue %||% NA),
                                                        dim1 = r$Dim1 %||% NA_character_)))
  x <- x[is.na(dim1) | dim1 %in% c("SEX_BTSX", "BTSX")][!is.na(value)]
  x[, .(value = value[1]), by = year][, `:=`(code = code, label = GHO[[code]])]
}), fill = TRUE)
if (!nrow(gho)) gho <- data.table(code = character(), label = character(), year = integer(), value = numeric())
log_msg("WHO GHO: %d indicators, %d values", uniqueN(gho$code), nrow(gho))
national <- rbind(national[, source := "World Bank"], gho[, .(code, label, year, value, source = "WHO GHO")], fill = TRUE)

saveRDS(list(census = census, national = national, built = format(Sys.Date(), "%d %B %Y")), "app/data/context.rds", compress = "xz")
log_msg("context written")
