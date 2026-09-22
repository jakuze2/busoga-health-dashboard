# 08_open_data.R
# Open data about Busoga beyond the health system, for the "Busoga in numbers" pages:
#   census history  UBOS 2002 Population and Housing Census and UBOS projections (2002-base, by the
#                   districts of the time), UBOS 2023 subnational projections, DHIS2 107a 2020-2026
#   access          HeiGIT accessibility indicators 2026 (share of people within reach of hospitals,
#                   primary health care and schools), CC BY-SA
#   hazards         HeiGIT risk assessment indicators 2026: flood exposure (people, crops, facilities),
#                   vulnerability, coping capacity, rural population, CC BY-SA
#   food prices     WFP / FAO / UBOS market prices for Busoga markets (Iganga, Jinja), CC BY-IGO
#   commerce        OpenStreetMap banks, ATMs, mobile money, fuel, markets, supermarkets (ODbL)
#   land            UBOS land-use layer 2006: forest reserves, rangeland, water
# Writes app/data/open_*.rds.

source("R/00_config.R")
suppressPackageStartupMessages({ library(sf); library(readxl) })
sf_use_s2(FALSE)
ou <- fread("data/meta/orgunits.csv", na.strings = "")
busoga_codes <- c(Bugiri = "UG2030", Bugweri = "UG2031", Buyende = "UG2038", Iganga = "UG2039", Jinja = "UG2040",
                  Kaliro = "UG2043", Kamuli = "UG2044", Luuka = "UG2051", Mayuge = "UG2053", Namayingo = "UG2055",
                  Namutumba = "UG2057")

# ---- census history ---------------------------------------------------------------------------
x <- as.data.table(read_excel("data/open/ubos_subcounty_pop_2002base.xls", sheet = "District Summary", col_names = FALSE))
yrs <- as.integer(unlist(x[1, ])); yrs <- zoo_fill <- yrs
for (i in seq_along(yrs)) if (is.na(yrs[i]) && i > 1) yrs[i] <- yrs[i - 1]
hdr <- unlist(x[2, ])
dist02 <- c("Bugiri", "Iganga", "Jinja", "Kamuli", "Kaliro", "Mayuge")
rows <- x[toupper(trimws(unlist(x[, 2]))) %in% toupper(dist02)]
hist <- rbindlist(lapply(seq_len(nrow(rows)), function(i) {
  r <- unlist(rows[i]); tot <- which(hdr == "Total")
  data.table(district_2002 = trimws(r[2]), year = yrs[tot], pop = as.numeric(r[tot]),
             source = fifelse(yrs[tot] == 2002L, "UBOS Census 2002", "UBOS projection (2002 census base)"))
}))
ub23 <- readRDS("app/data/ubos_pop.rds")[, .(pop = sum(pop)), by = district][, `:=`(year = 2023L, source = "UBOS projection 2023 (HDX)")]
p107 <- readRDS("app/data/population.rds")[level == "region", .(year, pop, source = "DHIS2 107a projection (MoH)")]
region_hist <- rbind(hist[, .(pop = sum(pop)), by = .(year, source)],
                     ub23[, .(pop = sum(pop)), by = .(year, source)], p107)
saveRDS(list(by_old_district = hist, current_2023 = ub23, region = region_hist), "app/data/open_census.rds", compress = "xz")
log_msg("census history: 2002 census Busoga total %s (6 districts of 2002)",
        format(hist[year == 2002, sum(pop)], big.mark = ","))

# ---- HeiGIT access and risk (district level) -------------------------------------------------
rd <- function(f) fread(file.path("data/open", f))[ADM2_PCODE %in% busoga_codes]
acc  <- rd("heigit_adm2_access.csv"); fac <- rd("heigit_adm2_facilities.csv")
vul  <- rd("heigit_adm2_vulnerability.csv"); cop <- rd("heigit_adm2_coping.csv"); fl <- rd("heigit_adm2_flood_exposure.csv")
nm <- setNames(names(busoga_codes), busoga_codes)
for (d in list(acc, fac, vul, cop, fl)) d[, district := nm[ADM2_PCODE]]
saveRDS(list(access = acc, facilities = fac, vulnerability = vul, coping = cop, flood = fl), "app/data/open_heigit.rds", compress = "xz")
log_msg("HeiGIT: %d Busoga districts", nrow(acc))

# ---- food prices ------------------------------------------------------------------------------
pr <- fread("data/open/wfp_prices_uga.csv", skip = 0)
pr <- pr[market %in% c("Iganga", "Jinja", "Jinja (UBoS)")]
pr[, `:=`(date = as.IDate(date), price = as.numeric(price))]
saveRDS(pr[, .(date, market, category, commodity, unit, pricetype, price, currency)], "app/data/open_prices.rds", compress = "xz")
log_msg("food prices: %d rows, %s to %s", nrow(pr), min(pr$date), max(pr$date))

# ---- commerce points per area ------------------------------------------------------------------
cm <- st_read("app/data/geo/osm_commerce.geojson", quiet = TRUE)
cm$type <- fcase(cm$amenity %in% c("bank", "microfinance", "money_lender"), "Banks & microfinance",
                 cm$amenity %in% c("atm"), "ATMs",
                 cm$amenity %in% c("money_transfer", "bureau_de_change") | cm$shop %in% c("mobile_phone") |
                   grepl("mobile_money", cm$amenity %||% ""), "Mobile money & forex",
                 cm$amenity %in% c("fuel"), "Fuel stations",
                 cm$amenity %in% c("marketplace"), "Markets",
                 cm$shop %in% c("agrarian", "farm"), "Agro-input shops",
                 default = "Shops & wholesale")
sc <- st_make_valid(st_read("app/data/geo/subcounty.geojson", quiet = TRUE))
j  <- st_join(cm, sc[, "uid"], join = st_within)
xy <- st_coordinates(j)
pts <- data.table(name = j$name, type = j$type, lon = xy[, 1], lat = xy[, 2], sc_uid = j$uid)
pts <- merge(pts, ou[level == 5, .(sc_uid = uid, l4 = uid_l4, l3 = uid_l3)], by = "sc_uid", all.x = TRUE)
cnt <- rbind(pts[!is.na(sc_uid), .N, by = .(uid = sc_uid, type)][, level := "subcounty"],
             pts[!is.na(l3), .N, by = .(uid = l3, type)][, level := "district"],
             pts[, .N, by = type][, `:=`(uid = cfg$region_uid, level = "region")])
saveRDS(list(points = pts, counts = cnt), "app/data/open_commerce.rds", compress = "xz")
log_msg("commerce: %d points", nrow(pts))

# ---- land (forest reserves, parks, rangeland, water) -----------------------------------------
lu <- st_read(list.files("data/open/landuse", pattern = "shp$", full.names = TRUE)[1], quiet = TRUE)
lu <- st_make_valid(st_transform(lu, 4326))
dist <- st_make_valid(st_read("app/data/geo/district.geojson", quiet = TRUE))
ix <- suppressWarnings(st_intersection(lu[, "FTYPE"], dist[, c("uid", "name")]))
if (nrow(ix)) {
  ix$km2 <- as.numeric(st_area(st_transform(ix, 32636))) / 1e6
  land <- as.data.table(st_drop_geometry(ix))[, .(km2 = sum(km2)), by = .(uid, district = name, type = FTYPE)]
  dist$km2_total <- as.numeric(st_area(st_transform(dist, 32636))) / 1e6
  land <- merge(land, data.table(uid = dist$uid, km2_total = dist$km2_total), by = "uid")
  land[, pct := 100 * km2 / km2_total]
  saveRDS(list(summary = land, shapes = st_simplify(ix, dTolerance = 0.002)), "app/data/open_land.rds", compress = "xz")
  log_msg("land: %d district x land-type rows", nrow(land))
}
