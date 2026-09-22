# 04d_education.R
# Education context for every sub-county, DLG, district and Busoga:
#   schools by level (OpenStreetMap; see scripts/fetch_context_layers.py for the classification)
#   children aged 5-19 (WorldPop age/sex estimates, from R/04c_population.R)
#   schools per 10,000 children aged 5-19
#   distance from each school to the nearest DHIS2 health facility (straight line, km)
# Writes app/data/education.rds (areas) and app/data/schools.rds (schools with nearest facility).

source("R/00_config.R")
suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)
ou <- fread("data/meta/orgunits.csv", na.strings = "")

sch <- st_read("app/data/geo/osm_schools.geojson", quiet = TRUE)
sch$category[is.na(sch$category)] <- "Unclassified"
sc  <- st_read("app/data/geo/subcounty.geojson", quiet = TRUE)
j   <- st_join(sch, sc[, c("uid")], join = st_within)
xy  <- st_coordinates(j)
s   <- data.table(name = j$name, category = j$category, lon = xy[, 1], lat = xy[, 2], sc_uid = j$uid)
s   <- s[!is.na(sc_uid)]

# nearest health facility (equirectangular distance is accurate enough at this latitude and scale)
fac <- ou[level == 6 & !is.na(lat), .(fuid = uid, fname = name, flat = lat, flon = lon)]
near <- vapply(seq_len(nrow(s)), function(i) {
  d <- 111.32 * sqrt((fac$flat - s$lat[i])^2 + (cos(s$lat[i] * pi / 180) * (fac$flon - s$lon[i]))^2)
  k <- which.min(d); c(k, d[k])
}, numeric(2))
s[, `:=`(near_fac = fac$fname[near[1, ]], near_km = round(near[2, ], 2))]

par <- ou[level == 5, .(sc_uid = uid, l4 = uid_l4, l3 = uid_l3)]
s <- merge(s, par, by = "sc_uid")
cats <- c("Pre-primary", "Primary", "Secondary", "Technical / vocational", "Tertiary college / institute", "University", "Unclassified")
summ <- function(x) x[, c(setNames(lapply(cats, function(k) sum(category == k)), cats),
                          list(schools = .N, median_km = median(near_km), over5km = sum(near_km > 5)))]
ed <- rbind(s[, summ(.SD), by = .(uid = sc_uid)][, level := "subcounty"],
            s[, summ(.SD), by = .(uid = l4)][, level := "dlg"],
            s[, summ(.SD), by = .(uid = l3)][, level := "district"],
            s[, summ(.SD)][, `:=`(uid = cfg$region_uid, level = "region")], fill = TRUE)

pyr <- readRDS("app/data/pyramid.rds")
kids <- pyr[year == max(year) & age %in% c("5-9", "10-14", "15-19"), .(children_5_19 = sum(pop)), by = .(uid)]
ed <- merge(ed, kids, by = "uid", all.x = TRUE)
ed[, schools_per_10k := round(10000 * schools / children_5_19, 1)]
saveRDS(ed, "app/data/education.rds", compress = "xz")
saveRDS(s[, .(name, category, lon, lat, sc_uid, near_fac, near_km)], "app/data/schools.rds", compress = "xz")
log_msg("education: %d schools in %d sub-counties; median distance to a facility %.1f km; %d schools over 5 km",
        nrow(s), uniqueN(s$sc_uid), median(s$near_km), sum(s$near_km > 5))
