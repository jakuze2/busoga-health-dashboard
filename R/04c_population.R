# 04c_population.R
# Population by age and sex (population pyramids) for Busoga and every district, DLG and
# sub-county, plus school-age population, from WorldPop open estimates:
#   WorldPop Global2 R2025A, constrained, 1 km, UN-adjusted, age/sex structures 2015-2030
#   (Bondarenko et al., WorldPop, University of Southampton; CC BY 4.0)
# The 1 km rasters are summed inside each DHIS2 boundary polygon.
# Note: WorldPop totals are modelled estimates and differ from the 107a projections in DHIS2;
# the pyramid is used for the age and sex structure, and the dashboard labels the source.
# Writes app/data/pyramid.rds.

source("R/00_config.R")
suppressPackageStartupMessages({ library(terra); library(sf) })
dir.create("data/worldpop", showWarnings = FALSE)

years <- c(2020L, 2026L)
ages  <- c("00", "01", sprintf("%02d", seq(5, 90, 5)))
url_of <- function(sex, age, yr) sprintf(
  "https://data.worldpop.org/GIS/AgeSex_structures/Global_2015_2030/R2025A/%d/UGA/v1/1km_ua/constrained/uga_%s_%s_%d_CN_1km_R2025A_UA_v1.tif",
  yr, sex, age, yr)

region <- st_read("app/data/geo/region.geojson", quiet = TRUE)
bb <- ext(vect(region)) + 0.05
polys <- lapply(c("subcounty", "dlg", "district", "region"), function(l) {
  g <- st_make_valid(st_read(sprintf("app/data/geo/%s.geojson", l), quiet = TRUE)); g$level <- l; g[, c("uid", "level")] })
polys <- vect(do.call(rbind, polys))

out <- list()
for (yr in years) for (sex in c("f", "m")) for (age in ages) {
  f <- sprintf("data/worldpop/busoga_%s_%s_%d.tif", sex, age, yr)
  if (!file.exists(f)) {
    tmp <- tempfile(fileext = ".tif")
    ok <- tryCatch({ download.file(url_of(sex, age, yr), tmp, mode = "wb", quiet = TRUE); TRUE }, error = function(e) FALSE)
    if (!ok) { log_msg("missing %s %s %d", sex, age, yr); next }
    writeRaster(crop(rast(tmp), bb), f, overwrite = TRUE)       # keep only the Busoga window
    unlink(tmp)
  }
  r <- rast(f)
  v <- terra::extract(r, polys, fun = sum, na.rm = TRUE, ID = FALSE, weights = TRUE)[[1]]
  out[[length(out) + 1]] <- data.table(level = polys$level, uid = polys$uid, year = yr,
                                       sex = if (sex == "f") "Female" else "Male", age_start = as.integer(age), pop = v)
}
pyr <- rbindlist(out)
# One official total everywhere: WorldPop gives the age/sex structure, scaled so that each area's
# total equals its 107a projected population in DHIS2 (derived from UBOS) for that year.
off <- readRDS("app/data/population.rds")[, .(level, uid, year, official = pop)]
pyr <- merge(pyr, off, by = c("level", "uid", "year"), all.x = TRUE)
pyr[, wp_total := sum(pop), by = .(level, uid, year)]
pyr[, `:=`(pop_worldpop = pop, pop = fifelse(!is.na(official) & wp_total > 0, pop * official / wp_total, pop))]
pyr[, c("official", "wp_total") := NULL]
pyr[, age := fcase(age_start == 0L, "0", age_start == 1L, "1-4", age_start == 90L, "90+",
                   default = sprintf("%d-%d", age_start, age_start + 4L))]
saveRDS(pyr, "app/data/pyramid.rds", compress = "xz")
tot <- pyr[level == "region", .(pop = round(sum(pop))), by = .(year, sex)]
log_msg("pyramids: %d areas; Busoga WorldPop totals: %s", uniqueN(pyr$uid),
        paste(sprintf("%d %s %s", tot$year, tot$sex, format(tot$pop, big.mark = ",")), collapse = "; "))
