# 04b_climate.R
# Monthly climate indicators for every sub-county, DLG, district and Busoga (inputs from
# scripts/fetch_climate.py):
#   rain_mm     monthly rainfall: CHIRPS v2.0 (0.05 deg) averaged over each boundary polygon
#   rain_anom   % difference from the unit's own 1991-2020 mean for that calendar month
#   spi3        Standardised Precipitation Index, 3 months: gamma distribution fitted to the unit's
#               1991-2020 3-month totals ending in the same calendar month (McKee et al. 1993;
#               method-of-moments fit). <= -1 moderately, <= -1.5 severely, <= -2 extremely dry.
#   tmax_c      mean daily maximum temperature, ERA5-Land at the district point (sub-counties
#               and DLGs take their district's value)
#   heat_days   days above the district's 2011-2020 90th percentile of daily maximum temperature
#   pm25, pm25_days  CAMS PM2.5 monthly mean and days above the WHO 24-hour guideline (15 ug/m3)
# Outlook: ECMWF SEAS5 ensemble per district; p_dry = share of members below the lower tercile
# of 1991-2020 CHIRPS rainfall for that month (model rainfall is not bias-corrected: indicative).
# Writes app/data/climate.rds and app/data/outlook.rds.

source("R/00_config.R")
suppressPackageStartupMessages({ library(terra); library(sf) })
ou <- fread("data/meta/orgunits.csv", na.strings = "")

# ---- CHIRPS zonal means --------------------------------------------------------------------
chirps_file <- "data/climate/chirps_busoga_monthly.nc"
r <- rast(chirps_file)
# Months since 1960 for each layer. Older terra versions put them in the layer names ("...T=252.5");
# newer ones name layers "precipitation_1" etc., so read the NetCDF time axis instead.
chirps_months <- function(r, file) {
  nm <- names(r)
  if (all(grepl("T=", nm))) return(as.numeric(sub(".*T=", "", nm)))
  d <- trimws(terra::describe(file, options = ""))
  stopifnot("CHIRPS time axis is not in months since 1960" = any(grepl("^T#units=months since 1960", d)))
  v <- grep("^NETCDF_DIM_T_VALUES=", d, value = TRUE)[1]
  as.numeric(strsplit(gsub("[{} ]", "", sub("^NETCDF_DIM_T_VALUES=", "", v)), ",")[[1]])
}
t_idx <- as.integer(floor(chirps_months(r, chirps_file)))
stopifnot("CHIRPS layers and time axis differ" = length(t_idx) == nlyr(r), !anyNA(t_idx))
periods <- (1960L + t_idx %/% 12L) * 100L + (t_idx %% 12L + 1L)
zonal <- function(lvl) {
  g <- st_read(sprintf("app/data/geo/%s.geojson", lvl), quiet = TRUE)
  v <- terra::extract(r, vect(g), fun = mean, na.rm = TRUE, weights = TRUE, exact = FALSE, ID = FALSE)
  m <- as.matrix(v)
  data.table(level = lvl, uid = rep(g$uid, times = ncol(m)), period = rep(periods, each = nrow(m)),
             rain_mm = as.vector(m))
}
rain <- rbindlist(lapply(c("subcounty", "dlg", "district", "region"), zonal))
rain <- rain[!is.na(rain_mm)]
setorder(rain, level, uid, period)
rain[, `:=`(year = period %/% 100L, month = period %% 100L)]
rain[, rain3 := frollsum(rain_mm, 3), by = .(level, uid)]
log_msg("CHIRPS: %d units, %d to %d", uniqueN(rain$uid), min(rain$period), max(rain$period))

# 1991-2020 normals per unit and calendar month
nrm <- rain[year %between% c(1991, 2020), .(rain_mean = mean(rain_mm), rain_t1 = quantile(rain_mm, 1/3, names = FALSE),
                                            r3_mean = mean(rain3, na.rm = TRUE), r3_var = var(rain3, na.rm = TRUE),
                                            r3_q0 = mean(rain3 <= 0, na.rm = TRUE)), by = .(level, uid, month)]
rain <- merge(rain, nrm, by = c("level", "uid", "month"))
rain[, rain_anom := 100 * (rain_mm - rain_mean) / pmax(rain_mean, 1)]
rain[, `:=`(shape = r3_mean^2 / r3_var, scale = r3_var / r3_mean)]
rain[, spi3 := qnorm(pmin(pmax(r3_q0 + (1 - r3_q0) * pgamma(pmax(rain3, 0), shape = shape, scale = scale), 1e-6), 1 - 1e-6))]
rain[, spi_class := fcase(spi3 <= -2, "Extremely dry", spi3 <= -1.5, "Severely dry", spi3 <= -1, "Moderately dry",
                          spi3 >= 1.5, "Very wet", spi3 >= 1, "Moderately wet", !is.na(spi3), "Near normal",
                          default = NA_character_)]
cl <- rain[year >= 2020, .(level, uid, period, rain_mm, rain_anom, spi3, spi_class)]

# ---- temperature and heat days (district points) -----------------------------------------
if (file.exists("data/climate/era5land_tmax_district.csv")) {
  tx <- fread("data/climate/era5land_tmax_district.csv")
  tx[, `:=`(period = as.integer(format(as.IDate(date), "%Y%m")), month = as.integer(format(as.IDate(date), "%m")))]
  p90 <- tx[period < 202101L, .(p90 = quantile(tmax_c, .9, names = FALSE)), by = .(uid, month)]
  tx <- merge(tx, p90, by = c("uid", "month"))
  tm <- tx[period >= 202001L, .(tmax_c = mean(tmax_c), heat_days = sum(tmax_c > p90), n = .N), by = .(uid, period)][n >= 25]
  # district value to its DLGs and sub-counties; Busoga = mean of districts
  d_of <- rbind(ou[level == 3, .(uid, d = uid)], ou[level == 4, .(uid, d = uid_l3)], ou[level == 5, .(uid, d = uid_l3)])
  tm_all <- rbind(merge(d_of, tm, by.x = "d", by.y = "uid", allow.cartesian = TRUE)[, .(uid, period, tmax_c, heat_days)],
                  tm[, .(uid = cfg$region_uid, tmax_c = mean(tmax_c), heat_days = mean(heat_days)), by = period])
  cl <- merge(cl, tm_all, by = c("uid", "period"), all.x = TRUE)
} else cl[, `:=`(tmax_c = NA_real_, heat_days = NA_real_)]

# ---- PM2.5 (districts; Busoga = mean of districts) ------------------------------------------
if (file.exists("data/climate/cams_pm25_district.csv")) {
  pm <- fread("data/climate/cams_pm25_district.csv")
  pm[, period := as.integer(format(as.IDate(date), "%Y%m"))]
  pm <- pm[, .(pm25 = mean(pm25_daily), pm25_days = sum(pm25_daily > 15), n = .N), by = .(uid, period)][n >= 20]
  pm <- rbind(pm[, .(uid, period, pm25, pm25_days)],
              pm[, .(uid = cfg$region_uid, pm25 = mean(pm25), pm25_days = mean(pm25_days)), by = period])
  cl <- merge(cl, pm, by = c("uid", "period"), all.x = TRUE)
} else cl[, `:=`(pm25 = NA_real_, pm25_days = NA_real_)]

saveRDS(cl[, .(level, uid, period, rain_mm, rain_anom, spi3, spi_class, tmax_c, heat_days, pm25, pm25_days)],
        "app/data/climate.rds", compress = "xz")
log_msg("climate: %s unit-months, %d to %d", format(nrow(cl), big.mark = ","), min(cl$period), max(cl$period))

# ---- seasonal outlook -----------------------------------------------------------------------
if (file.exists("data/climate/seas5_outlook_district.csv")) {
  so <- fread("data/climate/seas5_outlook_district.csv")
  so[, `:=`(period = month, cal = month %% 100L)]
  nd <- nrm[level == "district", .(uid, cal = month, rain_mean, rain_t1)]
  so <- merge(so, nd, by = c("uid", "cal"))
  ol <- so[, .(rain_mm = mean(rain_mm), rain_lo = quantile(rain_mm, .1, names = FALSE), rain_hi = quantile(rain_mm, .9, names = FALSE),
               p_dry = mean(rain_mm < rain_t1), tmax_c = mean(tmax_c, na.rm = TRUE), rain_normal = rain_mean[1],
               members = uniqueN(member)), by = .(uid, period)]
  ol <- rbind(ol[, .(level = "district", uid, period, rain_mm, rain_lo, rain_hi, p_dry, tmax_c, rain_normal, members)],
              ol[, .(level = "region", uid = cfg$region_uid, rain_mm = mean(rain_mm), rain_lo = mean(rain_lo), rain_hi = mean(rain_hi),
                     p_dry = mean(p_dry), tmax_c = mean(tmax_c), rain_normal = mean(rain_normal), members = max(members)), by = period])
  ol[, rain_anom := 100 * (rain_mm - rain_normal) / pmax(rain_normal, 1)]
  saveRDS(ol, "app/data/outlook.rds", compress = "xz")
  log_msg("outlook: %d months, %d members", uniqueN(ol$period), max(ol$members))
}
