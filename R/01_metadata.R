# 01_metadata.R
# Org unit hierarchy (region > district > DLG > sub-county > facility), facility
# groups (ownership, level, authority), facility coordinates and boundary polygons.
# Writes data/meta/orgunits.csv, data/meta/facility_groups.csv and app/data/geo/*.

source("R/00_config.R")
suppressPackageStartupMessages(library(sf))

raw <- d2_req("organisationUnits", list(
  filter = paste0("path:like:", cfg$region_uid),
  fields = "id,name,shortName,level,path,openingDate,closedDate,geometry",
  paging = "false")) |> req_perform() |> resp_body_json(simplifyVector = FALSE)
raw <- raw$organisationUnits
g_or_na <- function(x, k) if (is.null(x[[k]])) NA_character_ else as.character(x[[k]])
ou <- rbindlist(lapply(raw, function(x) data.table(
  id = x$id, name = x$name, shortName = g_or_na(x, "shortName"), level = x$level,
  path = x$path, openingDate = g_or_na(x, "openingDate"), closedDate = g_or_na(x, "closedDate"),
  gtype = if (is.null(x$geometry)) NA_character_ else x$geometry$type)))
geoms <- setNames(lapply(raw, `[[`, "geometry"), ou$id)
keep <- ou$level >= 2
ou <- ou[keep]
log_msg("org units in Busoga: %d", nrow(ou))

# parents from the path: /L1/L2/L3/L4/L5/L6
parts <- tstrsplit(sub("^/", "", ou$path), "/", fill = NA)
for (k in 2:6) set(ou, j = paste0("uid_l", k), value = parts[[k]])
nm <- setNames(ou$name, ou$id)
ou[, `:=`(district  = nm[uid_l3], dlg = nm[uid_l4],
          subcounty = nm[uid_l5], facility = fifelse(level == 6, name, NA_character_))]
ou[, district := sub(" District$", "", district)]
ou[, level_name := names(cfg$levels)[match(level, cfg$levels)]]

# facility coordinates (Point geometries only)
ou[, `:=`(lon = NA_real_, lat = NA_real_)]
pt_idx <- which(ou$gtype %in% "Point" & ou$level == 6)
xy <- t(vapply(pt_idx, function(i) as.numeric(unlist(geoms[[ou$id[i]]]$coordinates))[1:2], numeric(2)))
if (length(pt_idx)) ou[pt_idx, `:=`(lon = xy[, 1], lat = xy[, 2])]
# discard points outside a generous Busoga bounding box (data entry errors)
ou[!(lon %between% c(32.5, 34.3) & lat %between% c(-0.6, 1.9)), `:=`(lon = NA, lat = NA)]

# ---- boundary polygons -------------------------------------------------------
dir.create("app/data/geo", recursive = TRUE, showWarnings = FALSE)
poly_sf <- function(lvl) {
  i <- which(ou$level == lvl & ou$gtype %in% c("Polygon", "MultiPolygon"))
  gj <- sprintf('{"type":"FeatureCollection","features":[%s]}', paste(vapply(i, function(k)
    sprintf('{"type":"Feature","properties":{"uid":"%s"},"geometry":%s}', ou$id[k],
            toJSON(geoms[[ou$id[k]]], auto_unbox = TRUE, digits = NA)), ""), collapse = ","))
  s <- st_read(gj, quiet = TRUE)
  s <- st_make_valid(s)
  s
}
simplify <- function(s, tol) st_simplify(st_transform(s, 32636), dTolerance = tol, preserveTopology = TRUE) |>
  st_transform(4326)

g_dist <- poly_sf(3); g_sub <- poly_sf(5)
g_dist$name <- sub(" District$", "", nm[g_dist$uid])
g_sub <- merge(g_sub, ou[, .(uid = id, name, dlg_uid = uid_l4, district)], by = "uid")
# DLG boundaries: dissolve sub-counties by their DLG parent
# (in metres, with a 1 m buffer so slivers between source polygons do not break GEOS)
dissolve <- function(s, by) {
  s <- st_buffer(st_make_valid(st_transform(s, 32636)), 1)
  u <- lapply(split(st_geometry(s), by), st_union)
  st_transform(st_sf(uid = names(u), geometry = do.call(c, u)), 4326)
}
g_dlg <- dissolve(g_sub, g_sub$dlg_uid); g_dlg$name <- nm[g_dlg$uid]
g_reg <- dissolve(g_dist, rep(cfg$region_uid, nrow(g_dist))); g_reg$name <- "Busoga"

st_write(simplify(g_dist[, c("uid", "name")], 150), "app/data/geo/district.geojson", delete_dsn = TRUE, quiet = TRUE)
st_write(simplify(g_dlg[, c("uid", "name")], 150),  "app/data/geo/dlg.geojson",      delete_dsn = TRUE, quiet = TRUE)
st_write(simplify(g_sub[, c("uid", "name")], 80),   "app/data/geo/subcounty.geojson", delete_dsn = TRUE, quiet = TRUE)
st_write(simplify(g_reg, 200),                      "app/data/geo/region.geojson",   delete_dsn = TRUE, quiet = TRUE)
log_msg("polygons: %d districts, %d DLGs, %d sub-counties", nrow(g_dist), nrow(g_dlg), nrow(g_sub))

# ---- facility groups ---------------------------------------------------------
gs <- c(ownership = "qPIRLHZ6dTm", level = "E2fcwOxOuR4", authority = "au4gnriblmx")
fac_ids <- ou[level == 6, id]
grp <- rbindlist(lapply(names(gs), function(g) {
  x <- d2_get(paste0("organisationUnitGroupSets/", gs[[g]]),
              fields = "organisationUnitGroups[name,organisationUnits[id]]")$organisationUnitGroups
  rbindlist(lapply(seq_len(nrow(x)), function(i) {
    ids <- x$organisationUnits[[i]]$id
    ids <- intersect(ids, fac_ids)
    if (length(ids)) data.table(uid = ids, set = g, group = x$name[i])
  }))
}))
grp <- unique(grp, by = c("uid", "set"))
fwrite(grp, "data/meta/facility_groups.csv")

ou_out <- ou[, .(uid = id, name, short_name = shortName, level, level_name,
                 uid_l3, uid_l4, uid_l5, district, dlg, subcounty,
                 opening_date = substr(openingDate, 1, 10),
                 closed_date = substr(closedDate, 1, 10),
                 lon, lat)]
fwrite(ou_out, "data/meta/orgunits.csv")
log_msg("facilities: %d (with coordinates: %d); group rows: %d",
        ou_out[level == 6, .N], ou_out[level == 6 & !is.na(lat), .N], nrow(grp))
