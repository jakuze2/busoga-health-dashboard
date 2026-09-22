# 03_extract.R
# Pull every routine data element used by the indicators, with its full category
# option combo (age/sex etc.) breakdown, for every Busoga facility and month, plus
# the sub-county projected population by year, plus dataset reporting and
# timeliness. One file per chunk under data/raw/, so an interrupted run resumes.
#
#   Rscript R/03_extract.R            # resume, skip chunks already on disk
#   Rscript R/03_extract.R --refresh  # re-pull the most recent 12 months (late reports)

source("R/00_config.R")
args    <- commandArgs(trailingOnly = TRUE)
refresh <- "--refresh" %in% args

ops <- fread("data/meta/operands.csv", na.strings = "")
routine_des <- ops[operand_kind == "routine" & !is.na(de_name), unique(de)]
pop_des     <- ops[operand_kind == "population", unique(de)]
dir.create("data/raw/de", recursive = TRUE, showWarnings = FALSE)
dir.create("data/raw/reporting", recursive = TRUE, showWarnings = FALSE)

months <- months_between()
years  <- unique(substr(months, 1, 4))
recent <- tail(months, 12)
ou_fac <- paste0("ou:", cfg$region_uid, ";LEVEL-", cfg$levels[["facility"]])

pull <- function(file, fn) {
  if (file.exists(file) && !refresh) return(invisible(FALSE))
  t0 <- Sys.time()
  dt <- fn()
  saveRDS(dt, file, compress = "xz")
  log_msg("%s: %s rows in %.0fs", basename(file), format(nrow(dt), big.mark = ","),
          as.numeric(Sys.time() - t0, units = "secs"))
  invisible(TRUE)
}

# ---- routine data elements, facility x month x COC ---------------------------------
de_chunks <- chunk(sort(routine_des), 12)
wrote_des <- list()                                    # data elements actually downloaded this run, by year
for (y in years) {
  pe <- months[substr(months, 1, 4) == y]
  if (refresh && !any(pe %in% recent)) next
  for (k in seq_along(de_chunks)) {
    f <- sprintf("data/raw/de/de_%s_%02d.rds", y, k)
    if (!file.exists(f) || refresh) wrote_des[[y]] <- c(wrote_des[[y]], de_chunks[[k]])
    pull(f, function() {
      dt <- d2_analytics(c(paste0("dx:", paste(de_chunks[[k]], collapse = ";")),
                           paste0("pe:", paste(pe, collapse = ";")), ou_fac, "co"))
      if (nrow(dt)) setnames(dt, c("dx", "co", "pe", "ou"), c("de", "coc", "period", "uid"))
      dt
    })
  }
}

# ---- data elements added to the catalogue later (e.g. revised-form successors) -------------
# A per-year manifest records which data elements have been requested, so a new element is
# pulled once for every year without re-pulling everything.
for (y in years) {
  mf <- sprintf("data/raw/de/requested_%s.txt", y)
  req_des <- if (file.exists(mf)) readLines(mf) else
    unique(unlist(lapply(list.files("data/raw/de", pattern = sprintf("^de_%s_", y), full.names = TRUE),
                         function(f) unique(readRDS(f)$de))))
  req_des <- union(req_des, wrote_des[[y]])
  extra <- setdiff(routine_des, req_des)
  if (length(extra)) {
    pe <- months[substr(months, 1, 4) == y]
    f <- sprintf("data/raw/de/de_%s_x%s.rds", y, format(Sys.time(), "%Y%m%d%H%M%S"))
    dt <- d2_analytics(c(paste0("dx:", paste(extra, collapse = ";")), paste0("pe:", paste(pe, collapse = ";")), ou_fac, "co"))
    if (nrow(dt)) setnames(dt, c("dx", "co", "pe", "ou"), c("de", "coc", "period", "uid"))
    saveRDS(dt, f, compress = "xz")
    log_msg("%s: %d added data elements, %s rows", basename(f), length(extra), format(nrow(dt), big.mark = ","))
  }
  writeLines(sort(union(req_des, extra)), mf)
}

# ---- projected population, every area level x year (DHIS2 aggregates from wherever it was entered:
#      sub-county up to 2024, district level for later years) ----------------------------------
pull("data/raw/population.rds", function() {
  dt <- d2_analytics(c(paste0("dx:", paste(pop_des, collapse = ";")),
                       paste0("pe:", paste(years, collapse = ";")),
                       paste0("ou:", cfg$region_uid, ";LEVEL-2;LEVEL-3;LEVEL-4;LEVEL-5")))
  setnames(dt, c("dx", "pe", "ou"), c("de", "year", "uid"))
  dt
})

# ---- reporting completeness and timeliness, facility x month x dataset ---------------
metrics <- c("REPORTING_RATE", "REPORTING_RATE_ON_TIME", "ACTUAL_REPORTS",
             "ACTUAL_REPORTS_ON_TIME", "EXPECTED_REPORTS")
for (y in years) {
  pe <- months[substr(months, 1, 4) == y]
  if (refresh && !any(pe %in% recent)) next
  pull(sprintf("data/raw/reporting/rep_%s.rds", y), function() {
    dx <- as.vector(outer(names(cfg$datasets), metrics, paste, sep = "."))
    dt <- d2_analytics(c(paste0("dx:", paste(dx, collapse = ";")),
                         paste0("pe:", paste(pe, collapse = ";")), ou_fac))
    dt[, c("dataset", "metric") := tstrsplit(dx, ".", fixed = TRUE)]
    dt[, .(dataset, metric, period = pe, uid = ou, value)]
  })
}

# ---- category option combo names (for age/sex breakdowns) --------------------------
if (!file.exists("data/meta/coc.csv") || refresh) {
  files <- list.files("data/raw/de", pattern = "rds$", full.names = TRUE)
  cocs <- unique(unlist(lapply(files, function(f) unique(readRDS(f)$coc))))
  coc <- rbindlist(lapply(chunk(cocs, 150), function(ch) {
    x <- d2_get("categoryOptionCombos", filter = paste0("id:in:[", paste(ch, collapse = ","), "]"),
                fields = "id,name", paging = "false")$categoryOptionCombos
    data.table(coc = x$id, coc_name = x$name)
  }))
  fwrite(coc, "data/meta/coc.csv")
  log_msg("category option combos: %d", nrow(coc))
}
log_msg("extract complete")
