# 00_config.R
# Shared settings and DHIS2 helpers for the Busoga Health Forum pipeline.
# Sourced by every other script. Run scripts from the project root
# ("Busoga Health Forum/"), with HMIS_USER and HMIS_PWD in .Renviron.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(data.table)
})

readRenviron(".Renviron")

cfg <- list(
  base_url    = "https://hmis.health.go.ug/api",
  region_uid  = "Wd1lV9Qdj4o",               # Busoga (level 2)
  start_month = as.Date("2020-01-01"),       # earliest month with real data in this instance
  end_month   = seq(as.Date(format(Sys.Date(), "%Y-%m-01")), by = "-1 month",
                    length.out = 2)[2],      # last complete month
  levels      = c(region = 2, district = 3, dlg = 4, subcounty = 5, facility = 6),
  # monthly datasets used for completeness and timeliness
  datasets    = c(
    "RtEYsASU7PG" = "105:01 OPD",
    "ic1BSWhGOso" = "105:02-03 MCH, FP, EPI",
    "nGkMm2VBT4G" = "105:04-05 HTS & SMC",
    "VDhwrW9DiC1" = "105:06-09 Supplies & outreach",
    "quMWqLxzcfO" = "105:10 Lab",
    "EBqVAQRmiPm" = "108 IPD"
  )
)

months_between <- function(from = cfg$start_month, to = cfg$end_month)
  format(seq(from, to, by = "month"), "%Y%m")

# ---- HTTP ---------------------------------------------------------------------
d2_req <- function(path, query = list()) {
  user <- Sys.getenv("HMIS_USER"); pwd <- Sys.getenv("HMIS_PWD")
  stopifnot("HMIS_USER / HMIS_PWD missing from .Renviron" = nzchar(user) && nzchar(pwd))
  request(paste0(cfg$base_url, "/", path)) |>
    req_auth_basic(user, pwd) |>
    req_url_query(!!!query, .multi = "explode") |>
    req_headers(Accept = "application/json") |>
    # 409: DHIS2 is regenerating analytics tables; 429/5xx: busy. All are worth retrying.
    req_retry(max_tries = 6, backoff = ~ 20 * .x,
              is_transient = function(resp) resp_status(resp) %in% c(409, 429, 500, 502, 503, 504)) |>
    req_error(body = function(resp) tryCatch(resp_body_json(resp)$message, error = function(e) NULL)) |>
    req_timeout(900)
}

d2_get <- function(path, ...) {
  d2_req(path, list(...)) |> req_perform() |> resp_body_json(simplifyVector = TRUE)
}

# /analytics as a data.table. `dims` is a character vector of dimension strings.
d2_analytics <- function(dims, filters = NULL, extra = list()) {
  q <- c(list(dimension = dims), if (length(filters)) list(filter = filters),
         list(skipMeta = "true", outputIdScheme = "UID", ignoreLimit = "true"), extra)
  r <- d2_req("analytics", q) |> req_perform() |> resp_body_json(simplifyVector = TRUE)
  if (!length(r$rows)) return(data.table())
  dt <- as.data.table(r$rows)
  setnames(dt, r$headers$name)
  dt[, value := as.numeric(value)]
  dt[]
}

chunk <- function(x, n) split(x, ceiling(seq_along(x) / n))

log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), sprintf(...), "\n", sep = "")
