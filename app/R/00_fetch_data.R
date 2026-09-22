# Fetches the dashboard data before anything else loads. Shiny sources R/ in alphabetical
# order, so this runs before 00_utils.R reads data/.
#
# The data files are kept in a private GitHub repository (BHF_DATA_REPO, default
# jakuze2/busoga-health-data, folder app/data/). On Posit Connect Cloud the app starts without
# data/ and downloads it here, using a read-only token in the BHF_DATA_TOKEN variable
# (Connect Cloud: content Settings > Variables). When the app runs locally with an existing
# data/ folder, nothing is downloaded.

local({
  if (file.exists(file.path("data", "meta.json"))) return(invisible())

  repo  <- Sys.getenv("BHF_DATA_REPO", "jakuze2/busoga-health-data")
  ref   <- Sys.getenv("BHF_DATA_REF", "main")
  token <- Sys.getenv("BHF_DATA_TOKEN")
  base  <- Sys.getenv("BHF_DATA_API", "https://api.github.com")
  if (!nzchar(token))
    stop("No data/ folder and no BHF_DATA_TOKEN: set BHF_DATA_TOKEN to a read-only token for ", repo)

  api <- function(path, accept, dest = NULL) {
    h <- curl::new_handle()
    curl::handle_setheaders(h, Authorization = paste("Bearer", token), Accept = accept,
                            `X-GitHub-Api-Version` = "2022-11-28", `User-Agent` = "bhf-dashboard")
    url <- paste0(base, "/repos/", repo, "/", path)
    for (i in 1:4) {
      r <- tryCatch(if (is.null(dest)) curl::curl_fetch_memory(url, handle = h)
                    else curl::curl_fetch_disk(url, dest, handle = h),
                    error = function(e) list(status_code = 0L, msg = conditionMessage(e)))
      if (r$status_code == 200L) return(r)
      if (r$status_code %in% c(401L, 403L, 404L) && i == 1L && is.null(dest))
        stop("GitHub returned ", r$status_code, " for ", repo,
             ": check that BHF_DATA_TOKEN can read this repository")
      Sys.sleep(2 * i)
    }
    stop("Could not download ", path, " from ", repo)
  }

  tree <- jsonlite::fromJSON(rawToChar(
    api(paste0("git/trees/", ref, "?recursive=1"), "application/vnd.github+json")$content))
  files <- tree$tree$path[tree$tree$type == "blob" & startsWith(tree$tree$path, "app/data/")]
  if (!length(files)) stop("No files under app/data/ in ", repo)

  tmp <- tempfile("bhfdata")
  for (f in files) {
    dest <- file.path(tmp, sub("^app/data/", "", f))
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    api(paste0("contents/", utils::URLencode(f), "?ref=", ref), "application/vnd.github.raw", dest)
  }
  unlink("data", recursive = TRUE)
  if (!file.rename(tmp, "data")) {
    dir.create("data", showWarnings = FALSE)
    file.copy(list.files(tmp, full.names = TRUE), "data", recursive = TRUE)
  }
  message(sprintf("Downloaded %d data files from %s@%s", length(files), repo, ref))
})
