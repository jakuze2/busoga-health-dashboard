# run_refresh.R
# One entry point for every refresh. From this folder:
#   Rscript run_refresh.R --full      first run: metadata, definitions, everything from DHIS2
#   Rscript run_refresh.R --monthly   re-pull the last 12 months (late reports), climate, validate
#   Rscript run_refresh.R --weekly    re-pull the last 12 weeks of 033B, epidemic alerts, outlook
# Then push app/ to GitHub; Posit Connect Cloud republishes to the same URL.

args <- commandArgs(trailingOnly = TRUE)
mode <- if ("--full" %in% args) "full" else if ("--weekly" %in% args) "weekly" else "monthly"
run <- function(label, cmd, a = character()) {
  cat(sprintf("\n==== %s ====\n", label))
  st <- system2(cmd, a)
  if (st != 0) stop(label, " failed (exit ", st, ")")
}
R  <- file.path(R.home("bin"), "Rscript")
py <- Sys.getenv("PYTHON")                   # the GitHub workflow sets this
if (!nzchar(py)) { py <- Sys.which(c("python3", "python")); py <- unname(py[nzchar(py)][1]) }
cat("Python:", py, "\n")
# Under R, Python loses its own package folder (R's library path makes it load a different
# libpython). Run Python outside R's library path and point it at its packages explicitly.
py_site <- Sys.getenv("PYTHON_SITE")         # the GitHub workflow sets this
if (.Platform$OS.type == "unix" && nzchar(py)) {
  if (nzchar(py_site)) Sys.setenv(PYTHONPATH = py_site)
  py_args <- function(a) c("-u", "LD_LIBRARY_PATH", py, a)
  run_py <- function(label, a = character()) run(label, "env", py_args(a))
} else run_py <- function(label, a = character()) run(label, py, a)
refresh <- if (mode == "full") character() else "--refresh"

if (mode == "full") {
  run("Org units, boundaries, facility groups", R, "R/01_metadata.R")
  run("Indicator definitions", R, "R/02_indicators.R")
  run_py("OpenStreetMap context layers", "scripts/fetch_context_layers.py")
}
if (mode %in% c("full", "monthly")) {
  run("Monthly data from DHIS2", R, c("R/03_extract.R", refresh))
  run("Compute indicators", R, "R/04_compute.R")
  run("Data quality and breakdowns", R, "R/05_quality.R")
  run("Validate against DHIS2", R, "R/06_validate.R")
}
run("Weekly surveillance (033B)", R, c("R/03b_extract_weekly.R", refresh))
run("Epidemic alerts and predictions", R, "R/05b_epidemic.R")
if (!is.na(py)) {
  run_py("Climate, air quality and seasonal outlook", "scripts/fetch_climate.py")
  run("Climate indicators", R, "R/04b_climate.R")
}
if (mode == "full") {
  run("Population pyramids (WorldPop, scaled to the official totals)", R, "R/04c_population.R")
  run("Education and schools", R, "R/04d_education.R")
  if (!is.na(py)) run_py("OpenStreetMap commerce", c("scripts/fetch_context_layers.py", "commerce"))
}
run("App data", R, "R/07_app_data.R")
if (mode %in% c("full", "monthly")) run("Explainable AI (drivers of the monthly figures)", R, "R/09_explain.R")
if (mode == "full") run("Open data: census history, access, hazards, food prices, commerce", R, "R/08_open_data.R")
cat("\nDone. Check locally with shiny::runApp('app'), then push to GitHub.\n")
