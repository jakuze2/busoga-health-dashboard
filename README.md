# Busoga Health Forum: Dashboard

An interactive R Shiny dashboard of routine health data (Uganda national DHIS2) for every
health facility in Busoga's 12 districts and cities, with climate, epidemic early warning and
forecasting. Built for Busoga Health Forum (busogahealthforum.org).

## What is in it

| Page | What it answers |
|---|---|
| Overview | Busoga (or any area) at a glance: one colour-coded block per programme theme |
| Scorecard | Every district, DLG, sub-county or facility x every indicator, rated against Busoga |
| Maps | Choropleths from the official DHIS2 district and sub-county boundaries, with rankings |
| Atlas | All 613 DHIS2 facilities (reporting status, level) with roads, water, schools by level, markets, towns (OpenStreetMap) |
| Explore: Compare & trends | Up to 8 units over time; league tables; a whole programme for one unit |
| Explore: Cascade drill-down | Busoga > district > DLG > sub-county > facility, click to go down |
| Explore: Deep dive | Full profile of any area or facility |
| Explore: Breakdowns | By ownership, facility level, authority; age and sex |
| Early warning: Epidemic alerts | Weekly 033B surveillance, WHO normal channel, notifiable diseases, 4-week predictions, facility to Busoga |
| Early warning: Climate & environment | Rainfall, drought index (SPI-3), heat days, PM2.5, ECMWF seasonal outlook, climate x malaria |
| Early warning: Forecast | Any indicator, any area or facility, 3 to 12 months ahead with prediction intervals and a back-test |
| Data quality | Completeness, timeliness, non-reporting facilities, consistency checks, outliers, where to improve |
| Download district data | District and Busoga aggregates only: no facility is identifiable in the file |
| Indicators & methods | The indicator dictionary and how every number is made |

81 indicators in 8 themes (antenatal care; delivery & newborn; postnatal & family planning;
immunisation; child health & nutrition; malaria; HIV & PMTCT; services & mortality). Official
Ministry of Health DHIS2 indicators are used as defined in DHIS2; a few BHF-defined totals
are built only from named DHIS2 data elements and are marked as such.

## Folder layout

```
Busoga Health Forum/
  run_refresh.R              one command for every refresh (--full, --monthly, --weekly)
  R/                         the pipeline (R)
    00_config.R              settings, DHIS2 helpers (credentials from .Renviron)
    01_metadata.R            org unit hierarchy, boundaries, facility groups
    02_indicators.R          resolves the indicator catalogue against live DHIS2 definitions
    03_extract.R             monthly data elements x facility x month x category combo
    03b_extract_weekly.R     weekly 033B surveillance x facility x week
    04_compute.R             DHIS2-consistent indicator values at every level
    04b_climate.R            climate indicators per sub-county / district
    05_quality.R             data quality and age/sex breakdowns
    05b_epidemic.R           normal channels, alerts, 4-week predictions
    06_validate.R            compares our values with DHIS2's own indicator results
    07_app_data.R            final app files
  scripts/                   Python helpers (OpenStreetMap layers, Open-Meteo climate)
  data/                      pipeline data (not in this repository, see "Where the data live")
  app/                       the Shiny app (this folder is what Connect Cloud runs)
    app.R, R/, www/, manifest.json
    data/                    app data (not in this repository, see "Where the data live")
  .github/workflows/refresh.yml   automatic weekly and monthly refresh
```

## Where the data live

This repository holds code only. The data (`data/` and `app/data/`) are kept in the private
repository `jakuze2/busoga-health-data`, with the same folder layout.

- On Posit Connect Cloud the app starts without `app/data/` and downloads it from the private
  repository (`app/R/00_fetch_data.R`), using a read-only token stored in the content's
  **Settings > Variables** as `BHF_DATA_TOKEN`.
- Locally, keep `data/` and `app/data/` in this folder as before. They are listed in
  `.gitignore`, so they are never pushed here. If `app/data/` is missing, the app downloads it
  when `BHF_DATA_TOKEN` is set in `.Renviron`.
- The automatic refresh reads and writes the private repository (see below).

## Run it locally (Positron / RStudio)

```r
# R 4.5 (the app was built and tested on R 4.5.1)
shiny::runApp("app")
```

Refresh the data by hand on your computer (needs `HMIS_USER` and `HMIS_PWD` in `.Renviron`, never committed):

```
Rscript run_refresh.R --monthly    # after the 15th of each month
Rscript run_refresh.R --weekly     # any Monday
Rscript run_refresh.R --full       # rebuild everything from scratch (about 1.5 hours)
```

## Publish on Posit Connect Cloud (same URL every time)

1. Push this folder to a GitHub repository.
2. At connect.posit.cloud, sign in with GitHub, choose **Publish > Shiny**, pick the repository,
   and set the primary file to `app/app.R`. Connect Cloud reads `app/manifest.json` for the
   R version and packages.
3. Turn on **automatically republish on push**. Every refresh updates the same link.
4. Under the content's **Settings > Variables**, add `BHF_DATA_TOKEN` (a fine-grained token
   with Contents: read-only on `busoga-health-data`).

If packages in the app change, regenerate the manifest from R 4.5.1:
`rsconnect::writeManifest("app", appPrimaryDoc = "app.R")`.

## Automatic refresh (weekly and monthly)

`.github/workflows/refresh.yml` runs the pipeline on GitHub's servers:
weekly on Mondays (epidemic surveillance, seasonal outlook) and monthly on the 16th, the day
after the national reporting deadline (all monthly indicators, climate, validation). It pushes
the refreshed data to the private data repository, then updates `app/data_version.txt` here so
that Connect Cloud republishes and the app loads the new data.

It needs three repository secrets (GitHub > Settings > Secrets and variables > Actions):
`HMIS_USER` and `HMIS_PWD` (DHIS2 login) and `DATA_REPO_TOKEN` (a fine-grained token with
Contents: read and write on `busoga-health-data`). The credentials never appear in the code or
the logs. You can also run it by hand from the Actions tab (**Run workflow**, mode `weekly`,
`monthly` or `full`).

## Data sources and licences

- Health data: Uganda Ministry of Health, national DHIS2 (hmis.health.go.ug), analytics API.
- Boundaries and facility coordinates: DHIS2.
- Roads, water, schools, markets, towns, OSM health sites: (c) OpenStreetMap contributors, ODbL.
- Climate: Copernicus ERA5-Land and CAMS (contains modified Copernicus information), ECMWF SEAS5
  seasonal forecasts; served by Open-Meteo (CC BY 4.0).
- Logo: Busoga Health Forum.

## Method notes

- Indicator values follow DHIS2: numerators and denominators are summed from facilities upward,
  then divided (pooled, never averaged). A missing operand counts as zero unless all are missing.
- Population-based coverage uses the 107a projected population (sub-county, yearly) and is
  annualised; it is shown for areas, not for single facilities.
- Scorecard colours compare each unit with the Busoga value for the same period and indicator
  direction. No national targets are assumed.
- Epidemic thresholds follow the WHO normal-channel method (3rd quartile alert, mean + 2 SD
  epidemic) from the same weeks of up to five previous years; immediately notifiable diseases
  alert on any case (IDSR).
- Forecasts extend seasonality and recent trends; they cannot foresee stock-outs, campaigns,
  outbreaks or reporting changes. The back-test error is shown next to every forecast.
- Parish level is not available: DHIS2 has no parish level for Busoga and no open parish
  boundary map exists. With a UBOS parish shapefile, facilities can be assigned by GPS.
