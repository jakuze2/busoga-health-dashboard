# Busoga Health Forum - Dashboard
# R Shiny (bslib). Data: app/data/, built by the pipeline in ../R/. Deployed on Posit Connect Cloud.

bhf_theme <- bs_theme(
  version = 5, bg = "#ffffff", fg = "#0b0b0b", primary = BRAND$navy, secondary = BRAND$maroon,
  success = "#1b7a3a", warning = "#C17D11", danger = "#b3261e", info = "#3949AB",
  base_font = font_collection("Arial", "Helvetica", "sans-serif"),
  heading_font = font_collection("Arial", "Helvetica", "sans-serif"), font_scale = 0.95,
  "border-radius" = "10px", "navbar-bg" = "#ffffff"
)

nav_icon <- function(name) fontawesome::fa(name, height = "0.95em")

ui <- page_navbar(
  title = tags$span(class = "brand-lockup",
                    tags$span(class = "brand-stack", tags$img(src = "bhf_logo.png", alt = "Busoga Health Forum"),
                              tags$span(class = "brand-word", "Dashboard")),
                    tags$span(class = "brand-sub d-none d-xxl-block", "Routine health data", tags$br(), "for Busoga")),
  id = "nav", window_title = "Busoga Health Forum · Dashboard",
  theme = bhf_theme, fillable = FALSE,
  navbar_options = navbar_options(position = "static-top", bg = "#ffffff", theme = "light"),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css"),
                     tags$script(src = "html-to-image.min.js"), tags$script(src = "bhf-download.js"),
                     tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
                     tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),
  footer = tags$footer(class = "site-footer",
    div(class = "site-footer-inner",
        div(class = "brand-stack footer-brand", tags$img(src = "bhf_logo.png", alt = "Busoga Health Forum"), tags$span(class = "brand-word", "Dashboard")),
        div(class = "site-footer-text",
            div("Routine health data are the property of the Uganda Ministry of Health (DHIS2, hmis.health.go.ug); open datasets remain the property of their publishers (UBOS, WorldPop, CHIRPS, Copernicus/ECMWF, OpenStreetMap contributors). The Busoga Health Forum compiles and presents them."),
            div(class = "site-footer-contact", fontawesome::fa("envelope", fill = BRAND$navy, height = ".9em"), " Queries: ", CONTACT_NAME, " (",
                tags$a(href = paste0("mailto:", CONTACT_EMAIL), CONTACT_EMAIL), ")")))),
  nav_panel(tagList(nav_icon("house"), "Overview"), value = "overview", overview_ui("overview")),
  nav_panel(tagList(nav_icon("hospital"), "Facilities"), value = "facilities", facilities_ui("facilities")),
  nav_menu(tagList(nav_icon("chart-column"), "Analyse"),
    nav_panel(tagList(nav_icon("magnifying-glass-chart"), "Indicator explorer"), value = "explorer", explorer_ui("explorer")),
    nav_panel(tagList(nav_icon("bullseye"), "Performance against targets"), value = "targets", targets_ui("targets")),
    nav_panel(tagList(nav_icon("table-cells"), "Scorecard"), value = "scorecard", scorecard_ui("scorecard")),
    nav_panel(tagList(nav_icon("chart-line"), "Compare & trends"), value = "compare", compare_ui("compare")),
    nav_panel(tagList(nav_icon("sitemap"), "Cascade drill-down"), value = "drill", drill_ui("drill")),
    nav_panel(tagList(nav_icon("id-card"), "Area & facility profile"), value = "deep", deepdive_ui("deep")),
    nav_panel(tagList(nav_icon("layer-group"), "Breakdowns"), value = "breakdown", breakdown_ui("breakdown"))),
  nav_menu(tagList(nav_icon("map"), "Maps"),
    nav_panel(tagList(nav_icon("map-location-dot"), "Indicator maps"), value = "maps", maps_ui("maps")),
    nav_panel(tagList(nav_icon("earth-africa"), "Atlas of Busoga"), value = "atlas", atlas_ui("atlas"))),
  nav_menu(tagList(nav_icon("tower-broadcast"), "Early warning"),
    nav_panel(tagList(nav_icon("virus"), "Epidemic alerts"), value = "epidemic", epidemic_ui("epidemic")),
    nav_panel(tagList(nav_icon("cloud-sun-rain"), "Climate & environment"), value = "climate", climate_ui("climate")),
    nav_panel(tagList(nav_icon("chart-area"), "Forecast"), value = "forecast", forecast_ui("forecast"))),
  nav_panel(tagList(nav_icon("wand-magic-sparkles"), "AI insights"), value = "xai", xai_ui("xai")),
  nav_menu(tagList(nav_icon("map-location-dot"), "Busoga profile"),
    nav_panel(tagList(nav_icon("people-group"), "People: population, age and schools"), value = "population", population_ui("population")),
    nav_panel(tagList(nav_icon("chart-pie"), "Place: census history, access, farming and trade"), value = "region", region_ui("region"))),
  nav_panel(tagList(nav_icon("clipboard-check"), "Data quality"), value = "dq", dq_ui("dq")),
  nav_panel(tagList(nav_icon("file-lines"), "Brief"), value = "brief", brief_ui("brief")),
  nav_panel(tagList(nav_icon("circle-info"), "About"), value = "info", info_ui("info")),
  nav_menu(tagList(nav_icon("book"), "Reference"), align = "right",
    nav_panel(tagList(nav_icon("book"), "Indicator definitions"), value = "definitions", definitions_ui("definitions")),
    nav_panel(tagList(nav_icon("book-open"), "Methods"), value = "about", about_ui("about")),
    nav_panel(tagList(nav_icon("download"), "Download district data"), value = "download", download_ui("download")),
    nav_item(tags$a(href = "https://busogahealthforum.org/", target = "_blank", nav_icon("up-right-from-square"), "busogahealthforum.org")))
)

server <- function(input, output, session) {
  session$userData$deep_dive_uid <- reactiveVal(NULL)
  session$userData$facility_uid  <- reactiveVal(NULL)
  session$userData$explore       <- reactiveVal(NULL)
  overview_server("overview")
  facilities_server("facilities")
  explorer_server("explorer", incoming = session$userData$explore)
  targets_server("targets")
  brief_server("brief")
  scorecard_server("scorecard")
  compare_server("compare")
  drill_server("drill")
  deepdive_server("deep", jump = session$userData$deep_dive_uid)
  breakdown_server("breakdown")
  maps_server("maps")
  atlas_server("atlas")
  epidemic_server("epidemic")
  climate_server("climate")
  forecast_server("forecast")
  xai_server("xai")
  population_server("population")
  region_server("region")
  dq_server("dq")
  info_server("info")
  definitions_server("definitions")
  about_server("about")
  download_server("download")
}

shinyApp(ui, server)
