# About: purpose, audience, commissioning, development and use of this resource.

info_ui <- function(id) {
  tagList(
    div(class = "about-hero",
        div(class = "about-logo", tags$img(src = "bhf_logo.png", alt = "Busoga Health Forum")),
        div(div(class = "eyebrow", "About this dashboard"),
            h2("An open health information resource for Busoga"),
            p("A shared view of routine health data for every health facility, sub-county, DLG and district in the Busoga region, with climate, epidemic early warning and forecasting."))),
    layout_columns(col_widths = c(7, 5),
      card(card_body(
        h5("Purpose"),
        p("This dashboard is an open resource for the stakeholders and partners of the Busoga Kingdom, for the Basoga, and for the Busoga Health Forum. ",
          "It puts routine data from the national health information system (DHIS2) in one place so that health workers, managers, leaders and partners can see how services are performing, where gaps are, and where to act."),
        h5("Who it is for"),
        tags$ul(
          tags$li(tags$b("Busoga Kingdom"), " and its stakeholders and partners."),
          tags$li(tags$b("The Basoga"), ", the people of Busoga."),
          tags$li(tags$b("Busoga Health Forum"), ", a platform of health workers living and working in the Busoga region and of Basoga."),
          tags$li("District health teams, health facility managers, implementing partners, researchers and anyone working to improve health in Busoga.")),
        h5("Commissioning"),
        p("The dashboard was commissioned by the Kyabazinga of Busoga, Gabula Nadiope IV."),
        h5("Development"),
        p("The dashboard was developed for the Busoga Health Forum through the Makerere University Centre of Excellence for Maternal and Newborn Health, ",
          "which is directed by ", tags$b("Professor Peter Waiswa"), " (", tags$a(href = "mailto:pwaiswa@musph.ac.ug", "pwaiswa@musph.ac.ug"), "). ",
          "The dashboard was developed by ", tags$b("Dr Akuze Joseph Waiswa"),
          ", a senior medical statistician (", tags$a(href = "mailto:jakuze@musph.ac.ug", "jakuze@musph.ac.ug"), "), ",
          "using routine HMIS (DHIS2) data together with open datasets: the UBOS population statistics and census base, ",
          "ERA5-Land and CAMS (Copernicus), CHIRPS rainfall, ECMWF seasonal forecasts, WorldPop and OpenStreetMap."),
        h5("Using this resource"),
        p("The dashboard and its district-level downloads are open for use. When you use figures from it, please acknowledge the source, for example:"),
        div(class = "cite-box",
            sprintf("Busoga Health Forum Dashboard. Commissioned by the Kyabazinga of Busoga; developed by Akuze J.W., Makerere University Centre of Excellence for Maternal and Newborn Health. Data: Uganda Ministry of Health DHIS2, accessed %s.", META$extracted)))),
      tagList(
        card(card_header("At a glance"), card_body(
          div(class = "stat-row", style = "flex-direction:column;gap:.7rem",
              div("Coverage", tags$b(sprintf("%d districts and cities, %d DLGs, %d sub-counties, %d health facilities",
                                             OU[level_name == "district", .N], OU[level_name == "dlg", .N],
                                             OU[level_name == "subcounty", .N], OU[level_name == "facility", .N]))),
              div("Period", tags$b(sprintf("%s to %s", fmt_month(MONTH_MIN), fmt_month(MONTH_MAX)))),
              div("Indicators", tags$b(sprintf("%d, in %d programme themes", nrow(IND), uniqueN(IND$theme)))),
              div("Updated", tags$b(META$extracted), span(class = "muted", "monthly indicators monthly; epidemic surveillance weekly"))))),
        card(card_header("Data sources"), card_body(tags$ul(class = "mb-0",
          tags$li("Health data, boundaries and facility coordinates: Uganda Ministry of Health, national DHIS2 (hmis.health.go.ug)."),
          tags$li("Rainfall: CHIRPS v2.0 (Climate Hazards Center, UC Santa Barbara)."),
          tags$li("Temperature: Copernicus ERA5-Land. Air quality: Copernicus CAMS. Seasonal outlook: ECMWF SEAS5 (via Open-Meteo)."),
          tags$li("Population: Uganda Bureau of Statistics subnational population statistics (census base, via OCHA HDX) and WorldPop age and sex estimates."),
          tags$li("Roads, water, schools, markets and towns: © OpenStreetMap contributors (ODbL).")))),
        card(card_header("Please note"), card_body(tags$ul(class = "mb-0",
          tags$li("Figures are computed from routine facility reports and are only as complete and accurate as those reports; see the Data quality page."),
          tags$li("The data remain the property of the Ministry of Health. This dashboard is not an official Ministry of Health publication."),
          tags$li("Forecasts and epidemic predictions support planning and early response; they do not replace investigation by the district surveillance team."),
          tags$li("For questions, corrections or collaboration, contact ", tags$a(href = "mailto:jakuze@musph.ac.ug", "jakuze@musph.ac.ug"), "."))))))
  )
}

info_server <- function(id) moduleServer(id, function(input, output, session) {})
