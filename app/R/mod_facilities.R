# Facilities: a directory of every facility, and each facility's own data view.

STAFF <- if (file.exists(file.path(DATA, "staffing.csv"))) fread(file.path(DATA, "staffing.csv")) else NULL

facilities_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Facilities", "Every health facility, and each facility's own data",
              "Find a facility by name or filter the list, then click it to open its full data view below: every indicator against its sub-county, district and Busoga, trends, reporting and data quality."),
    filter_bar(
      selectInput(ns("district"), "District / City", c("All" = "", units_at("district")), width = "190px"),
      selectInput(ns("level"), "Facility level", c("All", "Hospital", "HC IV", "HC III", "HC II", "Clinic / other"), width = "150px"),
      selectInput(ns("own"), "Ownership", c("All", "GOV", "PNFP", "PFP"), width = "120px"),
      selectInput(ns("status"), "Reporting", c("All", names(REPORT_STATUS)), width = "200px"),
      period_ui(ns("period"))),
    card(full_screen = TRUE, card_header(textOutput(ns("n"), inline = TRUE), span(class = "sub", "click a row to open the facility")),
         reactableOutput(ns("dir"))),
    div(id = ns("profile_anchor")),
    deepdive_ui(ns("profile")),
    card(card_header("Staffing", span(class = "sub", "posts filled against the staffing norm")), uiOutput(ns("staff")))
  )
}

facilities_server <- function(id) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  per <- period_server("period")
  pick <- reactiveVal(NULL)
  deepdive_server("profile", jump = pick)

  fac <- reactive({
    f <- copy(OU[level_name == "facility"])
    f[, flevel := fcase(grp_level %in% c("RRH", "General Hospital"), "Hospital", grp_level %in% c("HC IV", "HC III", "HC II"), grp_level,
                        default = "Clinic / other")]
    if (!is.null(DQF)) f <- merge(f, DQF[, .(uid, report_status, completeness_12m, dq_score)], by = "uid", all.x = TRUE)
    f[is.na(report_status), report_status := "Not expected to report"]
    if (nzchar(input$district)) f <- f[uid_l3 == input$district]
    if (input$level != "All") f <- f[flevel == input$level]
    if (input$own != "All") f <- f[grp_ownership %in% input$own]
    if (input$status != "All") f <- f[report_status == input$status]
    f
  })
  output$n <- renderText(sprintf("%d facilities · key figures for %s", nrow(fac()), per()$label))
  output$dir <- renderReactable({
    f <- fac(); p <- per()
    key <- intersect(c("SRV01", "DEL01", "ANC03", "MAL02", "HIV01", "MCM01", "MCM02", "MCM03"), IND$code)
    s <- summarise_ind(key, "facility", p$from, p$to, uids = f$uid)
    w <- if (nrow(s)) dcast(s[, .(uid, code, value)], uid ~ code, value.var = "value") else data.table(uid = character())
    x <- merge(f[, .(uid, Facility = name, Level = flevel, Ownership = grp_ownership, `Sub-county` = subcounty, District = district,
                     Reporting = report_status, Completeness = completeness_12m, `DQ score` = dq_score)], w, by = "uid", all.x = TRUE)
    setorder(x, District, Facility)
    num_col <- function(cd) colDef(name = ind_label(cd), align = "right", minWidth = 105, cell = function(v) fmt_val(v, cd))
    cols <- c(list(uid = colDef(show = FALSE),
                   Facility = colDef(minWidth = 220, sticky = "left", style = list(fontWeight = 700, color = BRAND$navy, cursor = "pointer")),
                   Reporting = colDef(minWidth = 150, cell = function(v) span(span(class = "status-dot", style = sprintf("background:%s;margin-right:.35rem", REPORT_STATUS[[v]])), v)),
                   Completeness = colDef(name = "Completeness 12 m", align = "right", cell = function(v) if (is.na(v)) "–" else sprintf("%.0f%%", v)),
                   `DQ score` = colDef(align = "right", cell = function(v) if (is.na(v)) "–" else sprintf("%.0f", v))),
              setNames(lapply(intersect(key, names(x)), num_col), intersect(key, names(x))))
    reactable(x, columns = cols, searchable = TRUE, highlight = TRUE, compact = TRUE, pagination = TRUE, defaultPageSize = 12,
              onClick = JS(sprintf("function(rowInfo){ Shiny.setInputValue('%s', rowInfo.values.uid, {priority:'event'});
                                    document.getElementById('%s').scrollIntoView({behavior:'smooth'}); }", ns("row"), ns("profile_anchor"))),
              theme = reactableTheme(headerStyle = list(fontSize = ".75rem", color = BRAND$ink2)))
  })
  observeEvent(input$row, pick(input$row))
  observeEvent(session$userData$facility_uid(), { u <- session$userData$facility_uid(); if (!is.null(u)) pick(u) }, ignoreNULL = TRUE)

  output$staff <- renderUI({
    u <- pick()
    if (is.null(STAFF)) return(tagList(
      p("Staffing data are not in DHIS2 for Busoga: the HMIS 107c human resource inventory was last reported in 2020/21 and is incomplete, so it is not shown."),
      p("To add staffing, fill the template (one row per facility and cadre: posts filled and the staffing norm), for example from iHRIS or the district HR office, and save it as app/data/staffing.csv. This panel will then show it for every facility."),
      downloadButton(ns("tmpl"), "Download staffing template", class = "btn-sm btn-outline-secondary")))
    x <- if (is.null(u)) STAFF else STAFF[uid == u]
    if (!nrow(x)) return(p(class = "muted", "No staffing rows for this facility."))
    x <- x[, .(filled = sum(filled, na.rm = TRUE), norm = sum(norm, na.rm = TRUE)), by = cadre][order(-norm)]
    renderPlotly(plot_ly(x, y = ~factor(cadre, levels = rev(cadre))) |>
      add_bars(x = ~norm, name = "Staffing norm", marker = list(color = "#d9d7ee")) |>
      add_bars(x = ~filled, name = "Filled", marker = list(color = BRAND$navy)) |>
      plotly_base() |> layout(barmode = "overlay", yaxis = list(title = ""), xaxis = list(title = "Posts")))
  })
  output$tmpl <- downloadHandler(filename = "busoga_staffing_template.csv", content = function(f)
    fwrite(OU[level_name == "facility", .(uid, facility = name, district, cadre = "", filled = NA_integer_, norm = NA_integer_)], f))
})
