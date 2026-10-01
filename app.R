# Clinical Trial Feasibility Explorer
#
# Shiny sources R/ (labels, queries, plots) before this file. The app only reads
# data/trials.sqlite; building it is the pipeline's job (see README).

library(shiny)
library(bslib)

startup <- open_db()
CHOICES <- filter_choices(startup)
SNAPSHOT <- snapshot_info(startup)
DBI::dbDisconnect(startup)

DASHBOARD_QUERIES <- c("kpis", "enrollment_by_phase", "sites_per_trial", "duration_by_phase",
                       "termination_rate", "stop_reasons", "top_countries")

PHASE_CHOICES <- intersect(PHASE_LEVELS, CHOICES$phases)
EVERYTHING <- list(
  areas = CHOICES$areas,
  phases = PHASE_CHOICES,
  statuses = CHOICES$statuses,
  sponsor_classes = CHOICES$sponsor_classes,
  years = CHOICES$years
)

multi_select <- function(id, label, choices) {
  selectizeInput(id, label, choices = choices, selected = choices, multiple = TRUE,
                 options = list(plugins = list("remove_button")))
}

kpi <- function(title, output_id, note) {
  value_box(title = title, value = textOutput(output_id, inline = TRUE), p(class = "kpi-note", note))
}

chart_card <- function(id, title, note) {
  # Out of bslib's fill layout: as a fill item the SVG is squeezed to the card body's
  # height and letterboxed. Its height should follow the chart's own aspect ratio.
  plot <- htmltools::bindFillRole(ggiraph::girafeOutput(paste0(id, "_plot")),
                                  item = FALSE, overwrite = TRUE)
  navset_card_underline(
    title = title,
    full_screen = TRUE,
    nav_panel("Chart", plot),
    nav_panel("Table", tableOutput(paste0(id, "_table"))),
    footer = div(class = "chart-note", note)
  )
}

snapshot_note <- sprintf(
  "ClinicalTrials.gov API v2, pulled %s. %s interventional trials starting %s–%s.",
  substr(SNAPSHOT$fetched_at_oncology, 1, 10), fmt_int(as.numeric(SNAPSHOT$n_studies)),
  CHOICES$years[[1]], CHOICES$years[[2]]
)

ui <- page_navbar(
  title = "Clinical Trial Feasibility Explorer",
  window_title = "Trial Feasibility Explorer",
  # Only the listing fills the viewport; the dashboard scrolls.
  fillable = "Trials",
  theme = bs_theme(
    version = 5, bg = "#f9f9f7", fg = "#0b0b0b", primary = "#2a78d6",
    base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
    "card-bg" = "#fcfcfb"
  ),
  header = tags$head(tags$style(HTML("
    .kpi-note, .chart-note, .snapshot { color: #52514e; font-size: 0.8rem; margin: 0; }
    .bslib-value-box .value-box-value { font-size: 1.9rem; }
    .methods { max-width: 820px; }
    .methods table { font-size: 0.9rem; }
    .methods td, .methods th { padding: 4px 10px 4px 0; vertical-align: top; }
  "))),
  sidebar = sidebar(
    width = 300,
    checkboxGroupInput("areas", "Therapeutic area",
                       choices = labelled_choices(CHOICES$areas, AREA_LABELS), selected = CHOICES$areas),
    multi_select("phases", "Phase", PHASE_CHOICES),
    multi_select("statuses", "Overall status", labelled_choices(CHOICES$statuses, STATUS_LABELS)),
    multi_select("sponsor_classes", "Lead sponsor", labelled_choices(CHOICES$sponsor_classes, SPONSOR_CLASS_LABELS)),
    # No tick labels: at sidebar width the last two collide, and the handles show the years.
    sliderInput("years", "Start year", min = CHOICES$years[[1]], max = CHOICES$years[[2]],
                value = CHOICES$years, step = 1, sep = "", ticks = FALSE),
    actionButton("reset", "Reset filters", class = "btn-sm btn-outline-secondary"),
    hr(),
    p(class = "snapshot", snapshot_note)
  ),
  nav_panel(
    "Dashboard",
    layout_column_wrap(
      width = "190px", fill = FALSE,
      kpi("Trials", "kpi_trials", "in the current selection"),
      kpi("Median enrollment", "kpi_enrollment", "actual participants, enrolled trials"),
      kpi("Median sites", "kpi_sites", "trials listing at least one site"),
      kpi("Median duration", "kpi_duration", "completed trials, start to primary completion"),
      kpi("Stopped early", "kpi_stopped", textOutput("kpi_stopped_note", inline = TRUE))
    ),
    layout_columns(
      col_widths = c(6, 6),
      chart_card("enrollment", "Enrollment by phase",
                 "Actual enrollment only; estimated counts are targets, not outcomes."),
      chart_card("sites", "Sites per trial",
                 "Trials that list no locations are excluded.")
    ),
    layout_columns(
      col_widths = c(6, 6),
      chart_card("duration", "Duration by phase",
                 "Both dates actual. A terminated trial's completion date is when it stopped."),
      chart_card("termination", "Early termination rate",
                 sprintf("Out of trials with a final status. Phases with fewer than %d are not shown.",
                         MIN_FINAL_FOR_RATE))
    ),
    layout_columns(
      col_widths = c(6, 6),
      chart_card("stop_reasons", "Why trials stopped",
                 "Free-text reasons grouped by rules validated against 100 hand-labelled examples."),
      chart_card("countries", "Top countries",
                 "A trial with several sites in one country counts once.")
    )
  ),
  nav_panel(
    "Trials",
    card(
      full_screen = TRUE,
      card_header("Trials in the current selection"),
      DT::DTOutput("trials")
    )
  ),
  nav_panel("Methods", div(
    class = "methods",
    shiny::markdown(paste(readLines("methods.md", warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  )),
  nav_spacer(),
  nav_item(tags$a("Source code", href = "https://github.com/kartsen03/trial-feasibility-explorer",
                  target = "_blank", rel = "noopener"))
)

server <- function(input, output, session) {
  # One connection per session: the filter lives in connection-private temp tables.
  con <- open_db()
  session$onSessionEnded(function() DBI::dbDisconnect(con))

  filters <- reactive({
    list(areas = input$areas, phases = input$phases, statuses = input$statuses,
         sponsor_classes = input$sponsor_classes, years = input$years)
  }) |> debounce(400)

  # All dashboard queries for one filter state, cached across sessions: the default view
  # is computed once per app process and then served from memory.
  dashboard <- reactive({
    f <- filters()
    req(f$years)
    apply_filters(con, f)
    stats::setNames(lapply(DASHBOARD_QUERIES, function(q) run_query(con, q)), DASHBOARD_QUERIES)
  }) |> bindCache(filters())

  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "areas", selected = EVERYTHING$areas)
    for (id in c("phases", "statuses", "sponsor_classes")) {
      updateSelectizeInput(session, id, selected = EVERYTHING[[id]])
    }
    updateSliderInput(session, "years", value = EVERYTHING$years)
  })

  kpis <- reactive(dashboard()$kpis)
  output$kpi_trials <- renderText(fmt_int(kpis()$n_trials))
  output$kpi_enrollment <- renderText(fmt_int(kpis()$median_enrollment))
  output$kpi_sites <- renderText(fmt_int(kpis()$median_sites))
  output$kpi_duration <- renderText(fmt_months(kpis()$median_duration_months))
  output$kpi_stopped <- renderText(fmt_pct(kpis()$stopped_early_rate))
  output$kpi_stopped_note <- renderText(
    sprintf("of %s trials with a final status", fmt_int(kpis()$n_final))
  )

  chart <- function(query, build, rows = nrow) {
    ggiraph::renderGirafe({
      d <- dashboard()[[query]]
      validate(need(nrow(d) > 0, "No trials match the current filters."))
      p <- build(d)
      validate(need(!is.null(p), "Too few finished trials in this selection for a reliable rate."))
      as_girafe(p, rows(d))
    })
  }
  table_view <- function(query, tabulate) {
    renderTable({
      d <- dashboard()[[query]]
      validate(need(nrow(d) > 0, "No trials match the current filters."))
      tabulate(d)
    }, striped = TRUE, spacing = "s", width = "100%")
  }
  n_phases <- function(d) length(unique(d$phase))

  output$enrollment_plot <- chart("enrollment_by_phase", plot_enrollment)
  output$sites_plot <- chart("sites_per_trial", plot_sites, function(d) n_phases(d) + 1)
  output$duration_plot <- chart("duration_by_phase", plot_duration, function(d) 1.4 * n_phases(d))
  output$termination_plot <- chart("termination_rate", plot_termination,
                                   function(d) 1.4 * n_phases(d[d$n_final >= MIN_FINAL_FOR_RATE, ]))
  output$stop_reasons_plot <- chart("stop_reasons", plot_stop_reasons)
  output$countries_plot <- chart("top_countries", plot_countries)

  output$enrollment_table <- table_view("enrollment_by_phase", table_enrollment)
  output$sites_table <- table_view("sites_per_trial", table_sites)
  output$duration_table <- table_view("duration_by_phase", table_duration)
  output$termination_table <- table_view("termination_rate", table_termination)
  output$stop_reasons_table <- table_view("stop_reasons", table_stop_reasons)
  output$countries_table <- table_view("top_countries", table_countries)

  # The listing reads the same temp table, so it re-applies the filter itself rather than
  # rely on the cached dashboard having run. It only renders while its tab is open.
  trials <- reactive({
    f <- filters()
    req(f$years)
    apply_filters(con, f)
    run_query(con, "trials_table")
  })

  output$trials <- DT::renderDT({
    d <- trials()
    d$nct_id <- sprintf('<a href="https://clinicaltrials.gov/study/%1$s" target="_blank" rel="noopener">%1$s</a>', d$nct_id)
    for (a in names(AREA_LABELS)) d$areas <- gsub(a, AREA_LABELS[[a]], d$areas, fixed = TRUE)
    d$overall_status <- label_of(d$overall_status, STATUS_LABELS)
    d$sponsor_class <- label_of(d$sponsor_class, SPONSOR_CLASS_LABELS)
    DT::datatable(
      d,
      rownames = FALSE,
      escape = -1,  # only the NCT link column is trusted HTML; titles and sponsors are escaped
      filter = "top",
      fillContainer = TRUE,
      class = "compact stripe hover",
      colnames = c("NCT ID", "Title", "Areas", "Phase", "Status", "Start year", "Enrollment",
                   "Enrollment type", "Sites", "Duration (months)", "Lead sponsor",
                   "Sponsor type", "Stop reason"),
      options = list(
        pageLength = 25, scrollX = TRUE, order = list(), autoWidth = TRUE,
        columnDefs = list(list(width = "340px", targets = 1))
      )
    )
  }, server = TRUE)
}

shinyApp(ui, server)
