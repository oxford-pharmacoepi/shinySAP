# shinySAP -- structured capture of a Statistical Analysis Plan as JSON.
#
# Run with:  shiny::runApp("Documents/shinySAP")
#
# Files in R/ are sourced automatically by Shiny.

library(shiny)
library(bslib)
library(jsonlite)

SAP_SCHEMA_VERSION <- "0.1.0"

# Overridable so tests or a deployment can write somewhere else.
OUTPUT_DIR <- getOption("shinySAP.output_dir", "output")

ui <- page_navbar(
  title = "shinySAP",
  id = "nav",
  theme = bs_theme(version = 5, preset = "shiny"),
  window_title = "shinySAP",
  nav_panel("Study", div(class = "container-fluid py-3", study_ui("study"))),
  nav_panel("CDM Changes", div(class = "container-fluid py-3", cdm_changes_ui("cdm"))),
  nav_panel("Cohorts", div(class = "container-fluid py-3", cohorts_ui("cohorts"))),
  nav_panel("Analyses", div(class = "container-fluid py-3", analyses_ui("analyses"))),
  nav_panel("Review & Save", div(class = "container-fluid py-3", review_ui("review")))
)

server <- function(input, output, session) {
  study    <- study_server("study")
  cdm      <- cdm_changes_server("cdm")
  cohorts  <- cohorts_server("cohorts")
  analyses <- analyses_server("analyses", cohort_names = cohorts$names)

  # The single source of truth for what gets serialised.
  sap <- reactive(list(
    sap_schema_version = SAP_SCHEMA_VERSION,
    generated_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    study              = study$data(),
    cdm_changes        = cdm$data(),
    cohorts            = cohorts$data(),
    analyses           = analyses$data()
  ))

  load_sap <- function(loaded) {
    study$load(loaded$study %||% list())
    cdm$load(loaded$cdm_changes %||% list())
    cohorts$load(loaded$cohorts %||% list())
    analyses$load(loaded$analyses %||% list())
    nav_select("nav", selected = "Study", session = session)
  }

  review_server("review", sap = sap, output_dir = OUTPUT_DIR, on_load = load_sap)
}

shinyApp(ui, server)
