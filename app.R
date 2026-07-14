# shinySAP -- structured capture of a Statistical Analysis Plan as JSON.
#
# Run with:  shiny::runApp("Documents/shinySAP")
#
# Files in R/ are sourced automatically by Shiny.

library(shiny)
library(bslib)
library(jsonlite)

# 0.2.0 added cdm_sources and renamed analyses -> proposed_analyses.
# 0.3.0 moved the type-specific analysis fields under `parameters`.
# 0.4.0 added cohort sets: cohorts gained parent_cohort, and prevalence
#       parameters gained denominatorCohortId / outcomeCohortId (null = all).
# 0.5.0 dropped the prevalence_type parameter: a prevalence analysis_type now
#       names the estimator (estimatePointPrevalence / estimatePeriodPrevalence).
# 0.6.0 prevalence: stratifications renamed to strata, sensitivity_analyses
#       removed (other analysis types keep both).
SAP_SCHEMA_VERSION <- "0.6.0"

# Overridable so tests or a deployment can write somewhere else.
OUTPUT_DIR <- getOption("shinySAP.output_dir", "output")

ui <- page_navbar(
  title = "shinySAP",
  id = "nav",
  theme = bs_theme(version = 5, preset = "shiny"),
  window_title = "shinySAP",
  nav_panel("Study", div(class = "container-fluid py-3", study_ui("study"))),
  nav_panel("CDM Sources", div(class = "container-fluid py-3", cdm_sources_ui("sources"))),
  nav_panel("CDM Changes", div(class = "container-fluid py-3", cdm_changes_ui("cdm"))),
  nav_panel("Cohorts", div(class = "container-fluid py-3", cohorts_ui("cohorts"))),
  nav_panel("Proposed Analyses", div(class = "container-fluid py-3", analyses_ui("analyses"))),
  nav_panel("Review & Save", div(class = "container-fluid py-3", review_ui("review")))
)

server <- function(input, output, session) {
  study    <- study_server("study")
  sources  <- cdm_sources_server("sources")
  cdm      <- cdm_changes_server("cdm", source_names = sources$names)
  cohorts  <- cohorts_server("cohorts")
  analyses <- analyses_server("analyses",
                              cohort_names = cohorts$names,
                              source_names = sources$names,
                              cohort_details = cohorts$data)

  # The single source of truth for what gets serialised.
  sap <- reactive(list(
    sap_schema_version = SAP_SCHEMA_VERSION,
    generated_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    study              = study$data(),
    cdm_sources        = sources$data(),
    cdm_changes        = cdm$data(),
    cohorts            = cohorts$data(),
    proposed_analyses  = analyses$data()
  ))

  load_sap <- function(loaded) {
    study$load(loaded$study %||% list())
    sources$load(loaded$cdm_sources %||% list())
    cdm$load(loaded$cdm_changes %||% list())
    cohorts$load(loaded$cohorts %||% list())
    # "analyses" is the pre-0.2.0 name for this section.
    analyses$load(coalesce_key(loaded, "proposed_analyses", "analyses"))
    nav_select("nav", selected = "Study", session = session)
  }

  review_server("review", sap = sap, output_dir = OUTPUT_DIR, on_load = load_sap)
}

shinyApp(ui, server)
