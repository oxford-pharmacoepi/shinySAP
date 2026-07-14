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
# 0.3.1 made the Incidence parameters map 1:1 onto estimateIncidence().
# 0.4.0 added cohort sets (cohorts gained parent_cohort; prevalence gained
#       denominatorCohortId / outcomeCohortId, null = all IDs in the set) and
#       aligned prevalence with the estimators: parameters use the argument
#       names and order, strata replaces stratifications, sensitivity_analyses
#       is dropped there, and a prevalence analysis_type names the estimator
#       (estimatePointPrevalence / estimatePeriodPrevalence).
SAP_SCHEMA_VERSION <- "0.4.0"

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
  # by_name, not just names: the templates echo what the denominator cohort
  # already fixes, and validate against it.
  analyses <- analyses_server("analyses",
                              cohort_names = cohorts$names,
                              cohort_index = cohorts$by_name,
                              source_names = sources$names)

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
    # Anything that has to move between sections (0.3.0 put time at risk on the
    # cohort, not the analysis) must happen before any section builds its cards.
    loaded <- migrate_sap(loaded)
    study$load(loaded$study %||% list())
    sources$load(loaded$cdm_sources %||% list())
    cdm$load(loaded$cdm_changes %||% list())
    cohorts$load(loaded$cohorts %||% list())
    # "analyses" is the pre-0.2.0 name for this section.
    analyses$load(coalesce_key(loaded, "proposed_analyses", "analyses"))
    nav_select("nav", selected = "Study", session = session)
  }

  review_server("review", sap = sap, output_dir = OUTPUT_DIR, on_load = load_sap,
                problems = reactive(c(cohorts$problems(), analyses$problems())))
}

shinyApp(ui, server)
