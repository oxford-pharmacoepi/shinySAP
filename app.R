# shinySAP -- structured capture of a Statistical Analysis Plan as JSON.
#
# Run with:  shiny::runApp("Documents/shinySAP")
#
# Files in R/ are sourced automatically by Shiny.

if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")))
  Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64")

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
# 0.4.1 dropped the denominator's declared strata_variables: the generator makes
#       age_group and sex and nothing else, so they are fixed, not authored.
# 0.4.2 named every denominator key after the generator argument it feeds, and
#       folded the two date keys into the one cohortDateRange pair it takes.
# 0.4.3 did the same for Incidence: its parameters are now flat and named exactly
#       for estimateIncidence() (denominatorTable, outcomeTable, censorTable,
#       *CohortId, interval, completeDatabaseIntervals, outcomeWashout,
#       repeatedEvents, strata, includeOverallStrata), dropping the `estimand`
#       wrapper and the snake_case keys.
# 0.4.4 trimmed cdm_sources to name / source_key / country (pickers now refer to
#       sources by source_key), and in cdm_changes dropped cdm_version and
#       replaced the scalar data_source with a data_sources array.
# 0.4.5 retyped cdm_changes around the questions a SAP asks of the CDM (extra
#       validations, database-specific alterations, removing people with no year
#       of birth or sex data). The old table-edit taxonomy described alterations,
#       so legacy types map there and cdm_table/cdm_field fold into the
#       description on load.
# 0.4.6 promoted the common alterations to change types of their own (subset a
#       table, limit observation periods, remap concepts, implausible dates);
#       unrecognised types land on "Other database-specific alteration".
# 0.4.7 dropped a cdm_change's rationale; an old one folds into the description.
# 0.4.8 gave study an amendments array (version, date, description of change).
# 0.4.9 gave study an aim (the research question introducing the numbered
#       objectives; background doubles as rationale).
# 0.4.10 renamed study$acronym to study$study_code: what authors put there is a
#        short study name or code, not an acronym.
# 0.4.11 dropped a cohort's cohort_id: IDs are assigned at generation time, not
#        authored in the SAP. Without them the *CohortId sub-pickers stay hidden
#        and those parameters serialise null = all cohorts in the set.
SAP_SCHEMA_VERSION <- "0.4.11"

# Overridable so tests or a deployment can write somewhere else.
OUTPUT_DIR <- getOption("shinySAP.output_dir", "output")

ui <- page_navbar(
  title = "shinySAP",
  id = "nav",
  theme = bs_theme(version = 5, preset = "shiny"),
  window_title = "shinySAP",
  header = tags$head(tags$style(HTML("
    /* item_card(): the header toggles the body. The chevron points down when the
       card is open and right when it is shut; Bootstrap flips aria-expanded. */
    .item-card-toggle .item-card-chevron { transition: transform .15s ease-in-out; }
    .item-card-toggle[aria-expanded='false'] .item-card-chevron { transform: rotate(-90deg); }
    .item-card-toggle:focus { box-shadow: none; }

    /* Load SAP in the navbar: strip fileInput() down to its button -- no
       filename box, no progress bar -- and dress it as a nav link, pinned to
       the right edge and in the theme's primary blue. */
    .nav-item:has(> .navbar-load) { margin-left: auto; }
    /* width: shiny-input-container defaults to 300px; shrink to the button. */
    .navbar-load { margin-bottom: 0; width: auto !important; }
    .navbar-load .form-control, .navbar-load .progress { display: none; }
    .navbar-load .btn-file {
      background: none; border: none;
      font-size: var(--bs-nav-link-font-size, 1rem);
      font-weight: 700;
      color: var(--bs-primary, #0d6efd);
      padding: var(--bs-nav-link-padding-y, .5rem) var(--bs-nav-link-padding-x, .5rem);
    }
    .navbar-load .btn-file:hover,
    .navbar-load .btn-file:focus { color: var(--bs-link-hover-color, #0a58ca); }
  "))),
  nav_panel("Study", div(class = "container-fluid py-3", study_ui("study"))),
  nav_panel("CDM Sources", div(class = "container-fluid py-3", cdm_sources_ui("sources"))),
  nav_panel("CDM Changes", div(class = "container-fluid py-3", cdm_changes_ui("cdm"))),
  nav_panel("Cohorts", div(class = "container-fluid py-3", cohorts_ui("cohorts"))),
  nav_panel("Proposed Analyses", div(class = "container-fluid py-3", analyses_ui("analyses"))),
  nav_panel("Review & Save", div(class = "container-fluid py-3", review_ui("review"))),
  nav_spacer(),
  # Load acts on the whole SAP, not one section, so it lives outside the tabs.
  nav_item(
    tagAppendAttributes(
      fileInput("load", NULL, accept = ".json",
                buttonLabel = tagList(icon("upload"), "Load SAP"),
                placeholder = ""),
      class = "navbar-load"
    )
  )
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

  observeEvent(input$load, {
    loaded <- tryCatch(read_sap(input$load$datapath), error = function(e) e)
    if (inherits(loaded, "error")) {
      showNotification(paste("Could not read that file:", conditionMessage(loaded)), type = "error")
      return()
    }
    failed <- tryCatch({ load_sap(loaded); NULL }, error = function(e) e)
    if (!is.null(failed)) {
      showNotification(paste("Could not load that SAP:", conditionMessage(failed)), type = "error")
      return()
    }
    showNotification("SAP loaded.", type = "message")
  })

  review_server("review", sap = sap, output_dir = OUTPUT_DIR,
                problems = reactive(c(cohorts$problems(), analyses$problems())))
}

shinyApp(ui, server)
