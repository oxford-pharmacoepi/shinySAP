# Analysis templates: survival -------------------------------------------------
#
# Two schema types, one builder: single_event_survival maps onto
# CohortSurvival::estimateSingleEventSurvival() and competing_risk_survival onto
# estimateCompetingRiskSurvival(), which adds a competing outcome cohort with its
# own date variable and washout. Survival runs on plain cohorts (a target and its
# outcomes), so there is no denominator summary; strata are columns of the target
# cohort table this app cannot see, so they are free tokens.

survival_ui <- function(competing) function(ns, pf) shiny::tagList(
  bslib::layout_columns(
    col_widths = if (competing) c(4, 4, 4) else c(6, 6),
    entity_picker(ns("target_cohort_id"), "Target cohort", pf("target_cohort_id"),
                  placeholder = "Who is at risk; follow-up starts at entry"),
    entity_picker(ns("outcome_cohort_id"), "Outcome cohort", pf("outcome_cohort_id"),
                  placeholder = "The event of interest"),
    if (competing) entity_picker(ns("competing_outcome_cohort_id"), "Competing outcome cohort",
                                 pf("competing_outcome_cohort_id"),
                                 placeholder = "The event that prevents the outcome")
  ),

  section_heading("Event dates and washout"),
  bslib::layout_columns(
    col_widths = c(6, 6),
    shiny::div(
      shiny::selectInput(ns("outcome_date_variable"), "Outcome event date",
                         c("Not stated" = "", "cohort_start_date", "cohort_end_date"),
                         selected = pf("outcome_date_variable"), width = "100%"),
      unbounded_days_ui(ns, pf, "outcome_washout", "Outcome washout (days before index)",
                        "Unbounded (exclude anyone with a prior outcome)")
    ),
    if (competing) shiny::div(
      shiny::selectInput(ns("competing_outcome_date_variable"), "Competing outcome event date",
                         c("Not stated" = "", "cohort_start_date", "cohort_end_date"),
                         selected = pf("competing_outcome_date_variable"), width = "100%"),
      unbounded_days_ui(ns, pf, "competing_outcome_washout",
                        "Competing outcome washout (days before index)",
                        "Unbounded (exclude anyone with a prior competing outcome)")
    )
  ),

  section_heading("Censoring and follow-up"),
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    shiny::div(
      shiny::checkboxInput(ns("censor_on_cohort_exit"), "Censor at target cohort exit",
                           value = isTRUE(pf("censor_on_cohort_exit", FALSE)), width = "100%"),
      unbounded_days_ui(ns, pf, "follow_up_days", "Follow-up (days)", "Unbounded")
    ),
    shiny::div(
      date_input(ns("censor_on_date"), "Censor on a date", pf("censor_on_date")),
      shiny::textInput(ns("censor_on_date_variable"), "... or on a date column of the target cohort",
                       pf("censor_on_date_variable"), width = "100%",
                       placeholder = "e.g. end_of_study")
    ),
    shiny::div(
      shiny::textInput(ns("weight"), "Weight column (target cohort)", pf("weight"), width = "100%",
                       placeholder = "None (optional)"),
      shiny::div(class = "form-text", "A numeric column of observation weights for the Kaplan-Meier estimate.")
    )
  ),

  section_heading("Estimates"),
  bslib::layout_columns(
    col_widths = c(3, 3, 3, 3),
    shiny::numericInput(ns("event_gap"), "Event gap (days)", value = pf("event_gap", NULL),
                        min = 1, step = 1, width = "100%"),
    shiny::numericInput(ns("estimate_gap"), "Estimate gap (days)", value = pf("estimate_gap", NULL),
                        min = 1, step = 1, width = "100%"),
    shiny::numericInput(ns("restricted_mean_follow_up"), "Restricted mean horizon (days)",
                        value = pf("restricted_mean_follow_up", NULL), min = 1, step = 1, width = "100%"),
    shiny::numericInput(ns("minimum_survival_days"), "Minimum survival (days)",
                        value = pf("minimum_survival_days", NULL), min = 0, step = 1, width = "100%")
  ),

  section_heading("Stratification"),
  strata_ui(ns, pf, free = TRUE)
)

survival_collect <- function(competing) function(input) compact(c(
  list(
    target_cohort_id  = chr_or_null(input$target_cohort_id),
    outcome_cohort_id = chr_or_null(input$outcome_cohort_id)
  ),
  if (competing) list(competing_outcome_cohort_id = chr_or_null(input$competing_outcome_cohort_id)),
  list(outcome_date_variable = chr_or_null(input$outcome_date_variable)),
  if (competing) list(competing_outcome_date_variable = chr_or_null(input$competing_outcome_date_variable)),
  list(outcome_washout = parse_unbounded_days(input$outcome_washout, input$outcome_washout_unbounded)),
  if (competing) list(competing_outcome_washout = parse_unbounded_days(
    input$competing_outcome_washout, input$competing_outcome_washout_unbounded)),
  list(
    censor_on_cohort_exit     = isTRUE(input$censor_on_cohort_exit),
    censor_on_date            = as_date_value(input$censor_on_date),
    censor_on_date_variable   = chr_or_null(input$censor_on_date_variable),
    weight                    = chr_or_null(input$weight),
    follow_up_days            = parse_unbounded_days(input$follow_up_days, input$follow_up_days_unbounded),
    strata                    = parse_strata(input$strata),
    event_gap                 = num_or_null(input$event_gap),
    estimate_gap              = num_or_null(input$estimate_gap),
    restricted_mean_follow_up = num_or_null(input$restricted_mean_follow_up),
    minimum_survival_days     = num_or_null(input$minimum_survival_days)
  )
))

survival_flatten <- function(competing) function(p) {
  p <- unbounded_days_prefill(p, "outcome_washout")
  p <- unbounded_days_prefill(p, "follow_up_days")
  if (competing) p <- unbounded_days_prefill(p, "competing_outcome_washout")
  # [[ not $: `$` would partial-match censor_on_date_variable when the date is absent.
  if (!is.null(p[["censor_on_date"]])) p[["censor_on_date"]] <- format(as.Date(p[["censor_on_date"]]), "%Y-%m-%d")
  p$strata <- strata_tokens(p$strata)
  p
}

register_analysis_template(
  "single_event_survival",
  hint = "Kaplan-Meier survival from target cohort entry to a single outcome event (CohortSurvival).",
  ui = survival_ui(FALSE),
  collect = survival_collect(FALSE),
  pickers = list(cohorts = c("target_cohort_id", "outcome_cohort_id"), strata = "strata"),
  flatten = survival_flatten(FALSE),
  package = "CohortSurvival"
)

register_analysis_template(
  "competing_risk_survival",
  hint = "Cumulative incidence of an outcome in the presence of a competing event (CohortSurvival).",
  ui = survival_ui(TRUE),
  collect = survival_collect(TRUE),
  pickers = list(cohorts = c("target_cohort_id", "outcome_cohort_id", "competing_outcome_cohort_id"),
                 strata = "strata"),
  flatten = survival_flatten(TRUE),
  package = "CohortSurvival"
)
