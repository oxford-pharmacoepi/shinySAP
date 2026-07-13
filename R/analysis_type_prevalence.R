# Analysis template: Prevalence -----------------------------------------------
#
# See R/analysis_registry.R for what a template holds and the rules for writing
# one. Pickers are built with no `choices`: sync_pickers() fills them.

register_analysis_template(
  "Prevalence",

  hint = "Proportion of a denominator population with the condition at a point or across an interval.",

  ui = function(ns, pf) tagList(
    layout_columns(
      col_widths = c(6, 6),
      entity_picker(ns("denominator_cohort"), "Denominator cohort", pf("denominator_cohort"),
                    placeholder = "Population assessed"),
      entity_picker(ns("outcome_cohort"), "Condition cohort", pf("outcome_cohort"),
                    placeholder = "Condition being counted")
    ),
    # No time-at-risk block: prevalence is measured at time points, not across a
    # window anchored on cohort entry.
    layout_columns(
      col_widths = c(4, 4, 4),
      selectInput(ns("prevalence_type"), "Prevalence type", PREVALENCE_TYPES,
                  selected = pf("prevalence_type", PREVALENCE_TYPES[1]), width = "100%"),
      numericInput(ns("interval_length_days"), "Interval length (days)",
                   value = pf("interval_length_days", NULL), width = "100%"),
      checkboxInput(ns("full_contribution"), "Require full interval contribution",
                    value = isTRUE(pf("full_contribution", TRUE)))
    ),
    textAreaInput(ns("time_points"), "Time points / intervals (one per line)",
                  join_lines(pf("time_points", character(0))), rows = 3, width = "100%",
                  placeholder = "2020-01-01\n2021-01-01\n2022-01-01"),
    strat_ui(ns, pf)
  ),

  collect = function(input) c(
    list(
      denominator_cohort   = blank_to_na(input$denominator_cohort),
      outcome_cohort       = blank_to_na(input$outcome_cohort),
      prevalence_type      = input$prevalence_type,
      interval_length_days = input$interval_length_days %||% NA,
      full_contribution    = isTRUE(input$full_contribution),
      time_points          = as_array(split_lines(input$time_points))
    ),
    strat_collect(input)
  ),

  pickers = list(cohorts = c("denominator_cohort", "outcome_cohort")),

  # Nothing nested to unpack. flatten() earns its keep here migrating an older
  # file: before 0.3.0 every analysis used the generic form, which called the
  # denominator population `target_cohort`.
  flatten = function(p) {
    if (is.null(p$denominator_cohort)) p$denominator_cohort <- p$target_cohort
    p
  }
)
