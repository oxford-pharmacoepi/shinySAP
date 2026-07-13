# Analysis template: Incidence ------------------------------------------------
#
# See R/analysis_registry.R for what a template holds and the rules for writing
# one. Pickers are built with no `choices`: sync_pickers() fills them.

register_analysis_template(
  "Incidence",

  hint = "Events per unit of person-time contributed by a denominator population.",

  ui = function(ns, pf) tagList(
    layout_columns(
      col_widths = c(6, 6),
      entity_picker(ns("denominator_cohort"), "Denominator cohort", pf("denominator_cohort"),
                    placeholder = "Population at risk"),
      entity_picker(ns("outcome_cohort"), "Outcome cohort", pf("outcome_cohort"),
                    placeholder = "Event being counted")
    ),
    tar_ui(ns, pf),
    layout_columns(
      col_widths = c(4, 4, 4),
      selectInput(ns("denominator_unit"), "Denominator unit", DENOMINATOR_UNITS,
                  selected = pf("denominator_unit", DENOMINATOR_UNITS[1]), width = "100%"),
      numericInput(ns("rate_multiplier"), "Report rate per", value = pf("rate_multiplier", 1000),
                   width = "100%"),
      checkboxInput(ns("repeated_events"), "Count repeated events",
                    value = isTRUE(pf("repeated_events", FALSE)))
    ),
    textAreaInput(ns("calendar_intervals"), "Calendar intervals (one per line)",
                  join_lines(pf("calendar_intervals", character(0))), rows = 2, width = "100%",
                  placeholder = "2015-2019\n2020-2024"),
    strat_ui(ns, pf)
  ),

  collect = function(input) c(
    list(
      denominator_cohort = blank_to_na(input$denominator_cohort),
      outcome_cohort     = blank_to_na(input$outcome_cohort),
      denominator_unit   = input$denominator_unit,
      rate_multiplier    = input$rate_multiplier %||% NA,
      repeated_events    = isTRUE(input$repeated_events),
      calendar_intervals = as_array(split_lines(input$calendar_intervals))
    ),
    tar_collect(input),
    strat_collect(input)
  ),

  pickers = list(cohorts = c("denominator_cohort", "outcome_cohort")),

  # flatten() is also where an older file gets migrated onto the current inputs.
  # Before 0.3.0 every analysis used the generic form, which called the
  # denominator population `target_cohort` -- carry it across rather than drop a
  # cohort reference on the floor.
  flatten = function(p) {
    if (is.null(p$denominator_cohort)) p$denominator_cohort <- p$target_cohort
    tar_flatten(p)
  }
)
