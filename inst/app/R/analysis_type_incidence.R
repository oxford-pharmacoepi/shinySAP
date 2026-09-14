# Analysis template: incidence -------------------------------------------------
#
# `parameters` are the schema's incidence parameters, which map onto
# IncidencePrevalence::estimateIncidence() (see sap_code.R for the argument
# names). Everything the denominator cohort already fixes -- age groups, sex,
# prior observation, time at risk -- is inherited, not restated here.

register_analysis_template(
  "incidence",

  hint = "Events per unit of person-time contributed by a denominator population.",

  ui = function(ns, pf) shiny::tagList(
    denominator_outcome_ui(ns, pf),
    denominator_summary_ui(ns, pf),
    cohort_ids_of_interest_ui(ns, pf),

    section_heading("Risk set"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::div(
        # A washout is too consequential to inherit silently: it starts blank,
        # and the schema-level check on Review asks for it only indirectly (the
        # estimator's default is Inf), so the hint says what blank means.
        unbounded_days_ui(ns, pf, "washout", "Outcome washout (days)",
                          "Unbounded (one event per person)"),
        shiny::div(class = "form-text", "Blank = the estimator's default, one event per person."),
        shiny::checkboxInput(ns("repeated_events"), "Count repeated events",
                             value = isTRUE(pf("repeated_events", FALSE)), width = "100%"),
        shiny::div(class = "form-text",
                   "Requires a finite washout: after each event's washout elapses, the person re-enters time at risk.")
      ),
      shiny::div(
        entity_picker(ns("censor_cohort_id"), "Censoring cohort", pf("censor_cohort_id"),
                      placeholder = "None (optional)"),
        shiny::div(class = "form-text",
                   "Follow-up ends at this cohort's event. It must hold one record per person.")
      )
    ),

    section_heading("Time granularity"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::selectInput(ns("interval"), "Interval",
                         c("Not stated" = "", shinySAP::sapSchemaVocabulary()$time_intervals("incidence")),
                         selected = pf("interval"), width = "100%"),
      shiny::checkboxInput(ns("complete_database_intervals"), "Require complete intervals",
                           value = isTRUE(pf("complete_database_intervals", TRUE)))
    ),

    section_heading("Stratification"),
    strata_ui(ns, pf)
  ),

  # Keys in the schema's order; unauthored values are omitted.
  collect = function(input) compact(list(
    denominator_cohort_id              = chr_or_null(input$denominator_cohort_id),
    outcome_cohort_id                  = chr_or_null(input$outcome_cohort_id),
    censor_cohort_id                   = chr_or_null(input$censor_cohort_id),
    denominator_cohort_ids_of_interest = vec_or_null(chr_vec(input$denominator_cohort_ids_of_interest)),
    outcome_cohort_ids_of_interest     = vec_or_null(chr_vec(input$outcome_cohort_ids_of_interest)),
    interval                           = chr_or_null(input$interval),
    complete_database_intervals        = isTRUE(input$complete_database_intervals %||% TRUE),
    washout                            = parse_unbounded_days(input$washout, input$washout_unbounded),
    repeated_events                    = isTRUE(input$repeated_events),
    strata                             = parse_strata(input$strata),
    include_overall_strata             = isTRUE(input$include_overall_strata %||% TRUE)
  )),

  pickers = list(
    cohorts = c("denominator_cohort_id", "outcome_cohort_id", "censor_cohort_id"),
    strata  = "strata"
  ),

  denominator = "denominator_cohort_id",

  flatten = function(p) {
    p <- unbounded_days_prefill(p, "washout")
    p$strata <- strata_tokens(p$strata)
    p
  }
)
