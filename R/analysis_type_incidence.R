# Analysis template: Incidence ------------------------------------------------
#
# `parameters` maps 1:1 onto IncidencePrevalence::estimateIncidence(). Nothing
# else belongs here -- if a field is not one of its arguments, it is not part of
# this analysis:
#
#   denominator_cohort  -> denominatorTable       outcome_cohort -> outcomeTable
#   censor_cohort       -> censorTable
#   estimand$interval                    -> interval
#   estimand$complete_database_intervals -> completeDatabaseIntervals
#   estimand$outcome_washout             -> outcomeWashout
#   estimand$repeated_events             -> repeatedEvents
#   estimand$strata                      -> strata
#   estimand$include_overall_strata      -> includeOverallStrata
#
# denominatorCohortId / outcomeCohortId / censorCohortId are deliberately absent:
# a cohort's id is a property of the cohort, and the Cohorts tab already carries
# it. Restating it per analysis would only let the two drift apart.
#
# Everything the cohort already fixes -- study period, age groups, sex, prior
# observation, time at risk -- is inherited, not restated. denominator_summary_ui()
# echoes it read-only.

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

    # Read-only echo of what the selected denominator already fixes.
    denominator_summary_ui(ns, pf),

    # --- Risk set definition (the estimand) ---
    layout_columns(
      col_widths = c(4, 4, 4),
      # No default: "" so it starts unset and validate() can insist on a choice.
      # estimateIncidence() itself defaults to Inf, but a washout is too
      # consequential to inherit silently -- a SAP has to say it out loud.
      # (Inf cannot be a value here; JSON has no Infinity. See WASHOUT_UNBOUNDED.)
      selectInput(ns("outcome_washout"), "Outcome washout", OUTCOME_WASHOUT_CHOICES,
                  selected = pf("outcome_washout", ""), width = "100%"),
      checkboxInput(ns("repeated_events"), "Count repeated events",
                    value = isTRUE(pf("repeated_events", FALSE))),
      entity_picker(ns("censor_cohort"), "Censoring cohort", pf("censor_cohort"),
                    placeholder = "None (optional)")
    ),

    # --- Time granularity ---
    layout_columns(
      col_widths = c(6, 6),
      selectizeInput(ns("interval"), "Interval", INTERVALS, multiple = TRUE,
                     selected = pf("interval", "years"), width = "100%"),
      checkboxInput(ns("complete_database_intervals"), "Require complete intervals",
                    value = isTRUE(pf("complete_database_intervals", TRUE)))
    ),

    # --- Stratification (columns on the denominator cohort) ---
    strata_ui(ns, pf)
  ),

  collect = function(input) list(
    denominator_cohort = blank_to_na(input$denominator_cohort),
    outcome_cohort     = blank_to_na(input$outcome_cohort),
    censor_cohort      = blank_to_na(input$censor_cohort),

    estimand = c(
      list(
        interval                    = as_array(input$interval),
        complete_database_intervals = isTRUE(input$complete_database_intervals),
        outcome_washout             = parse_washout(input$outcome_washout),
        repeated_events             = isTRUE(input$repeated_events)
      ),
      strata_collect(input)
    )
  ),

  pickers = list(
    cohorts = c("denominator_cohort", "outcome_cohort", "censor_cohort"),
    # Choices come from the selected denominator's strata_variables, not from the
    # cohort list -- see analysis_item_server().
    strata  = "strata"
  ),

  validate = function(p, cohorts) {
    errs <- character()
    # The pickers take free text, so the denominator may name a cohort nobody has
    # written down -- cohorts[[name]] would error on that. NULL is a normal answer
    # here, and denominator_summary() already flags it on the card.
    d <- cohort_by_name(cohorts, p$denominator_cohort)

    if (!is.null(d) && !d$kind %in% c("denominator", "target_denominator"))
      errs <- c(errs, "Denominator must be a denominator or target-denominator cohort.")

    # Order matters: an unset washout is not a *finite* one, so check it first or
    # an unset washout would also trip the repeated-events rule.
    if (is.null(p$estimand$outcome_washout)) {
      errs <- c(errs, "Outcome washout must be stated explicitly; there is no safe default.")
    } else if (isTRUE(p$estimand$repeated_events) &&
               washout_is_unbounded(p$estimand$outcome_washout)) {
      errs <- c(errs, "Repeated events requires a finite outcome washout.")
    }

    errs <- c(errs, validate_strata_against(p$estimand$strata, d))
    errs
  },

  # flatten() undoes collect()'s nesting so pf() can find each input again, and
  # migrates an older file onto the current inputs. Every key collect() nests MUST
  # be unpacked here or it silently comes back blank on load -- the mirror check
  # in tests/test_sap_json.R fails if one is missed.
  flatten = function(p) {
    p <- c(p, p$estimand %||% list())
    p$estimand <- NULL

    # The select holds the raw string; parse_washout() turns it back into a value.
    if (!is.null(p$outcome_washout)) p$outcome_washout <- as.character(p$outcome_washout)

    # The strata multi-select holds one token per group, crossed vars comma-joined.
    p$strata <- strata_tokens(p$strata)

    # Before 0.3.0 the generic form called the denominator `target_cohort`.
    if (is.null(p$denominator_cohort)) p$denominator_cohort <- p$target_cohort

    # 0.3.0 moved time at risk from the analysis onto the cohort. It cannot be
    # promoted from here -- an analysis prefill cannot write to a cohort -- so
    # migrate_sap() in R/utils.R does it before any section loads, and by the time
    # we get here there is nothing left to carry.
    p$time_at_risk <- NULL

    # 0.3.1 dropped these: rate-per-N and the denominator unit are presentation
    # choices made downstream, not arguments to estimateIncidence(), and a
    # sensitivity analysis is a second call rather than a parameter of this one.
    p$reporting <- NULL
    p$denominator_unit <- NULL
    p$rate_multiplier <- NULL
    p$sensitivity_analyses <- NULL
    p$stratifications <- NULL
    p
  }
)
