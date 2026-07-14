# Analysis template: Incidence ------------------------------------------------
#
# `parameters` maps 1:1 onto IncidencePrevalence::estimateIncidence(), key for
# key and in the signature's order. If a field is not one of its arguments it is
# not part of this analysis. The keys ARE the argument names -- no wrapper object,
# no snake_case -- so the JSON reads as the call it describes:
#
#   estimateIncidence(cdm, denominatorTable, outcomeTable, censorTable,
#     denominatorCohortId, outcomeCohortId, censorCohortId, interval,
#     completeDatabaseIntervals, outcomeWashout, repeatedEvents, strata,
#     includeOverallStrata)
#
# `cdm` is a runtime database handle, not a plan field, so it is the only argument
# absent. The three *CohortId arguments select which cohorts of a denominator /
# outcome / censor SET to use; null means "all", which is the estimator's own
# default and the usual case. They are filled by the subcohort pickers (see
# `subcohorts` below), exactly as the Prevalence template fills its own.
#
# Everything the denominator cohort already fixes -- date range, age groups, sex,
# prior observation, time at risk -- is inherited, not restated here.
# denominator_summary_ui() echoes it read-only, and lists the cohort set it spans.

register_analysis_template(
  "Incidence",

  hint = "Events per unit of person-time contributed by a denominator population.",

  ui = function(ns, pf) tagList(
    layout_columns(
      col_widths = c(6, 6),
      entity_picker(ns("denominatorTable"), "Denominator cohort", pf("denominatorTable"),
                    placeholder = "Population at risk"),
      entity_picker(ns("outcomeTable"), "Outcome cohort", pf("outcomeTable"),
                    placeholder = "Event being counted")
    ),

    # Read-only echo of what the selected denominator already fixes, and of the
    # cohorts its cohort set generates -- the incidence runs on every one of them.
    denominator_summary_ui(ns, pf),

    # Filled by the analyses module (see `subcohorts` below) only when the picked
    # cohort spans a set; all of the set's IDs selected by default.
    uiOutput(ns("denominatorCohortId_ui")),
    uiOutput(ns("outcomeCohortId_ui")),

    # --- Risk set definition ---
    layout_columns(
      col_widths = c(4, 4, 4),
      # outcomeWashout is a number of days, so it is typed rather than picked from
      # a menu -- any number, not just the handful a dropdown could offer. It
      # starts blank on purpose: estimateIncidence() defaults to Inf, but a washout
      # is too consequential to inherit silently, and validate() insists on it.
      #
      # A number field cannot hold Inf, and blanking it already means "never
      # stated", so unbounded is a checkbox. See parse_washout() for the three
      # states the pair encodes.
      div(
        numericInput(ns("outcomeWashout"), "Outcome washout (days)",
                     value = washout_days_value(pf("outcomeWashout", NULL)),
                     min = 0, step = 1, width = "100%"),
        checkboxInput(ns("outcomeWashout_unbounded"), "Unbounded (one event per person)",
                      value = isTRUE(pf("outcomeWashout_unbounded", FALSE))),
        div(class = "form-text", "Leave both blank and the SAP is incomplete.")
      ),
      checkboxInput(ns("repeatedEvents"), "Count repeated events",
                    value = isTRUE(pf("repeatedEvents", FALSE))),
      entity_picker(ns("censorTable"), "Censoring cohort", pf("censorTable"),
                    placeholder = "None (optional)")
    ),
    uiOutput(ns("censorCohortId_ui")),

    # --- Time granularity ---
    layout_columns(
      col_widths = c(6, 6),
      selectizeInput(ns("interval"), "Interval", INTERVALS, multiple = TRUE,
                     selected = pf("interval", "years"), width = "100%"),
      checkboxInput(ns("completeDatabaseIntervals"), "Require complete intervals",
                    value = isTRUE(pf("completeDatabaseIntervals", TRUE)))
    ),

    # --- Stratification (columns on the denominator cohort) ---
    strata_ui(ns, pf)
  ),

  # Key order follows estimateIncidence()'s signature. An unset *CohortId is NA --
  # which serialises to null, the argument's own default of "all cohorts".
  collect = function(input) {
    denominator_ids <- as.numeric(unlist(input$denominatorCohortId))
    outcome_ids     <- as.numeric(unlist(input$outcomeCohortId))
    censor_ids      <- as.numeric(unlist(input$censorCohortId))
    list(
      denominatorTable          = blank_to_na(input$denominatorTable),
      outcomeTable              = blank_to_na(input$outcomeTable),
      censorTable               = blank_to_na(input$censorTable),
      denominatorCohortId       = if (length(denominator_ids)) I(denominator_ids) else NA,
      outcomeCohortId           = if (length(outcome_ids)) I(outcome_ids) else NA,
      censorCohortId            = if (length(censor_ids)) I(censor_ids) else NA,
      interval                  = as_array(input$interval),
      completeDatabaseIntervals = isTRUE(input$completeDatabaseIntervals),
      outcomeWashout            = parse_washout(input$outcomeWashout,
                                                input$outcomeWashout_unbounded),
      repeatedEvents            = isTRUE(input$repeatedEvents),
      strata                    = parse_strata(input$strata),
      # TRUE until the checkbox reports otherwise -- the estimator's own default,
      # and inert anyway while strata are empty. The camelCase key is the JSON
      # convention; the input id is the shared strata block's.
      includeOverallStrata      = isTRUE(input$include_overall_strata %||% TRUE)
    )
  },

  pickers = list(
    cohorts = c("denominatorTable", "outcomeTable", "censorTable"),
    # Choices are the columns the selected denominator carries (STRATA_VARIABLES),
    # not the cohort list -- see analysis_item_server().
    strata  = "strata"
  ),

  denominator = "denominatorTable",

  subcohorts = list(
    denominatorCohortId = list(from = "denominatorTable", label = "Denominator cohort IDs"),
    outcomeCohortId     = list(from = "outcomeTable",     label = "Outcome cohort IDs"),
    censorCohortId      = list(from = "censorTable",      label = "Censor cohort IDs")
  ),

  validate = function(p, cohorts) {
    errs <- character()
    # The pickers take free text, so the denominator may name a cohort nobody has
    # written down -- cohort_by_name() returns NULL for that, which is normal here,
    # and denominator_summary() already flags it on the card.
    d <- cohort_by_name(cohorts, p$denominatorTable)

    if (!is.null(d) && !d$kind %in% c("denominator", "target_denominator"))
      errs <- c(errs, "Denominator must be a denominator or target-denominator cohort.")

    # Order matters: an unset washout is not a *finite* one, so check it first or
    # an unset washout would also trip the repeated-events rule.
    w <- washout_days(p$outcomeWashout)
    if (is.null(w)) {
      errs <- c(errs, "Outcome washout must be stated explicitly; there is no safe default.")
    } else if (isTRUE(p$repeatedEvents) && is.infinite(w)) {
      errs <- c(errs, "Repeated events requires a finite outcome washout.")
    }

    errs <- c(errs, validate_strata_against(p$strata, d))
    errs
  },

  # flatten() carries an older file onto the current inputs. Before 0.4.3 the
  # Incidence parameters were snake_case and nested under an `estimand` object that
  # estimateIncidence() has no concept of; before that the generic form called the
  # denominator `target_cohort` and the analysis carried its own time at risk. Old
  # keys are read with [[, never $: on a file that lacks one, $ would partial-match
  # a longer name and migrate the wrong value.
  flatten = function(p) {
    # Un-nest the pre-0.4.3 `estimand` wrapper: its fields sat one level down.
    est <- p[["estimand"]]
    if (!is.null(est)) {
      for (k in names(est)) if (is.null(p[[k]])) p[[k]] <- est[[k]]
      p$estimand <- NULL
    }

    # Rename the pre-0.4.3 snake_case keys onto the input ids, which for every
    # field but the overall-strata checkbox are the argument names themselves.
    ren <- c(denominator_cohort = "denominatorTable", target_cohort = "denominatorTable",
             outcome_cohort = "outcomeTable", censor_cohort = "censorTable",
             complete_database_intervals = "completeDatabaseIntervals",
             outcome_washout = "outcomeWashout", repeated_events = "repeatedEvents")
    for (old in names(ren)) {
      new <- ren[[old]]
      if (is.null(p[[new]]) && !is.null(p[[old]])) p[[new]] <- p[[old]]
      p[[old]] <- NULL
    }

    # The overall-strata checkbox is the shared block's input, whose id stays
    # snake_case; the JSON key is camelCase. A new file carries includeOverallStrata
    # and needs it copied onto the input id; an old file already used the input id.
    if (is.null(p$include_overall_strata))
      p$include_overall_strata <- p[["includeOverallStrata"]]
    if (is.null(p$include_overall_strata)) p$include_overall_strata <- TRUE

    # The JSON holds ONE washout value; the form holds TWO inputs, because a number
    # field cannot show Inf and the checkbox carries it instead. Read the flag off
    # the value first -- overwriting it would destroy the Inf we need it for.
    # washout_days() also reads the pre-0.3.2 shapes: the "unbounded" sentinel
    # string, and a bare number rather than a one-element array.
    w <- p$outcomeWashout
    p$outcomeWashout_unbounded <- washout_is_unbounded(w)
    # p[...] <- list(NULL) keeps the key with a NULL value; p$x <- NULL would DELETE
    # it, and `$` partial matching would then resolve p$outcomeWashout to
    # outcomeWashout_unbounded and hand the number field a TRUE.
    p["outcomeWashout"] <- list(washout_days_value(w))

    # The strata multi-select holds one token per group, crossed vars comma-joined.
    # strata_tokens() also reads the legacy `stratifications` shape.
    p$strata <- strata_tokens(p[["strata"]] %||% p[["stratifications"]])

    # 0.3.0 moved time at risk onto the denominator cohort; migrate_sap() does that
    # before any section loads, so by here there is nothing left to carry.
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
