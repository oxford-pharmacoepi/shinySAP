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

    section_heading("Risk set"),
    layout_columns(
      col_widths = c(6, 6),
      # Washout and repeated events are ONE decision pair -- the validator ties
      # them together (repeated events needs a finite washout) -- so they share
      # a column; censoring is a separate concept and gets its own.
      #
      # outcomeWashout is a number of days, so it is typed rather than picked from
      # a menu -- any number, not just the handful a dropdown could offer. It
      # starts blank on purpose: estimateIncidence() defaults to Inf, but a washout
      # is too consequential to inherit silently, and validate() insists on it.
      # A number field cannot hold Inf, and blanking it already means "never
      # stated", so unbounded is a checkbox. See parse_washout() for the three
      # states the pair encodes.
      div(
        numericInput(ns("outcomeWashout"), "Outcome washout (days)",
                     value = washout_days_value(pf("outcomeWashout", NULL)),
                     min = 0, step = 1, width = "100%"),
        checkboxInput(ns("outcomeWashout_unbounded"), "Unbounded (one event per person)",
                      value = isTRUE(pf("outcomeWashout_unbounded", FALSE))),
        div(class = "form-text", "Leave both blank and the SAP is incomplete."),
        checkboxInput(ns("repeatedEvents"), "Count repeated events",
                      value = isTRUE(pf("repeatedEvents", FALSE)), width = "100%"),
        div(class = "form-text",
            "Requires a finite washout: after each event's washout elapses, the
             person re-enters time at risk.")
      ),
      div(
        entity_picker(ns("censorTable"), "Censoring cohort", pf("censorTable"),
                      placeholder = "None (optional)"),
        # estimateIncidence(censorTable =): "must only include one record per
        # person". A data-level constraint the SAP cannot check -- but it can
        # say it, so the author picks a suitable cohort.
        div(class = "form-text",
            "Follow-up ends at this cohort's event. It must hold one record per person."),
        uiOutput(ns("censorCohortId_ui"))
      )
    ),
    cohort_role_notes_ui(ns, pf),

    section_heading("Time granularity"),
    layout_columns(
      col_widths = c(6, 6),
      selectizeInput(ns("interval"), "Interval", INTERVALS, multiple = TRUE,
                     selected = pf("interval", "years"), width = "100%"),
      checkboxInput(ns("completeDatabaseIntervals"), "Require complete intervals",
                    value = isTRUE(pf("completeDatabaseIntervals", TRUE)))
    ),

    # Columns on the denominator cohort; see strata_ui().
    section_heading("Stratification"),
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

    if (!is.null(d) && !is_denominator_kind(d$kind))
      errs <- c(errs, "Denominator must be a denominator or target-denominator cohort.")

    # The mirror-image mistake: outcome and censoring must be PLAIN cohorts --
    # pointing either at a generated denominator is as wrong as the reverse.
    for (side in list(c("outcomeTable", "Outcome"), c("censorTable", "Censoring"))) {
      ch <- cohort_by_name(cohorts, p[[side[[1]]]])
      if (!is.null(ch) && is_denominator_kind(ch$kind)) {
        errs <- c(errs, sprintf("%s must be a plain cohort, not a generated denominator.",
                                side[[2]]))
      }
    }

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

  # flatten() is the inverse of collect(): the JSON keys back onto the input ids.
  # Every key here is a live one -- the two places the ids and the argument names
  # genuinely differ, plus the defaults an absent key needs.
  flatten = function(p) {
    # The overall-strata checkbox is the shared block's input, whose id stays
    # snake_case; the JSON key is the argument's camelCase.
    if (is.null(p$include_overall_strata))
      p$include_overall_strata <- p[["includeOverallStrata"]]
    if (is.null(p$include_overall_strata)) p$include_overall_strata <- TRUE

    # The JSON holds ONE washout value; the form holds TWO inputs, because a number
    # field cannot show Inf and the checkbox carries it instead. Read the flag off
    # the value first -- overwriting it would destroy the Inf we need it for.
    w <- p$outcomeWashout
    p$outcomeWashout_unbounded <- washout_is_unbounded(w)
    # p[...] <- list(NULL) keeps the key with a NULL value; p$x <- NULL would DELETE
    # it, and `$` partial matching would then resolve p$outcomeWashout to
    # outcomeWashout_unbounded and hand the number field a TRUE.
    p["outcomeWashout"] <- list(washout_days_value(w))

    # The strata multi-select holds one token per group, crossed vars comma-joined.
    p$strata <- strata_tokens(p[["strata"]])
    p
  }
)
