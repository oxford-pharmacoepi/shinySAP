# Analysis template: Prevalence -----------------------------------------------
#
# See R/analysis_registry.R for what a template holds and the rules for writing
# one. Pickers are built with no `choices`: sync_pickers() fills them.
#
# Fields mirror IncidencePrevalence::estimatePointPrevalence() and
# estimatePeriodPrevalence(). The denominator population itself (age groups, sex,
# prior observation, date range -- generateDenominatorCohortSet()) is NOT captured
# here: the user defines it as a cohort on the Cohorts tab and points at it below.
#
# The two estimators take different intervals ("overall" is period-only) and
# different arguments, so each prevalence type has its own interval select and
# collect() serialises only the selected type's arguments -- the JSON never
# carries a knob the chosen estimator does not have. Parameter keys use the
# estimators' own argument names (denominatorTable, timePoint, strata, ...),
# which is also why this template has its own strata textarea instead of the
# shared strat_ui() block and its snake_case keys.

register_analysis_template(
  "Prevalence",

  hint = paste0("Proportion of a denominator population with the condition, at a point ",
                "(point prevalence) or across an interval (period prevalence)."),

  ui = function(ns, pf) tagList(
    layout_columns(
      col_widths = c(6, 6),
      entity_picker(ns("denominatorTable"), "Denominator cohort", pf("denominatorTable"),
                    placeholder = "Population assessed (defined on the Cohorts tab)"),
      entity_picker(ns("outcomeTable"), "Outcome cohort", pf("outcomeTable"),
                    placeholder = "Cohort counted as prevalent cases")
    ),
    # Read-only echo of what the selected denominator already fixes, and of the
    # cohorts its cohort set actually generates -- a prevalence runs on every one
    # of them, exactly as an incidence does.
    denominator_summary_ui(ns, pf),

    # Filled by the analyses module (see `subcohorts` below) only when the
    # picked cohort spans a set; all of the set's IDs selected by default.
    uiOutput(ns("denominatorCohortId_ui")),
    uiOutput(ns("outcomeCohortId_ui")),
    # No time-at-risk block: prevalence is measured at points or over calendar
    # intervals, not across a window anchored on cohort entry.
    selectInput(ns("prevalence_type"), "Prevalence type", PREVALENCE_TYPES,
                selected = pf("prevalence_type", PREVALENCE_TYPES[1]), width = "100%"),

    # Point prevalence: timePoint locates the measurement within each interval.
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Point prevalence'", ns("prevalence_type")),
      layout_columns(
        col_widths = c(6, 6),
        selectInput(ns("point_interval"), "Interval", POINT_PREVALENCE_INTERVALS,
                    selected = pf("point_interval", "years"), width = "100%"),
        selectInput(ns("timePoint"), "Time point within interval", PREVALENCE_TIMEPOINTS,
                    selected = pf("timePoint", PREVALENCE_TIMEPOINTS[1]), width = "100%")
      )
    ),

    # Period prevalence: who counts within an interval, and how records combine.
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Period prevalence'", ns("prevalence_type")),
      layout_columns(
        col_widths = c(6, 6),
        selectInput(ns("period_interval"), "Interval", PERIOD_PREVALENCE_INTERVALS,
                    selected = pf("period_interval", "years"), width = "100%"),
        selectInput(ns("level"), "Estimation level", PREVALENCE_LEVELS,
                    selected = pf("level", PREVALENCE_LEVELS[1]), width = "100%")
      ),
      layout_columns(
        col_widths = c(6, 6),
        checkboxInput(ns("fullContribution"), "Require full interval contribution",
                      value = isTRUE(pf("fullContribution", FALSE)), width = "100%"),
        checkboxInput(ns("completeDatabaseIntervals"), "Complete database intervals only",
                      value = isTRUE(pf("completeDatabaseIntervals", TRUE)), width = "100%")
      )
    ),

    # The shared structured-strata block: one token per stratification, comma to
    # cross variables, choices being the columns the chosen denominator carries
    # (see pickers$strata and `denominator` below).
    strata_ui(ns, pf)
  ),

  # No prevalence_type reads as point, matching the select's default above.
  collect = function(input) {
    point <- !identical(input$prevalence_type, "Period prevalence")
    # Explicit IDs when the picked cohort spans a set; null -- which
    # IncidencePrevalence reads as "all" -- when it does not.
    denominator_ids <- as.numeric(unlist(input$denominatorCohortId))
    outcome_ids     <- as.numeric(unlist(input$outcomeCohortId))
    # Key order follows the estimators' signatures.
    c(
      list(
        denominatorTable    = blank_to_na(input$denominatorTable),
        outcomeTable        = blank_to_na(input$outcomeTable),
        denominatorCohortId = if (length(denominator_ids)) I(denominator_ids) else NA,
        outcomeCohortId     = if (length(outcome_ids)) I(outcome_ids) else NA,
        # No prevalence_type key: the point/period split is serialised one
        # level up, as the analysis_type (see serialised_type below).
        interval            = if (point) input$point_interval else input$period_interval
      ),
      if (point) list(
        timePoint = input$timePoint
      ) else list(
        completeDatabaseIntervals = isTRUE(input$completeDatabaseIntervals),
        fullContribution          = isTRUE(input$fullContribution),
        level                     = input$level
      ),
      list(
        strata               = parse_strata(input$strata),
        # TRUE until the checkbox reports otherwise -- the estimators' own
        # default, and inert anyway while strata are empty. The camelCase key
        # is this template's convention; the input id is the shared block's.
        includeOverallStrata = isTRUE(input$include_overall_strata %||% TRUE)
      )
    )
  },

  pickers = list(
    cohorts = c("denominatorTable", "outcomeTable"),
    # Choices are the columns the selected denominator carries (STRATA_VARIABLES),
    # not the cohort list -- see analysis_item_server().
    strata  = "strata"
  ),

  denominator = "denominatorTable",

  subcohorts = list(
    denominatorCohortId = list(from = "denominatorTable", label = "Denominator cohort IDs"),
    outcomeCohortId     = list(from = "outcomeTable",     label = "Outcome cohort IDs")
  ),

  validate = function(p, cohorts)
    validate_strata_against(p$strata, cohort_by_name(cohorts, p$denominatorTable)),

  # The file's analysis_type names the estimator planned; "Prevalence" is only
  # the registry key. ANALYSIS_TYPE_ALIASES maps both names back on load.
  serialised_type = function(input)
    if (identical(input$prevalence_type, "Period prevalence")) "estimatePeriodPrevalence"
    else "estimatePointPrevalence",

  # flatten() carries older files onto the current inputs. Before 0.3.0 every
  # analysis used the generic form, which called the denominator population
  # `target_cohort`; the first prevalence template stored an interval length in
  # days and free-text time points; and the keys were snake_case before they
  # were aligned with the estimators' argument names. Old keys are read with
  # [[, not $: on a file that lacks one, $ would partial-match a longer name
  # (`time_point` finds `time_points`) and migrate the wrong value.
  flatten = function(p) {
    # The point/period select is not among the parameters: the split lives in
    # the file's analysis_type, which load() passes through here. Files saved
    # before the estimator rename still carry prevalence_type and keep it;
    # with neither, the select's own default (point) applies.
    if (is.null(p$prevalence_type))
      p$prevalence_type <- switch(as.character(p$analysis_type %||% ""),
        estimatePointPrevalence  = "Point prevalence",
        estimatePeriodPrevalence = "Period prevalence") %||% "Point prevalence"

    if (is.null(p$denominatorTable))
      p$denominatorTable <- p[["denominator_cohort"]] %||% p[["target_cohort"]]
    if (is.null(p$outcomeTable))
      p$outcomeTable <- p[["outcome_cohort"]]
    if (is.null(p$timePoint))
      p$timePoint <- p[["time_point"]]
    if (is.null(p$fullContribution))
      p$fullContribution <- p[["full_contribution"]]
    if (is.null(p$completeDatabaseIntervals))
      p$completeDatabaseIntervals <- p[["complete_database_intervals"]]
    # The checkbox id is the shared block's snake_case; the JSON key is this
    # template's camelCase. Files from before the alignment already used the
    # snake_case key, which is the input id itself.
    if (is.null(p$include_overall_strata))
      p$include_overall_strata <- p[["includeOverallStrata"]]
    if (is.null(p$include_overall_strata))
      p$include_overall_strata <- TRUE

    # Tokens for the strata multi-select. strata_tokens() reads every shape
    # this field has had: a list of variable groups (current), a flat array of
    # free-text lines (the textarea era), and legacy `stratifications`.
    p$strata <- strata_tokens(p[["strata"]] %||% p[["stratifications"]])

    # collect() serialises only the selected type's arguments, so a loaded
    # point analysis carries no period fields and vice versa. Fill the gaps
    # with the estimators' own defaults: every rendered input must be findable
    # after a round trip (the mirror invariant), and a type switch after load
    # should start from the defaults, not blanks.
    if (is.null(p$timePoint))                 p$timePoint                 <- PREVALENCE_TIMEPOINTS[1]
    if (is.null(p$fullContribution))          p$fullContribution          <- FALSE
    if (is.null(p$completeDatabaseIntervals)) p$completeDatabaseIntervals <- TRUE
    if (is.null(p$level))                     p$level                     <- PREVALENCE_LEVELS[1]

    # An old interval length becomes the new interval when the day count is a
    # calendar unit. Free-text time_points, and lengths that fit no unit, have
    # no field left to land in (this template has no sensitivity_analyses), so
    # they do not survive a load. `interval` is read with [[ throughout: on an
    # old record with no such key, $ would partial-match interval_length_days
    # and make the interval look already set.
    days <- p$interval_length_days
    if (is.null(p[["interval"]]) && length(days) == 1 && !is.na(days)) {
      p$interval <-
        if (days == 7) "weeks"
        else if (days >= 28 && days <= 31) "months"
        else if (days >= 90 && days <= 92) "quarters"
        else if (days >= 365 && days <= 366) "years"
    }

    # The one saved `interval` feeds whichever select applies. "overall" never
    # reaches the point select: it is not among its choices, and a selected
    # value selectInput cannot find would silently become the first choice.
    interval <- p[["interval"]]
    p$period_interval <- interval
    if (!is.null(interval) && !identical(interval, "overall"))
      p$point_interval <- interval
    p
  }
)
