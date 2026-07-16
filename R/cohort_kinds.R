# Cohort kind registry --------------------------------------------------------
#
# A cohort's `kind` decides which arguments it carries, because the three kinds
# are not the same object at all:
#
#   denominator         generateDenominatorCohortSet(cohortDateRange, ageGroup,
#                       sex, daysPriorObservation, requirementInteractions)
#
#   target_denominator  generateTargetDenominatorCohortSet(... the same, plus
#                       targetCohortTable, targetCohortId, timeAtRisk,
#                       requirementsAtEntry)
#
#   everything else     A plain cohort definition -- entry events, inclusion and
#                       exit criteria, a concept set. None of the generator
#                       arguments apply: a denominator cohort set is *generated*,
#                       not defined by entry criteria.
#
# Same shape as R/analysis_registry.R: a partial registry keyed by kind, an
# "other" fallback, and ui/collect/flatten mirroring one another. Small enough to
# live in one file; split it the way analysis_type_*.R is split if it grows.
#
# Sourced before dynamic_items.R (entity_picker), utils.R (`%||%`) and bslib
# (layout_columns), so nothing here may call them at the top level.

COHORT_KINDS <- c(
  "Denominator (general population)"      = "denominator",
  "Target denominator (from a cohort)"    = "target_denominator",
  "Target cohort (exposure / index)"      = "target",
  "Outcome"                               = "outcome",
  "Comparator"                            = "comparator",
  "Censoring"                             = "censor",
  "Strata"                                = "strata",
  "Other"                                 = "other"
)

# 0.3.0 replaced `role` with `kind`. Old files still carry the role vocabulary.
#
# "Target" maps to `target`, a PLAIN cohort -- not to `target_denominator`. They
# are different objects: a target cohort is defined by entry criteria, and a
# target denominator is *generated from* it by
# generateTargetDenominatorCohortSet(). Mapping an old Target to a denominator
# would drop its entry events, inclusion criteria and concept set, since a
# denominator's block does not carry them. migrate_sap() synthesises the
# denominator instead.
COHORT_KIND_ALIASES <- c(
  "Target"     = "target",
  "Comparator" = "comparator",
  "Outcome"    = "outcome",
  "Strata"     = "strata",
  "Other"      = "other"
)

# generateDenominatorCohortSet(sex =): "one or more of Male, Female, or Both".
COHORT_SEXES <- c("Both", "Male", "Female")

# ageGroup defaults to list(c(0, 150)); 150 is the package's open-ended upper age.
AGE_MAX <- 150

# The columns generateDenominatorCohortSet() puts on the denominator table, and so
# the only things estimateIncidence(strata =) / estimatePrevalence(strata =) can
# stratify by. Fixed, not declared per cohort: the generator makes these two and
# nothing else, so a cohort has no say in it. (Until 0.4.1 this was a textarea on
# the denominator card, which let an author name a column the generator does not
# produce -- the strata picker then offered it and the validator waved it through.)
STRATA_VARIABLES <- c("age_group", "sex")

COHORT_COMMON_FIELDS <- c("name", "kind", "description")

canonical_cohort_kind <- function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(x)) return(unname(COHORT_KINDS[[1]]))
  if (x %in% COHORT_KINDS) return(x)                                  # already a kind
  if (x %in% names(COHORT_KIND_ALIASES)) return(unname(COHORT_KIND_ALIASES[[x]]))
  unname(COHORT_KINDS[[1]])
}

# Analyses look a cohort up by the name in a picker. Free text is allowed, so an
# unknown name is normal -- NULL, not an error.
cohort_by_name <- function(cohorts, nm) {
  if (length(nm) != 1 || is.na(nm) || !nzchar(nm)) return(NULL)
  if (!nm %in% names(cohorts)) return(NULL)
  cohorts[[nm]]
}

is_denominator_kind <- function(kind) {
  canonical_cohort_kind(kind) %in% c("denominator", "target_denominator")
}

# Bounded intervals ------------------------------------------------------------
#
# Both ageGroup and timeAtRisk are lists of numeric pairs -- list(c(0, 17),
# c(18, 30)) and list(c(0, 30), c(31, 60)) -- so they share a parser.
#
# An unbounded upper bound (timeAtRisk's default is c(0, Inf)) travels as JSON
# `null`: [[0, null]]. There is no "unset" state to confuse it with, because the
# argument always has a default. jsonlite writes Inf as null under na = "null"
# anyway, and reading it back gives list(0, NULL) -- so ALWAYS index a pair with
# [[1]]/[[2]] and never unlist() it, or the null collapses and the pair silently
# becomes length 1.

# "0, 30" / "0-30" / "18-64" / "0, Inf" / "65+"  ->  c(lo, hi), hi = Inf if open.
parse_bounds <- function(s) {
  s <- trimws(s)
  if (!nzchar(s)) return(NULL)
  num <- function(p) {
    if (tolower(p) %in% c("inf", "infinity", "")) return(Inf)
    suppressWarnings(as.numeric(p))
  }
  if (grepl("\\+\\s*$", s)) {
    lo <- num(sub("\\+\\s*$", "", s))
    return(if (is.na(lo)) NULL else c(lo, Inf))
  }
  parts <- trimws(unlist(strsplit(s, "[,-]")))
  parts <- parts[nzchar(parts)]
  if (length(parts) != 2) return(NULL)
  pair <- c(num(parts[1]), num(parts[2]))
  if (any(is.na(pair))) NULL else pair
}

# A textarea of one interval per line -> the JSON's list of pairs.
parse_bound_list <- function(x) {
  pairs <- lapply(split_lines(x), parse_bounds)
  pairs <- pairs[!vapply(pairs, is.null, logical(1))]
  lapply(pairs, as_num_array)
}

# The JSON's list of pairs -> textarea lines, for pf() on the way back in.
# `open` is what an unbounded upper bound reads as.
format_bound_list <- function(pairs, open = "Inf") {
  if (!length(pairs)) return(character(0))
  vapply(pairs, function(p) {
    lo <- p[[1]]
    hi <- if (length(p) >= 2) p[[2]] else NULL
    hi <- if (is.null(hi) || (is.numeric(hi) && is.infinite(hi))) open else format(hi)
    paste0(format(lo), ", ", hi)
  }, character(1))
}

# The upper bound of a pair as a number, resolving JSON null back to Inf.
bound_upper <- function(pair) {
  hi <- if (length(pair) >= 2) pair[[2]] else NULL
  if (is.null(hi)) Inf else as.numeric(hi)
}

# The registry ----------------------------------------------------------------

# The registry lives wherever this file was sourced; register_cohort_kind()
# updates it there explicitly.
COHORT_TEMPLATES <- list()
cohort_registry_env <- environment()

# hint      one line shown above the block, or NULL
# ui        function(ns, pf) -> the kind's inputs
# collect   function(input) -> the kind's JSON, reading ONLY its own input ids
# pickers   input ids that pick another cohort
# flatten   function(params) -> prefill keys; the inverse of collect's nesting
# validate  function(cohort, cohorts) -> character() of problems
register_cohort_kind <- function(kind, hint = NULL, ui, collect,
                                 pickers = list(), flatten = function(p) p,
                                 validate = function(cohort, cohorts) character(0)) {
  cohort_registry_env$COHORT_TEMPLATES[[kind]] <- list(
    hint = hint, ui = ui, collect = collect, pickers = pickers,
    flatten = flatten, validate = validate
  )
}

cohort_template <- function(kind) {
  tmpl <- COHORT_TEMPLATES[[canonical_cohort_kind(kind)]]
  if (is.null(tmpl)) COHORT_TEMPLATES[["other"]] else tmpl
}

# cohortDateRange ---------------------------------------------------------------
#
# The argument takes TWO dates -- its default is literally as.Date(c(NA, NA)) --
# so it is ONE key holding a two-element array, not two keys. A missing bound is
# null, which is what the argument itself means by NA: "use the earliest (or
# latest) observation period in the database".
#
#   ["2015-01-01", "2024-12-31"]   both bounds given
#   ["2015-01-01", null]           open-ended at the top
#   [null, null]                   the argument's own default
#
# Never unlist() it, for the same reason as ageGroup and timeAtRisk: a null bound
# would collapse and the pair would silently become length 1.
cohort_date_range <- function(start, end) as_array(c(blank_to_na(start), blank_to_na(end)))

# One bound of a cohortDateRange, as the date field wants it. NULL when unset.
date_bound <- function(range, i) {
  if (is.null(range) || length(range) < i) return(NULL)
  v <- range[[i]]
  if (is.null(v) || (length(v) == 1 && is.na(v))) return(NULL)
  as.character(v)
}

# Shared blocks ---------------------------------------------------------------
#
# Input ids ARE the generator's argument names, so the JSON key and the field that
# produced it cannot drift apart -- the same convention the Prevalence template
# uses for the estimators. cohortDateRange is the one exception: one argument, two
# date fields, so the ids carry a Start/End suffix and collect() pairs them up.

# The requirements every denominator cohort set is generated with, target or not.
# Both generators take these under exactly these names.
denominator_requirements_ui <- function(ns, pf) tagList(
  # cohortDateRange takes Dates, so these are pickers rather than free text.
  # dateInput() has no placeholder, and blank is a meaningful value here, so what
  # blank *means* moves into help text under each field.
  layout_columns(
    col_widths = c(6, 6),
    div(
      date_input(ns("cohortDateRangeStart"), "Cohort date range: earliest start",
                 pf("cohortDateRangeStart")),
      div(class = "form-text", "Blank = the earliest observation period in the database.")
    ),
    div(
      date_input(ns("cohortDateRangeEnd"), "Cohort date range: latest end",
                 pf("cohortDateRangeEnd")),
      div(class = "form-text", "Blank = the latest observation period in the database.")
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    # ageGroup = list(c(0, 17), c(18, 30)): numeric pairs, one cohort each.
    textAreaInput(ns("ageGroup"), "Age groups (one per line, as lower, upper)",
                  join_lines(format_bound_list(pf("ageGroup", list()), open = AGE_MAX)),
                  rows = 3, width = "100%", placeholder = "0, 17\n18, 64\n65, 150"),
    selectizeInput(ns("sex"), "Sex", COHORT_SEXES, multiple = TRUE, width = "100%",
                   selected = pf("sex", "Both"),
                   options = list(placeholder = "One or more of Both / Male / Female"))
  ),
  layout_columns(
    col_widths = c(6, 6),
    selectizeInput(ns("daysPriorObservation"), "Days of prior observation required",
                   choices = character(0), multiple = TRUE, width = "100%",
                   selected = as.character(unlist(pf("daysPriorObservation", 0))),
                   options = list(create = TRUE, placeholder = "0 (type more to vary it)")),
    checkboxInput(ns("requirementInteractions"),
                  "Generate a cohort for every combination of age group, sex and prior observation",
                  value = isTRUE(pf("requirementInteractions", TRUE)))
  )
  # No strata input: the columns a denominator carries are not the author's to
  # choose. generateDenominatorCohortSet() produces age_group and sex, and nothing
  # else -- see STRATA_VARIABLES, which is now the only place that is said.
)

denominator_requirements_collect <- function(input) list(
  cohortDateRange         = cohort_date_range(input$cohortDateRangeStart,
                                              input$cohortDateRangeEnd),
  ageGroup                = parse_bound_list(input$ageGroup),
  sex                     = as_array(input$sex %||% "Both"),
  daysPriorObservation    = as_num_array(input$daysPriorObservation %||% 0),
  requirementInteractions = isTRUE(input$requirementInteractions)
)

# One cohortDateRange key becomes the two date fields that produced it. Read with
# [[, never $: on a cohort that lacks the key, $ would partial-match
# cohortDateRangeStart and hand back a single date where a pair belongs.
denominator_requirements_flatten <- function(p) {
  dr <- p[["cohortDateRange"]]
  p$cohortDateRangeStart <- date_bound(dr, 1)
  p$cohortDateRangeEnd   <- date_bound(dr, 2)
  p
}

# What the generator actually produces ------------------------------------------
#
# A denominator cohort is not ONE cohort -- it is a cohort SET, and the arguments
# on the card are the axes it is crossed over. Three age groups and two sexes is
# six cohorts, and an analysis run on it is run on all six. The Analyses tab shows
# this back, so an author can see the six rather than infer them.
#
# requirementInteractions decides how the axes combine, and the two cases are very
# different:
#
#   TRUE (the default)  every combination of ageGroup x sex x daysPriorObservation.
#   FALSE               "only the first value specified for the other factors will
#                       be used" -- so each factor varies alone against the first
#                       value of the others. That is why the docs warn that ORDER
#                       MATTERS when it is FALSE: the first value of each is the
#                       baseline everything else is measured against.
#
# timeAtRisk is deliberately NOT one of those factors -- requirementInteractions
# does not mention it. Each interval "creates one set of denominator cohorts", so
# it multiplies whatever the requirements produce, interactions or not.
denominator_cohort_set <- function(cohort) {
  first_or <- function(x, default) if (length(x)) x else default
  ages  <- first_or(cohort$ageGroup %||% list(), list(as_num_array(c(0, AGE_MAX))))
  sexes <- first_or(as.character(unlist(cohort$sex %||% character(0))), "Both")
  prior <- first_or(as.numeric(unlist(cohort$daysPriorObservation %||% numeric(0))), 0)

  req <- function(a, s, d) list(ageGroup = ages[[a]], sex = sexes[[s]],
                                daysPriorObservation = prior[[d]])
  combos <- list()
  if (isTRUE(cohort$requirementInteractions %||% TRUE)) {
    for (a in seq_along(ages)) {
      for (s in seq_along(sexes)) {
        for (d in seq_along(prior)) combos[[length(combos) + 1]] <- req(a, s, d)
      }
    }
  } else {
    # The baseline -- the first value of every factor -- then each further level of
    # each factor against it. Hence "order matters" when interactions are off.
    combos[[1]] <- req(1, 1, 1)
    for (a in seq_along(ages)[-1])  combos[[length(combos) + 1]] <- req(a, 1, 1)
    for (s in seq_along(sexes)[-1]) combos[[length(combos) + 1]] <- req(1, s, 1)
    for (d in seq_along(prior)[-1]) combos[[length(combos) + 1]] <- req(1, 1, d)
  }

  # Only a target denominator has a time at risk; a plain one contributes all
  # observed time, so there is a single implicit set.
  windows <- if (identical(canonical_cohort_kind(cohort$kind), "target_denominator")) {
    first_or(cohort$timeAtRisk %||% list(), list(as_num_array(c(0, Inf))))
  } else {
    list(NULL)
  }

  out <- list()
  for (w in windows) {
    for (cmb in combos) {
      cmb$timeAtRisk <- w
      out[[length(out) + 1]] <- cmb
    }
  }
  out
}

# One generated cohort as a single line, e.g.
#   "Age 18, 64 · Female · 365 days prior observation · time at risk 0, 30"
format_denominator_cohort <- function(x) {
  parts <- c(
    sprintf("Age %s", format_bound_list(list(x$ageGroup), open = AGE_MAX)),
    x$sex,
    sprintf("%s days prior observation", format(x$daysPriorObservation))
  )
  if (!is.null(x$timeAtRisk)) {
    parts <- c(parts, sprintf("time at risk %s", format_bound_list(list(x$timeAtRisk))))
  }
  paste(parts, collapse = " · ")
}

# The plain cohort definition: what a source cohort actually is.
cohort_definition_ui <- function(ns, pf) tagList(
  layout_columns(
    col_widths = c(6, 6),
    textAreaInput(ns("entry_events"), "Entry events (one per line)",
                  join_lines(pf("entry_events", character(0))), rows = 4, width = "100%",
                  placeholder = "First metformin dispensation"),
    textAreaInput(ns("exit_criteria"), "Exit criteria (one per line)",
                  join_lines(pf("exit_criteria", character(0))), rows = 4, width = "100%",
                  placeholder = "End of continuous observation")
  ),
  textAreaInput(ns("inclusion_criteria"), "Inclusion / exclusion criteria (one per line)",
                join_lines(pf("inclusion_criteria", character(0))), rows = 3, width = "100%",
                placeholder = "Aged 18 or over at index\nNo prior insulin exposure"),
  textInput(ns("concept_set"), "Concept set / codelist", pf("concept_set"), width = "100%")
)

cohort_definition_collect <- function(input) list(
  entry_events       = as_array(split_lines(input$entry_events)),
  inclusion_criteria = as_array(split_lines(input$inclusion_criteria)),
  exit_criteria      = as_array(split_lines(input$exit_criteria)),
  concept_set        = blank_to_na(input$concept_set)
)

# Shared validation ------------------------------------------------------------

validate_denominator_requirements <- function(p) {
  errs <- character(0)
  for (g in p$ageGroup %||% list()) {
    if (bound_upper(g) < as.numeric(g[[1]])) {
      errs <- c(errs, sprintf("Age group '%s' has an upper bound below its lower bound.",
                              format_bound_list(list(g), open = AGE_MAX)))
    }
  }
  if (!length(as.character(unlist(p$sex %||% character(0))))) {
    errs <- c(errs, "Sex must be at least one of Both, Male or Female.")
  }
  errs
}

# The kinds -------------------------------------------------------------------

register_cohort_kind(
  "denominator",
  hint = paste("Generated with generateDenominatorCohortSet(). A denominator cohort set is",
               "generated from the whole database, not defined by entry criteria."),
  ui = function(ns, pf) denominator_requirements_ui(ns, pf),
  collect = function(input) denominator_requirements_collect(input),
  flatten = denominator_requirements_flatten,
  validate = function(cohort, cohorts) validate_denominator_requirements(cohort)
)

register_cohort_kind(
  "target_denominator",
  hint = paste("Generated with generateTargetDenominatorCohortSet(): the same requirements,",
               "restricted to the time a person spends in a target cohort."),
  ui = function(ns, pf) tagList(
    entity_picker(ns("targetCohortTable"), "Target cohort to build the denominator from",
                  pf("targetCohortTable"),
                  placeholder = "Another cohort defined on this tab (targetCohortTable)"),
    # timeAtRisk = list(c(0, 30), c(31, 60)): lower and upper bounds in days,
    # BOTH relative to target cohort entry. There is no anchoring on cohort end --
    # if time at risk runs past cohort exit or the observation period, only the
    # time up to those is contributed. Each interval generates its own cohort set.
    textAreaInput(ns("timeAtRisk"),
                  "Time at risk (one interval per line, days from target cohort entry)",
                  join_lines(format_bound_list(pf("timeAtRisk", list(I(c(0, Inf)))))),
                  rows = 3, width = "100%", placeholder = "0, Inf\n0, 30\n31, 60"),
    denominator_requirements_ui(ns, pf),
    checkboxInput(ns("requirementsAtEntry"),
                  "Requirements must be met on the target cohort start date",
                  value = isTRUE(pf("requirementsAtEntry", TRUE)))
  ),
  # Keys, and their order, are generateTargetDenominatorCohortSet()'s own. `cdm` is
  # a live database handle, not a plan field; `name` is the cohort's own name, in
  # the common half of the card. targetCohortId is not captured yet -- the cohort
  # ids are handled internally for now.
  collect = function(input) {
    req <- denominator_requirements_collect(input)
    list(
      targetCohortTable       = blank_to_na(input$targetCohortTable),
      cohortDateRange         = req$cohortDateRange,
      timeAtRisk              = parse_bound_list(input$timeAtRisk),
      ageGroup                = req$ageGroup,
      sex                     = req$sex,
      daysPriorObservation    = req$daysPriorObservation,
      requirementsAtEntry     = isTRUE(input$requirementsAtEntry),
      requirementInteractions = req$requirementInteractions
    )
  },
  pickers = list(cohorts = "targetCohortTable"),
  flatten = denominator_requirements_flatten,
  validate = function(cohort, cohorts) {
    errs <- validate_denominator_requirements(cohort)
    if (is.na(cohort$targetCohortTable %||% NA)) {
      errs <- c(errs, "A target denominator must name the target cohort it is built from.")
    } else {
      t <- cohort_by_name(cohorts, cohort$targetCohortTable)
      if (!is.null(t) && is_denominator_kind(t$kind)) {
        errs <- c(errs, "The target cohort must be a plain cohort, not another denominator.")
      }
      if (identical(cohort$targetCohortTable, cohort$name)) {
        errs <- c(errs, "A target denominator cannot be built from itself.")
      }
    }
    if (!length(cohort$timeAtRisk %||% list())) {
      errs <- c(errs, "Time at risk must have at least one interval (the default is 0, Inf).")
    }
    for (w in cohort$timeAtRisk %||% list()) {
      if (bound_upper(w) < as.numeric(w[[1]])) {
        errs <- c(errs, sprintf("Time at risk '%s' ends before it starts.",
                                format_bound_list(list(w))))
      }
    }
    errs
  }
)

# Outcome, comparator, censoring, strata and anything else: a plain cohort. None
# of the generator arguments apply -- and note there is no washout here, because
# neither generator takes one. A washout is estimateIncidence(outcomeWashout =),
# which the Incidence analysis captures.
register_cohort_kind(
  "other",
  ui = function(ns, pf) cohort_definition_ui(ns, pf),
  collect = function(input) cohort_definition_collect(input)
)
