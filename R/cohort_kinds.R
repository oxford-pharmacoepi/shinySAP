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

# The columns generateDenominatorCohortSet() puts on the denominator table, and
# so the only things estimateIncidence(strata =) can stratify by unless the ETL
# adds more.
DEFAULT_STRATA_VARIABLES <- c("age_group", "sex")

COHORT_COMMON_FIELDS <- c("name", "kind", "cohort_id", "parent_cohort", "description")

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

COHORT_TEMPLATES <- list()

# hint      one line shown above the block, or NULL
# ui        function(ns, pf) -> the kind's inputs
# collect   function(input) -> the kind's JSON, reading ONLY its own input ids
# pickers   input ids that pick another cohort
# flatten   function(params) -> prefill keys; the inverse of collect's nesting
# validate  function(cohort, cohorts) -> character() of problems
register_cohort_kind <- function(kind, hint = NULL, ui, collect,
                                 pickers = list(), flatten = function(p) p,
                                 validate = function(cohort, cohorts) character(0)) {
  COHORT_TEMPLATES[[kind]] <<- list(
    hint = hint, ui = ui, collect = collect, pickers = pickers,
    flatten = flatten, validate = validate
  )
}

cohort_template <- function(kind) {
  tmpl <- COHORT_TEMPLATES[[canonical_cohort_kind(kind)]]
  if (is.null(tmpl)) COHORT_TEMPLATES[["other"]] else tmpl
}

# Shared blocks ---------------------------------------------------------------

# The requirements every denominator cohort set is generated with, target or not.
denominator_requirements_ui <- function(ns, pf) tagList(
  # cohortDateRange takes Dates, so these are pickers rather than free text.
  # dateInput() has no placeholder, and blank is a meaningful value here, so what
  # blank *means* moves into help text under each field.
  layout_columns(
    col_widths = c(6, 6),
    div(
      date_input(ns("cohort_date_range_start"), "Cohort date range: earliest start",
                 pf("cohort_date_range_start")),
      div(class = "form-text", "Blank = the earliest observation period in the database.")
    ),
    div(
      date_input(ns("cohort_date_range_end"), "Cohort date range: latest end",
                 pf("cohort_date_range_end")),
      div(class = "form-text", "Blank = the latest observation period in the database.")
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    # ageGroup = list(c(0, 17), c(18, 30)): numeric pairs, one cohort each.
    textAreaInput(ns("age_groups"), "Age groups (one per line, as lower, upper)",
                  join_lines(format_bound_list(pf("age_groups", list()), open = AGE_MAX)),
                  rows = 3, width = "100%", placeholder = "0, 17\n18, 64\n65, 150"),
    selectizeInput(ns("sex"), "Sex", COHORT_SEXES, multiple = TRUE, width = "100%",
                   selected = pf("sex", "Both"),
                   options = list(placeholder = "One or more of Both / Male / Female"))
  ),
  layout_columns(
    col_widths = c(6, 6),
    selectizeInput(ns("days_prior_observation"), "Days of prior observation required",
                   choices = character(0), multiple = TRUE, width = "100%",
                   selected = as.character(unlist(pf("days_prior_observation", 0))),
                   options = list(create = TRUE, placeholder = "0 (type more to vary it)")),
    checkboxInput(ns("requirement_interactions"),
                  "Generate a cohort for every combination of age group, sex and prior observation",
                  value = isTRUE(pf("requirement_interactions", TRUE)))
  ),
  # estimateIncidence(strata =) can only use columns on the denominator table.
  # The generator always produces age_group and sex; list anything your ETL adds.
  textAreaInput(ns("strata_variables"), "Strata columns on this denominator (one per line)",
                join_lines(pf("strata_variables", DEFAULT_STRATA_VARIABLES)), rows = 2,
                width = "100%", placeholder = "age_group\nsex\nregion")
)

denominator_requirements_collect <- function(input) list(
  cohort_date_range_start  = blank_to_na(input$cohort_date_range_start),
  cohort_date_range_end    = blank_to_na(input$cohort_date_range_end),
  age_groups               = parse_bound_list(input$age_groups),
  sex                      = as_array(input$sex %||% "Both"),
  days_prior_observation   = as_num_array(input$days_prior_observation %||% 0),
  requirement_interactions = isTRUE(input$requirement_interactions),
  strata_variables         = as_array(split_lines(input$strata_variables))
)

# Nothing nested beyond the pair lists, which pf() re-formats in the UI.
denominator_requirements_flatten <- function(p) p

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
  for (g in p$age_groups %||% list()) {
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
    entity_picker(ns("target_cohort"), "Target cohort to build the denominator from",
                  pf("target_cohort"),
                  placeholder = "Another cohort defined on this tab (targetCohortTable)"),
    # timeAtRisk = list(c(0, 30), c(31, 60)): lower and upper bounds in days,
    # BOTH relative to target cohort entry. There is no anchoring on cohort end --
    # if time at risk runs past cohort exit or the observation period, only the
    # time up to those is contributed. Each interval generates its own cohort set.
    textAreaInput(ns("time_at_risk"),
                  "Time at risk (one interval per line, days from target cohort entry)",
                  join_lines(format_bound_list(pf("time_at_risk", list(I(c(0, Inf)))))),
                  rows = 3, width = "100%", placeholder = "0, Inf\n0, 30\n31, 60"),
    denominator_requirements_ui(ns, pf),
    checkboxInput(ns("requirements_at_entry"),
                  "Requirements must be met on the target cohort start date",
                  value = isTRUE(pf("requirements_at_entry", TRUE)))
  ),
  collect = function(input) c(
    list(
      target_cohort         = blank_to_na(input$target_cohort),
      time_at_risk          = parse_bound_list(input$time_at_risk),
      requirements_at_entry = isTRUE(input$requirements_at_entry)
    ),
    denominator_requirements_collect(input)
  ),
  pickers = list(cohorts = "target_cohort"),
  flatten = denominator_requirements_flatten,
  validate = function(cohort, cohorts) {
    errs <- validate_denominator_requirements(cohort)
    if (is.na(cohort$target_cohort %||% NA)) {
      errs <- c(errs, "A target denominator must name the target cohort it is built from.")
    } else {
      t <- cohort_by_name(cohorts, cohort$target_cohort)
      if (!is.null(t) && is_denominator_kind(t$kind)) {
        errs <- c(errs, "The target cohort must be a plain cohort, not another denominator.")
      }
      if (identical(cohort$target_cohort, cohort$name)) {
        errs <- c(errs, "A target denominator cannot be built from itself.")
      }
    }
    if (!length(cohort$time_at_risk %||% list())) {
      errs <- c(errs, "Time at risk must have at least one interval (the default is 0, Inf).")
    }
    for (w in cohort$time_at_risk %||% list()) {
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
