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
  "Target cohort (population of interest)" = "target",
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

# Ids a denominator kind's ui() renders that are OUTPUTS, not inputs: the live
# preview of the generated cohort set, filled by cohort_item_server(). They hold
# no value, so collect() never reads them and the tests exempt them from the
# collect -> JSON -> flatten round trip -- the same idea as the analysis
# registry's DISPLAY_ONLY_IDS.
COHORT_DISPLAY_ONLY_IDS <- "cohort_set_preview"

# 0.4.12 dropped `description`: the kind block already says what a cohort IS in
# structured form (entry events, criteria, generator arguments), and free text
# beside it only drifted out of step.
# 0.4.13 added `data_sources`: the SAP-level counterpart of the generators'
# `cdm` argument -- WHICH databases the cohort is built against. Common, not a
# kind field: every kind is built against a database, and the kind blocks stay
# exactly the generator's other arguments.
COHORT_COMMON_FIELDS <- c("name", "kind", "data_sources")

# "" is the UNSET kind: a new card starts with none chosen, because a kind is a
# decision -- the same rule as sex and requirementInteractions. Unset renders a
# prompt instead of a block, collects nothing kind-specific, and is a validation
# problem (cohort_problems), never a silent default. An unknown non-empty kind
# (a hand-edited file) falls back to "other": reading it as a plain cohort loses
# nothing, where reading it as a denominator would invent generator arguments.
canonical_cohort_kind <- function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(x)) return("")
  if (x %in% COHORT_KINDS) return(x)                                  # already a kind
  if (x %in% names(COHORT_KIND_ALIASES)) return(unname(COHORT_KIND_ALIASES[[x]]))
  "other"
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

# One saved (or live) cohort -> the prefill its card is rebuilt from. Shared by
# cohorts load() and the Duplicate button, so the two can never drift.
cohort_to_prefill <- function(ch) {
  # 0.2.0 called it `role`, with a different vocabulary.
  ch$kind <- canonical_cohort_kind(ch$kind %||% ch$role)
  cohort_template(ch$kind)$flatten(ch)
}

# Everything wrong with one cohort. The kind check comes first: with no kind
# there is no template to validate against -- and falling through to the plain
# template would find nothing wrong, which would read as "fine".
cohort_problems <- function(ch, cohorts) {
  kind <- canonical_cohort_kind(ch$kind)
  if (!nzchar(kind)) {
    return("This cohort has no kind chosen, so it carries no fields and cannot be validated.")
  }
  tryCatch(
    as.character(cohort_template(kind)$validate(ch, cohorts)),
    error = function(e) paste("Could not validate this cohort:", conditionMessage(e))
  )
}

# Names are the identity everything joins on (cohort_by_name, the analysis
# pickers), so two cohorts sharing one silently shadow each other: the pickers
# offer a single entry and every lookup takes the first. One problem entry per
# duplicated name, shaped like the per-cohort entries in problems_r.
duplicate_name_problems <- function(cohorts) {
  nms <- vapply(cohorts, function(ch) {
    nm <- as.character(ch$name %||% "")[1]
    if (is.na(nm)) "" else trimws(nm)
  }, character(1))
  nms <- nms[nzchar(nms)]
  lapply(unique(nms[duplicated(nms)]), function(nm) list(
    name = nm,
    messages = sprintf(
      "%d cohorts are named '%s'. Everything referring to that name silently uses the first; rename one.",
      sum(nms == nm), nm)
  ))
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
  # date_input()'s placeholder shows the FORMAT; blank is a meaningful value
  # here, so what blank *means* stays in the help text under each field.
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
  # The three requirement factors together, in one row: they are the axes the
  # cohort set is crossed over. requirementInteractions is NOT one of them -- it
  # is the rule for how they combine -- so it sits below all three at full
  # width, rather than beside one of them as if it belonged to it.
  layout_columns(
    col_widths = c(6, 6),
    # ageGroup = list(c(0, 17), c(18, 30)): numeric pairs, one cohort each.
    textAreaInput(ns("ageGroup"), "Age groups (one per line, as lower, upper)",
                  join_lines(format_bound_list(pf("ageGroup", list()), open = AGE_MAX)),
                  rows = 4, width = "100%", placeholder = "0, 17\n18, 64\n65, 150"),
    # Nothing is preselected: a prefilled "Both" or "0" would be a decision the
    # author never made. The generator's own defaults still apply downstream --
    # the preview says so -- but the SAP records only what was actually chosen,
    # and the sex validator objects until the author picks.
    div(
      selectizeInput(ns("sex"), "Sex", COHORT_SEXES, multiple = TRUE, width = "100%",
                     selected = pf("sex"),
                     options = list(placeholder = "One or more of Both / Male / Female")),
      # The saved values MUST be among the choices: shiny silently drops a
      # selected value it cannot find, so with empty choices every rebuild
      # (load, kind switch, duplicate) emptied the field and the next autosave
      # wrote [] -- the values were never saved back. Same defense as
      # entity_picker() and the index_rule field.
      selectizeInput(ns("daysPriorObservation"), "Days of prior observation required",
                     choices = as.character(unlist(pf("daysPriorObservation"))),
                     multiple = TRUE, width = "100%",
                     selected = as.character(unlist(pf("daysPriorObservation"))),
                     options = list(create = TRUE,
                                    placeholder = "Type one or more numbers of days, e.g. 0 or 365"))
    )
  ),
  checkboxInput(ns("requirementInteractions"),
                "Generate a cohort for every combination of age group, sex and prior observation",
                value = isTRUE(pf("requirementInteractions", FALSE)), width = "100%"),
  # The unticked behaviour is the surprising half, and it silently changes what
  # the ORDER of the values above means -- so it is said here, not only in the
  # package docs.
  div(class = "form-text mb-2",
      paste("Unticked, the first value of each field above is the baseline, and each further",
            "value varies alone against it — so the order of the values matters."))
  # No strata input: the columns a denominator carries are not the author's to
  # choose. generateDenominatorCohortSet() produces age_group and sex, and nothing
  # else -- see STRATA_VARIABLES, which is now the only place that is said.
)

# Empty stays empty -- writing "Both" or 0 for an untouched field would put a
# decision in the JSON the author never made, and flatten() would then prefill
# it back into the form as if they had.
denominator_requirements_collect <- function(input) list(
  cohortDateRange         = cohort_date_range(input$cohortDateRangeStart,
                                              input$cohortDateRangeEnd),
  ageGroup                = parse_bound_list(input$ageGroup),
  sex                     = as_array(input$sex %||% character(0)),
  daysPriorObservation    = as_num_array(input$daysPriorObservation %||% numeric(0)),
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
#   "Age 18, 64 | Female | 365 days prior observation | time at risk 0, 30"
format_denominator_cohort <- function(x) {
  parts <- c(
    sprintf("Age %s", format_bound_list(list(x$ageGroup), open = AGE_MAX)),
    x$sex,
    sprintf("%s days prior observation", format(x$daysPriorObservation))
  )
  if (!is.null(x$timeAtRisk)) {
    parts <- c(parts, sprintf("time at risk %s", format_bound_list(list(x$timeAtRisk))))
  }
  paste(parts, collapse = " | ")
}

# Which occurrence of the entry event indexes the cohort -- "Index date" in
# protocol tables. A RULE, not a date (hence the key index_rule): "influenza
# vaccination" says what qualifies, "first per season" says which occurrence
# indexes, and conflating the two into one entry-event line hid a decision
# reviewers need to see. A small canonical core plus a free tail (create =
# TRUE), because "first per influenza season" fits no enum.
INDEX_RULES <- c(
  "First occurrence ever",
  "First occurrence per period (state which)",
  "Latest occurrence",
  "All occurrences (re-entry per episode)"
)

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
  layout_columns(
    col_widths = c(6, 6),
    selectizeInput(ns("index_rule"), "Index date",
                   choices = unique(c("", INDEX_RULES, pf("index_rule"))),
                   selected = pf("index_rule"), width = "100%",
                   options = list(create = TRUE,
                                  placeholder = "Which occurrence of the entry event indexes the cohort")),
    textInput(ns("concept_set"), "Concept set / codelist", pf("concept_set"), width = "100%",
              placeholder = "Name or reference of the codelist, e.g. cs_influenza_vaccine")
  ),
  textAreaInput(ns("inclusion_criteria"), "Inclusion / exclusion criteria (one per line)",
                join_lines(pf("inclusion_criteria", character(0))), rows = 3, width = "100%",
                placeholder = "Aged 18 or over at index\nNo prior insulin exposure")
)

cohort_definition_collect <- function(input) list(
  entry_events       = as_array(split_lines(input$entry_events)),
  index_rule         = blank_to_na(input$index_rule),
  inclusion_criteria = as_array(split_lines(input$inclusion_criteria)),
  exit_criteria      = as_array(split_lines(input$exit_criteria)),
  concept_set        = blank_to_na(input$concept_set)
)

# Shared validation ------------------------------------------------------------

# Every check here mirrors what generateDenominatorCohortSet() will actually
# accept: ages are finite pairs within 0..AGE_MAX, prior observation is a
# non-negative number of days, the date range runs forwards. Empty ageGroup /
# sex / daysPriorObservation are problems rather than silent defaults -- the
# form starts blank on purpose, and [] in the JSON must mean "not decided yet",
# never "meant the default but cannot be told apart from forgot".
validate_denominator_requirements <- function(p) {
  errs <- character(0)

  groups <- p$ageGroup %||% list()
  if (!length(groups)) {
    errs <- c(errs, "Age groups must have at least one interval; the generator's default is 0, 150.")
  }
  for (g in groups) {
    lo <- as.numeric(g[[1]])
    hi <- bound_upper(g)
    label <- format_bound_list(list(g))   # an open bound renders as Inf, not a fake 150
    if (is.infinite(hi)) {
      errs <- c(errs, sprintf(
        "Age group '%s' has no upper bound; the generator needs a finite age, at most %d.",
        label, AGE_MAX))
    } else if (hi < lo) {
      errs <- c(errs, sprintf("Age group '%s' has an upper bound below its lower bound.", label))
    }
    if (lo < 0 || (!is.infinite(hi) && hi > AGE_MAX)) {
      errs <- c(errs, sprintf("Age group '%s' must lie within 0 and %d.", label, AGE_MAX))
    }
  }

  if (!length(as.character(unlist(p$sex %||% character(0))))) {
    errs <- c(errs, "Sex must be at least one of Both, Male or Female.")
  }

  prior <- suppressWarnings(as.numeric(unlist(p$daysPriorObservation %||% numeric(0))))
  if (!length(prior)) {
    errs <- c(errs,
              "Days of prior observation must have at least one value; the generator's default is 0.")
  } else if (any(is.na(prior)) || any(prior < 0, na.rm = TRUE)) {
    errs <- c(errs, "Days of prior observation must be numbers of days, none negative.")
  }

  dr    <- p[["cohortDateRange"]]
  start <- date_bound(dr, 1)
  end   <- date_bound(dr, 2)
  if (!is.null(start) && !is.null(end) && as.Date(start) > as.Date(end)) {
    errs <- c(errs, sprintf("Cohort date range starts (%s) after it ends (%s).", start, end))
  }

  errs
}

# The kinds -------------------------------------------------------------------

register_cohort_kind(
  "denominator",
  hint = paste("Generated with generateDenominatorCohortSet(): a denominator cohort set",
               "built from the whole database, not defined by entry criteria."),
  # The uiOutput is the live preview of the set these requirements generate --
  # display-only (COHORT_DISPLAY_ONLY_IDS), filled by cohort_item_server().
  ui = function(ns, pf) tagList(
    denominator_requirements_ui(ns, pf),
    uiOutput(ns("cohort_set_preview"))
  ),
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
    # Starts EMPTY, like every other decision on the card: the validator asks
    # for at least one interval, and the preview shows what the generator's
    # default (0, Inf) would produce meanwhile.
    textAreaInput(ns("timeAtRisk"),
                  "Time at risk (one interval per line, days from target cohort entry)",
                  join_lines(format_bound_list(pf("timeAtRisk", list()))),
                  rows = 3, width = "100%", placeholder = "0, Inf\n0, 30\n31, 60"),
    denominator_requirements_ui(ns, pf),
    # Unticked until the author says otherwise -- the generator's own default
    # is TRUE, so the recorded FALSE is an explicit choice, same trade-off as
    # requirementInteractions.
    checkboxInput(ns("requirementsAtEntry"),
                  "Requirements must be met on the target cohort start date",
                  value = isTRUE(pf("requirementsAtEntry", FALSE)), width = "100%"),
    # The unticked mode is the semantically rich one, and it carries the
    # vignette's trap -- so it is said here, not only in the package docs.
    div(class = "form-text mb-2",
        paste("Unticked, people start contributing once they meet the requirements,",
              "even part-way through their time in the target cohort. Time at risk",
              "stays anchored on target entry either way: someone eligible only from",
              "day 31 contributes nothing to a 0–30 window.")),
    uiOutput(ns("cohort_set_preview"))
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
      # Both bounds are days FROM TARGET ENTRY, so a window cannot start before
      # day 0 -- the generator has no notion of time before entry.
      lo <- suppressWarnings(as.numeric(w[[1]]))
      if (is.na(lo) || lo < 0) {
        errs <- c(errs, sprintf("Time at risk '%s' must start at day 0 or later.",
                                format_bound_list(list(w))))
      } else if (bound_upper(w) < lo) {
        errs <- c(errs, sprintf("Time at risk '%s' ends before it starts.",
                                format_bound_list(list(w))))
      }
    }
    errs
  }
)

# The target cohort, per the a03 vignette: the population of interest -- people
# and the episodes during which they belong to it. A PLAIN cohort with its own
# hint: IncidencePrevalence never creates it, only points at it.
register_cohort_kind(
  "target",
  hint = paste("A pre-existing cohort of the population of interest — people and the",
               "episodes during which they belong to it. Instantiated outside",
               "IncidencePrevalence; a target denominator is generated FROM it,",
               "restricting person-time to those episodes."),
  ui = function(ns, pf) cohort_definition_ui(ns, pf),
  collect = function(input) cohort_definition_collect(input)
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
