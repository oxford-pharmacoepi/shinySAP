# Analysis type registry ------------------------------------------------------
#
# Each analysis type can have its own set of inputs. This file holds the registry
# and the blocks templates share; the templates themselves are one file each,
# R/analysis_type_<name>.R.
#
# Shiny sources R/ in C-locale alphabetical order into one shared environment and
# does NOT recurse into subdirectories (loadSupport(): recursive = FALSE) -- so
# template files must sit flat in R/, and this file must sort before them.
# "analysis_registry.R" < "analysis_type_*.R" because 'r' < 't'.
#
# It also sorts before R/dynamic_items.R (entity_picker), R/utils.R (`%||%`) and
# app.R (which runs library(bslib), so layout_columns). None of those exist yet
# while this file is being sourced, so nothing here may call them at the top
# level. Inside a function body they are fine -- that runs later.

# The dropdown. The registry is deliberately partial: a type with no template
# falls back to "Other", so adding one later never means touching this vector.
ANALYSIS_TYPES <- c(
  "Cohort characterisation", "Incidence", "Prevalence",
  "Comparative cohort", "Self-controlled case series", "Case-control",
  "Survival analysis", "Patient-level prediction", "Drug utilisation", "Other"
)

# Types renamed after SAPs had already been saved under the old label, plus the
# labels templates serialise through `serialised_type` -- both must resolve back
# to their registry key. Without this, selectInput() drops a `selected` it
# cannot find in `choices`, the browser falls back to the first option, and the
# analysis silently changes type.
ANALYSIS_TYPE_ALIASES <- c(
  "Incidence rate"           = "Incidence",
  "estimatePointPrevalence"  = "Prevalence",
  "estimatePeriodPrevalence" = "Prevalence"
)

# The half of the card every analysis type shares, and the only keys load() lifts
# straight from the file without going through a template.
ANALYSIS_COMMON_FIELDS <- c("name", "analysis_type", "data_sources")

# Ids already taken by the common half and by item_card(). No template may reuse
# one, or the card would carry a duplicate input id.
RESERVED_INPUT_IDS <- c(ANALYSIS_COMMON_FIELDS, "remove", "box", "type_fields")

ANCHORS           <- c("cohort start", "cohort end")
DENOMINATOR_UNITS <- c("person-years", "person-months", "person-days")

# IncidencePrevalence::estimate*Prevalence() argument domains. The two estimators
# accept different intervals -- "overall" is period-only -- so each prevalence
# type gets its own interval vector. timePoint is point-only; level,
# fullContribution and completeDatabaseIntervals are period-only.
PREVALENCE_TYPES            <- c("Point prevalence", "Period prevalence")
POINT_PREVALENCE_INTERVALS  <- c("weeks", "months", "quarters", "years")
PERIOD_PREVALENCE_INTERVALS <- c(POINT_PREVALENCE_INTERVALS, "overall")
PREVALENCE_TIMEPOINTS       <- c("start", "middle", "end")
PREVALENCE_LEVELS           <- c("person", "record")
# Ids a template renders that are outputs, not inputs: they hold no value, so
# they are exempt from the collect/flatten round-trip check in the tests. The
# *_ui ids are the subcohort placeholders (see `subcohorts` below).
DISPLAY_ONLY_IDS <- c("denominator_summary", "denominatorCohortId_ui", "outcomeCohortId_ui",
                      "censorCohortId_ui")

# IncidencePrevalence::estimateIncidence(interval =): more than one may be given,
# and results are estimated for each.
INTERVALS <- c("weeks", "months", "quarters", "years", "overall")

# estimateIncidence(outcomeWashout =) is a NUMBER of days, defaulting to Inf. So
# the SAP holds a number -- but three states have to stay apart:
#
#   unset            the author never said. The validator objects; see below.
#   a number of days including 0, which is a substantively different analysis.
#   Inf              the API's own default: one event per person, ever.
#
# JSON has no Infinity, so the washout travels as a ONE-ELEMENT NUMERIC ARRAY,
# exactly the convention age_groups and time_at_risk already use (R/cohort_kinds.R):
# Inf is null *inside* an array, while a bare null keeps its schema-wide meaning of
# "absent".
#
#   [365]   365 days        [0]  no washout
#   [null]  Inf, unbounded  null the author never said
#
# So never unlist() a washout either -- [null] would collapse to length 0. Read it
# with washout_days(), which resolves the null back to Inf.

# The washout is captured as a NUMBER of days -- any number, not a menu of
# preset ones. But a number field cannot hold Inf, and blanking it has to keep
# meaning "never stated", so "unbounded" gets its own checkbox alongside it. The
# two inputs together carry the three states:
#
#   box clear, field blank  -> NULL     the author never said; validate() objects
#   box clear, field n      -> [n]      n days, 0 included
#   box ticked              -> [null]    Inf, whatever is in the field
#
# The field must start EMPTY rather than at some default. estimateIncidence()
# defaults to Inf, but a washout is too consequential to inherit silently -- a SAP
# has to say it out loud, which is the whole point of the "no safe default" rule.

# The two inputs -> the JSON value. The checkbox wins: a ticked box is Inf no
# matter what the number field happens to still hold.
#
# `x` may also be a string, which is how an old file and the tests reach here --
# "365", or the pre-0.3.2 "unbounded" sentinel.
parse_washout <- function(x, unbounded = FALSE) {
  if (isTRUE(unbounded)) return(as_num_array(Inf))
  x <- trimws(as.character(x %||% "")[1])         # a blank numericInput sends NA
  if (is.na(x) || !nzchar(x)) return(NULL)
  if (tolower(x) %in% c("inf", "infinity", "unbounded")) return(as_num_array(Inf))
  n <- suppressWarnings(as.numeric(x))
  if (is.na(n) || n < 0) NULL else as_num_array(n)
}

# The washout as a number, resolving a JSON null back to Inf. NULL means unset --
# the one state that is not a number. Also reads the pre-0.3.2 shapes, so the
# validator and the form agree about what an old file says: the "unbounded"
# sentinel string, and a bare number rather than a one-element array.
washout_days <- function(w) {
  if (is.null(w) || !length(w)) return(NULL)
  v <- w[[1]]
  if (is.null(v)) return(Inf)                     # [null] -- an unbounded washout
  if (is.character(v) && tolower(v) %in% c("unbounded", "inf", "infinity")) return(Inf)
  n <- suppressWarnings(as.numeric(v))
  if (is.na(n)) NULL else n
}

washout_is_unbounded <- function(w) {
  d <- washout_days(w)
  !is.null(d) && is.infinite(d)
}

# The JSON value -> the number field, for pf() on the way back in. NULL leaves the
# field blank -- which is right for BOTH an unset washout and an unbounded one,
# since a number field cannot show Inf and the checkbox carries that instead.
washout_days_value <- function(w) {
  d <- washout_days(w)
  if (is.null(d) || is.infinite(d)) NULL else d
}

format_washout <- function(w) {
  d <- washout_days(w)
  if (is.null(d)) return("not stated")
  if (is.infinite(d)) return("unbounded")
  paste(d, "days")
}

# The registry ----------------------------------------------------------------
#
# Populated at source time by the analysis_type_*.R files. Lookup is by key, so
# the order they register in does not matter. The registry lives wherever this
# file was sourced; register_analysis_template() updates it there explicitly.
ANALYSIS_TEMPLATES <- list()
analysis_registry_env <- environment()

# A template is a set of pieces that mirror one another, so a field cannot be
# added to the form without also being serialised and read back:
#
#   hint      one line shown above the block, or NULL
#   ui        function(ns, pf) -> the type's inputs
#   collect   function(input) -> the type's JSON, reading ONLY its own input ids
#   pickers   input ids that pick a cohort or a CDM source, by entity
#   denominator  the input id naming the denominator cohort; the strata picker
#             and the denominator summary are driven from it
#   subcohorts multi-selects of cohort IDs, keyed to one of the template's
#             cohort pickers: field id -> list(from, label). The template's
#             ui() renders uiOutput(ns("<field>_ui")); the analyses module
#             fills it only when the picked cohort spans a set.
#   serialised_type  optional function(input) -> the analysis_type written to
#             the file when it is finer than the registry key (e.g. the
#             estimator planned). Every value it can return MUST appear in
#             ANALYSIS_TYPE_ALIASES, or the file would load as "Other";
#             flatten() sees the raw label under `analysis_type` and recovers
#             whatever inputs encode it.
#   flatten   function(params) -> prefill keys; the inverse of collect's nesting,
#             or identity if collect nests nothing. Also the place to migrate an
#             older file's keys onto the current inputs.
#   validate  function(params, cohorts) -> character() of problems, where
#             `cohorts` is the named list from cohorts$by_name(). An analysis can
#             name a cohort nobody defined (the pickers allow free text), so look
#             one up with cohort_by_name() and handle NULL.
register_analysis_template <- function(type, hint = NULL, ui, collect,
                                       pickers = list(), denominator = "denominator_cohort",
                                       subcohorts = list(), serialised_type = NULL,
                                       flatten = function(p) p,
                                       validate = function(params, cohorts) character(0)) {
  analysis_registry_env$ANALYSIS_TEMPLATES[[type]] <- list(
    hint = hint, ui = ui, collect = collect, pickers = pickers,
    denominator = denominator, subcohorts = subcohorts,
    serialised_type = serialised_type, flatten = flatten, validate = validate
  )
}

# The cohort IDs a "cohort set" spans: the picked cohort's own ID plus the ID of
# every cohort that names it as parent, labelled for a picker. A cohort without
# an ID contributes nothing -- there is no ID to reference. One ID is not a set:
# callers treat length < 2 as "no sub-cohorts".
#
# `parent_cohort` is no longer captured on the cohort card, so only an older SAP
# still carries it; a cohort built now matches solely by its own name. The read is
# kept so those older files still resolve their sub-cohort selections.
subcohort_choices <- function(parent_name, cohorts) {
  parent_name <- as.character(parent_name %||% "")
  if (!nzchar(parent_name)) return(numeric(0))
  out <- numeric(0)
  for (ch in cohorts) {
    id <- ch$cohort_id
    if (is.null(id) || length(id) != 1 || is.na(id)) next
    nm  <- as.character(ch$name %||% "")
    par <- as.character(ch$parent_cohort %||% "")
    if (identical(nm, parent_name) || identical(par, parent_name))
      out[sprintf("%s (%s)", nm, format(id))] <- as.numeric(id)
  }
  out
}

# NULL, NA and "" must all resolve rather than error: ANALYSIS_TEMPLATES[[NULL]]
# throws in R (it does not return NULL), and an analysis can be saved with no
# analysis_type. load() clears the section before repopulating it, so an error
# here would wipe the user's analyses.
canonical_analysis_type <- function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(x)) return(ANALYSIS_TYPES[1])
  if (x %in% names(ANALYSIS_TYPE_ALIASES)) ANALYSIS_TYPE_ALIASES[[x]] else x
}

analysis_template <- function(x) {
  tmpl <- ANALYSIS_TEMPLATES[[canonical_analysis_type(x)]]
  if (is.null(tmpl)) ANALYSIS_TEMPLATES[["Other"]] else tmpl
}

# Shared blocks ---------------------------------------------------------------
#
# Templates that share a block share its input ids. That is safe because only one
# template is ever in the DOM, and it is what lets a time-at-risk window carry
# over when you switch between two types that both have one.

tar_ui <- function(ns, pf) tagList(
  tags$label(class = "form-label fw-semibold", "Time at risk"),
  layout_columns(
    col_widths = c(3, 3, 3, 3),
    numericInput(ns("tar_start_offset"), "Start (days)", value = pf("tar_start_offset", 0), width = "100%"),
    selectInput(ns("tar_start_anchor"), "Anchored on", ANCHORS,
                selected = pf("tar_start_anchor", ANCHORS[1]), width = "100%"),
    numericInput(ns("tar_end_offset"), "End (days)", value = pf("tar_end_offset", 0), width = "100%"),
    selectInput(ns("tar_end_anchor"), "Anchored on", ANCHORS,
                selected = pf("tar_end_anchor", ANCHORS[2]), width = "100%")
  )
)

tar_collect <- function(input) list(
  time_at_risk = list(
    start_offset_days = input$tar_start_offset %||% NA,
    start_anchor      = input$tar_start_anchor,
    end_offset_days   = input$tar_end_offset %||% NA,
    end_anchor        = input$tar_end_anchor
  )
)

# Assigning NULL drops a key, which is what pf() reads as "absent".
tar_flatten <- function(p) {
  tar <- p$time_at_risk
  p$tar_start_offset <- tar$start_offset_days
  p$tar_start_anchor <- tar$start_anchor
  p$tar_end_offset   <- tar$end_offset_days
  p$tar_end_anchor   <- tar$end_anchor
  p
}

strat_ui <- function(ns, pf) layout_columns(
  col_widths = c(6, 6),
  textAreaInput(ns("stratifications"), "Stratifications (one per line)",
                join_lines(pf("stratifications", character(0))), rows = 4, width = "100%",
                placeholder = "Sex\n10-year age bands"),
  textAreaInput(ns("sensitivity_analyses"), "Sensitivity analyses (one per line)",
                join_lines(pf("sensitivity_analyses", character(0))), rows = 4, width = "100%",
                placeholder = "30-day washout")
)

strat_collect <- function(input) list(
  stratifications      = as_array(split_lines(input$stratifications)),
  sensitivity_analyses = as_array(split_lines(input$sensitivity_analyses))
)

# Structured strata, for templates that map onto IncidencePrevalence -----------
#
# `strata` there is a list of *variable groups*, naming columns on the
# denominator cohort table: list("sex", c("sex", "age_group")) means one
# stratification by sex and another by the cross of sex and age group. A flat
# one-per-line textarea cannot say that, so each token in this multi-select is
# one stratification, and a comma inside a token crosses its variables.
#
# The choices are the columns the *selected denominator cohort* carries, so this is
# a picker: a template must declare it under pickers$strata, and
# analysis_item_server() keeps it in step with the denominator. Those columns are
# fixed (STRATA_VARIABLES) -- a cohort does not declare them.
strata_ui <- function(ns, pf) tagList(
  selectizeInput(
    ns("strata"), "Strata", choices = character(0),
    selected = strata_tokens(pf("strata", list())), multiple = TRUE, width = "100%",
    options = list(create = TRUE,
                   placeholder = "Columns on the denominator cohort; comma to cross (sex, age_group)")
  ),
  # Inert without strata -- the estimators then return only the overall
  # estimate -- so the choice is offered only when strata exist. The checkbox
  # keeps its value while hidden, so strata removed and re-added get the
  # user's earlier choice back.
  conditionalPanel(
    condition = sprintf("(input['%s'] || []).length > 0", ns("strata")),
    checkboxInput(ns("include_overall_strata"), "Also report an overall (unstratified) result",
                  value = isTRUE(pf("include_overall_strata", TRUE)), width = "100%")
  )
)

# Selectize tokens -> the JSON's list of groups. "sex, age_group" is one group.
parse_strata <- function(x) {
  toks <- trimws(as.character(unlist(x %||% character(0))))
  toks <- toks[nzchar(toks)]
  lapply(toks, function(tok) {
    vars <- trimws(unlist(strsplit(tok, ",", fixed = TRUE)))
    as_array(vars[nzchar(vars)])
  })
}

# The JSON's list of groups -> selectize tokens, for pf() on the way back in.
strata_tokens <- function(groups) {
  if (!length(groups)) return(character(0))
  vapply(groups, function(g) paste(as.character(unlist(g)), collapse = ", "), character(1))
}

# The columns a denominator cohort actually carries, which is what an analysis
# built on it may stratify by.
#
# Not a field on the cohort: generateDenominatorCohortSet() produces age_group and
# sex and nothing else, so there is nothing for an author to decide. Anything that
# is not a denominator carries no strata columns at all -- an analysis on it is
# already wrong for a bigger reason, which its own validator reports.
cohort_strata_variables <- function(cohort) {
  if (is.null(cohort) || !is_denominator_kind(cohort$kind)) return(character(0))
  STRATA_VARIABLES
}

# A read-only echo of what the chosen denominator already fixes, so nobody
# re-specifies a study period or an age band that the cohort has already
# decided. Unlike the other shared blocks this one owns an *output*, not inputs:
# it must react to the denominator picker, which the static ui() cannot. The
# item server fills it (see analysis_item_server), so a template gets the block
# just by dropping this placeholder into its ui().
denominator_summary_ui <- function(ns, pf) {
  uiOutput(ns("denominator_summary"))
}

# cohort is the cohorts$by_name() entry for whatever the denominator picker
# holds, or NULL when it names a cohort that has not been written down.
denominator_summary <- function(cohort) {
  if (is.null(cohort)) {
    return(div(
      class = "alert alert-warning py-2 small mb-3",
      "This cohort is not defined on the Cohorts tab, so nothing can be inherited from it."
    ))
  }
  if (!is_denominator_kind(cohort$kind)) {
    return(div(
      class = "alert alert-warning py-2 small mb-3",
      sprintf(paste("'%s' is not a denominator cohort, so it fixes no study period, age groups,",
                    "sex or time at risk. Set its kind on the Cohorts tab."),
              cohort$name %||% "This cohort")
    ))
  }
  fact <- function(label, value) {
    div(class = "col", tags$span(class = "text-muted", label), tags$br(), tags$strong(value))
  }
  none <- function(x) {
    x <- as.character(unlist(x %||% character(0)))
    if (!length(x) || !any(nzchar(x))) "—" else paste(x, collapse = ", ")
  }
  # Never unlist() a bound pair -- a JSON null upper bound would collapse.
  bounds <- function(pairs, open = "Inf") {
    if (!length(pairs)) return("—")
    paste(format_bound_list(pairs, open = open), collapse = " | ")
  }
  div(
    class = "border rounded bg-body-tertiary p-3 mb-3 small",
    div(class = "text-muted mb-2", "Inherited from this cohort — set it on the Cohorts tab:"),
    div(
      class = "row row-cols-auto gap-3",
      # cohortDateRange is one key holding two dates, either of which may be null.
      # date_bound() indexes it; unlist() would collapse a null and shift the pair.
      fact("Cohort date range", {
        dr    <- cohort[["cohortDateRange"]]
        start <- date_bound(dr, 1)
        end   <- date_bound(dr, 2)
        if (is.null(start) && is.null(end)) "—"
        else paste(none(start), "to", none(end))
      }),
      fact("Age groups", bounds(cohort$ageGroup, open = AGE_MAX)),
      fact("Sex", none(cohort$sex)),
      fact("Prior observation", paste(none(cohort$daysPriorObservation), "days")),
      # Only a target denominator has one; a plain denominator contributes all
      # observed time, so there is nothing to show.
      if (identical(canonical_cohort_kind(cohort$kind), "target_denominator"))
        fact("Time at risk (days from target entry)", bounds(cohort$timeAtRisk))
    ),
    denominator_cohort_set_ui(cohort)
  )
}

# The axes above are a cohort SET, and the analysis runs on every cohort in it --
# three age groups and two sexes is six cohorts, not one. Spelling them out is the
# point: it is the only place the author sees what the arguments they typed will
# actually generate, and how fast it multiplies.
DENOMINATOR_SET_SHOWN <- 100

denominator_cohort_set_ui <- function(cohort) {
  set <- denominator_cohort_set(cohort)
  n   <- length(set)
  # Never silently truncate: say what was dropped. Only a pathological cohort gets
  # near this, but a 500-line card would be unreadable and unscrollable.
  shown  <- utils::head(set, DENOMINATOR_SET_SHOWN)
  hidden <- n - length(shown)
  div(
    class = "mt-3",
    div(class = "text-muted mb-1",
        sprintf("This cohort set generates %d cohort%s, and the analysis runs on %s:",
                n, if (n == 1) "" else "s", if (n == 1) "it" else "all of them")),
    tags$ol(
      class = "mb-0 ps-3 font-monospace",
      lapply(shown, function(x) tags$li(format_denominator_cohort(x)))
    ),
    if (hidden > 0)
      div(class = "text-muted fst-italic mt-1",
          sprintf("… and %d more not listed here.", hidden))
  )
}

# Two things can be wrong with a stratification, and they are different:
#
#   1. The variable is not a column on the denominator cohort at all, so
#      estimateIncidence(strata = ) would fail outright.
#   2. The column exists but the cohort has already collapsed it -- a male-only
#      cohort has no sex left to vary, a single-age-band cohort no age. The call
#      would succeed and return a stratification of one level, which is not the
#      analysis anyone meant.
#
# `groups` is the JSON shape: a list of variable groups. Returns character(0)
# when there is nothing to object to, including when the cohort is unknown --
# that is denominator_summary()'s problem to report, not this one's.
validate_strata_against <- function(groups, cohort) {
  if (!length(groups) || is.null(cohort)) return(character(0))
  errs     <- character(0)
  declared <- cohort_strata_variables(cohort)
  sex      <- as.character(unlist(cohort$sex %||% "Both"))
  n_ages   <- length(cohort$ageGroup %||% list())

  for (v in unique(as.character(unlist(groups)))) {
    if (!v %in% declared) {
      errs <- c(errs, sprintf(
        "Cannot stratify by '%s': the denominator cohort does not carry that column (it has %s).",
        v, if (length(declared)) paste(declared, collapse = ", ") else "none"))
      next
    }
    # sex = "Both" generates one cohort holding both sexes, which can be split.
    # sex = c("Male", "Female") generates two single-sex cohorts, neither of which
    # has a sex left to vary.
    if (identical(v, "sex") && !"Both" %in% sex) {
      errs <- c(errs, sprintf(
        "Cannot stratify by sex: the denominator cohort is restricted to %s.",
        paste(sex, collapse = " and ")))
    }
    if (identical(v, "age_group") && n_ages < 2) {
      errs <- c(errs,
        "Cannot stratify by age_group: the denominator cohort defines fewer than two age groups.")
    }
  }
  unique(errs)
}

# Every input id a template's ui() creates, recovered by handing it a namespace
# that records instead of namespacing. Saves maintaining the id list by hand in a
# third place; the tests use it to check for collisions.
template_field_ids <- function(tmpl) {
  rec <- new.env(parent = emptyenv())
  rec$ids <- character(0)
  rec_ns <- function(x) {
    rec$ids <- c(rec$ids, x)
    x
  }
  tmpl$ui(rec_ns, function(key, default = NULL) default)
  unique(rec$ids)
}
