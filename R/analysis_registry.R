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
#
# The two estimators this app generates code for, plus the generic form. The
# seven types offered until 0.4.24 (cohort characterisation, comparative cohort,
# self-controlled case series, case-control, survival analysis, patient-level
# prediction, drug utilisation) were labels and nothing else: none had a
# template, so every one rendered the "Other" block and serialised the same
# generic fields. Offering them implied this app could plan a study it has no
# estimator for -- each needs its own package (CohortCharacteristics,
# CohortSurvival, DrugUtilisation ...) before the label means anything.
ANALYSIS_TYPES <- c("Incidence", "Prevalence", "Other")

# The labels a template serialises through `serialised_type` must resolve back to
# its registry key. Without this, shiny::selectInput() drops a `selected` it cannot find
# in `choices`, the browser falls back to the first option, and the analysis
# silently changes type on load.
ANALYSIS_TYPE_ALIASES <- c(
  "estimatePointPrevalence"  = "Prevalence",
  "estimatePeriodPrevalence" = "Prevalence"
)

# Whether an analysis carries the study's conclusion or probes how robust it is.
#
# This is NOT an estimator argument, and that is exactly why it needs a field:
# "re-run with a 30-day washout" is a second call, not an argument to the first
# (which is why `sensitivity_analyses` was dropped from Incidence in 0.3.1 and
# from Prevalence in 0.4.0) -- but the DISTINCTION still has to be written down
# somewhere, because it is the first thing a reviewer looks for. A protocol
# states it in a column of its own; without a field, an author can only spell it
# into the analysis name, where nothing can group or check it.
#
# Unset is a real state, like the cohort kind and the analysis type: a new card
# starts with no role chosen rather than defaulting to one, and
# analysis_role_problems() reports a plan whose primary analysis is missing.
ANALYSIS_ROLES <- c("Primary" = "primary", "Sensitivity" = "sensitivity")

# The half of the card every analysis type shares, and the only keys load() lifts
# straight from the file without going through a template.
#
# `objectives` belongs here for the same reason the other three do: the shared
# half of the card renders it and collect() writes it. Leaving it out broke the
# mirror in one direction only -- the picker saved fine, but analysis_to_prefill()
# never handed it back, so LOADING a SAP rendered every analysis with no
# objectives and the next save wrote that emptiness over the file. A field the
# card owns must round-trip, or the app silently deletes it.
ANALYSIS_COMMON_FIELDS <- c("name", "analysis_type", "role", "data_sources", "objectives")

# Ids already taken by the common half and by item_card(). No template may reuse
# one, or the card would carry a duplicate input id.
RESERVED_INPUT_IDS <- c(ANALYSIS_COMMON_FIELDS, "remove", "duplicate", "box", "type_fields")

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
DISPLAY_ONLY_IDS <- c("denominator_summary", "cohort_role_notes", "denominatorCohortId_ui",
                      "outcomeCohortId_ui", "censorCohortId_ui")

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
# `x` arrives as a string when the tests reach here directly; a numericInput
# sends a number, or NA once the field is cleared.
parse_washout <- function(x, unbounded = FALSE) {
  if (isTRUE(unbounded)) return(as_num_array(Inf))
  x <- trimws(as.character(x %||% "")[1])         # a blank numericInput sends NA
  if (is.na(x) || !nzchar(x)) return(NULL)
  n <- suppressWarnings(as.numeric(x))
  if (is.na(n) || n < 0) NULL else as_num_array(n)
}

# The washout as a number, resolving a JSON null back to Inf. NULL means unset --
# the one state that is not a number.
washout_days <- function(w) {
  if (is.null(w) || !length(w)) return(NULL)
  v <- w[[1]]
  if (is.null(v)) return(Inf)                     # [null] -- an unbounded washout
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
#             ui() renders shiny::uiOutput(ns("<field>_ui")); the analyses module
#             fills it only when the picked cohort spans a set.
#   serialised_type  optional function(input) -> the analysis_type written to
#             the file when it is finer than the registry key (e.g. the
#             estimator planned). Every value it can return MUST appear in
#             ANALYSIS_TYPE_ALIASES, or the file would load as "Other";
#             flatten() sees the raw label under `analysis_type` and recovers
#             whatever inputs encode it.
#   flatten   function(params) -> prefill keys; the inverse of collect's nesting,
#             or identity if collect nests nothing. Also where an input id that
#             differs from its JSON key is reconciled.
#   validate  function(params, cohorts) -> character() of problems, where
#             `cohorts` is the named list from cohorts$by_name(). An analysis can
#             name a cohort nobody defined (the pickers allow free text), so look
#             one up with cohort_by_name() and handle NULL.
#   package   which library the generated estimator call comes from. Defaulted
#             rather than required, because every template so far is an
#             IncidencePrevalence estimator -- but declared here so one that is
#             not can say so, and the script header follows automatically.
register_analysis_template <- function(type, hint = NULL, ui, collect,
                                       pickers = list(), denominator = "denominator_cohort",
                                       subcohorts = list(), serialised_type = NULL,
                                       flatten = function(p) p,
                                       validate = function(params, cohorts) character(0),
                                       package = "IncidencePrevalence") {
  analysis_registry_env$ANALYSIS_TEMPLATES[[type]] <- list(
    hint = hint, ui = ui, collect = collect, pickers = pickers,
    denominator = denominator, subcohorts = subcohorts,
    serialised_type = serialised_type, flatten = flatten, validate = validate,
    package = package
  )
}

# The cohort IDs a "cohort set" spans: the picked cohort's own ID plus the ID of
# every cohort that names it as parent, labelled for a picker. A cohort without
# an ID contributes nothing -- there is no ID to reference. One ID is not a set:
# callers treat length < 2 as "no sub-cohorts".
#
# DEAD FOR NOW, and deliberately left whole: no cohort template collects
# `cohort_id` or `parent_cohort`, so against a cohort built in this app it always
# returns nothing and the sub-cohort pickers never render. That makes the three
# *CohortId arguments unreachable from the UI even though they are real estimator
# arguments -- a half-built feature, not a legacy path. Finish it by capturing an
# id on the cohort card, or delete the mechanism outright; either is a decision,
# not a cleanup.
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
#
# "" is the UNSET type: a new card starts with none chosen, because the type
# decides everything else on the card -- the same rule as the cohort kind.
# Unset renders a prompt, collects no parameters, and is a validation problem,
# never a silent Incidence.
canonical_analysis_type <- function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(x)) return("")
  if (x %in% names(ANALYSIS_TYPE_ALIASES)) ANALYSIS_TYPE_ALIASES[[x]] else x
}

analysis_template <- function(x) {
  tmpl <- ANALYSIS_TEMPLATES[[canonical_analysis_type(x)]]
  if (is.null(tmpl)) ANALYSIS_TEMPLATES[["Other"]] else tmpl
}

# "" for unset, exactly as canonical_analysis_type() does. A role the vocabulary
# does not know (a hand-edited file) is NOT coerced to a neighbour: it comes back
# as itself so the document shows what the file says and the problem is visible,
# rather than a plan silently reading as primary because "main" was typed.
canonical_analysis_role <- function(x) {
  if (length(x) != 1 || is.na(x) || !nzchar(x)) return("")
  as.character(x)
}

# A plan needs exactly one thing it concludes from.
#
# Every analysis unset is the normal state of a half-written SAP and says
# nothing; a plan that HAS roles but no primary among them is the case worth
# reporting, because it reads as complete while leaving the reader to guess which
# result is the answer. Two primaries is the same gap from the other side --
# legitimate when a study genuinely has two co-primary estimands, which is why it
# is a warning rather than a rule.
#
# Problems OF THE ANALYSES SECTION, shaped like the per-analysis entries in
# problems_r. Warn-not-block, like every other problem here.
analysis_role_problems <- function(analyses) {
  analyses <- analyses %||% list()
  if (!length(analyses)) return(list())
  roles <- vapply(analyses, function(a) canonical_analysis_role(a$role), character(1))
  known <- roles[nzchar(roles)]
  if (!length(known)) return(list())

  found <- list()
  unknown <- setdiff(unique(known), ANALYSIS_ROLES)
  if (length(unknown)) {
    found[[length(found) + 1]] <- list(
      name = "Analyses",
      messages = sprintf("'%s' is not a role this app knows; use %s.",
                         paste(unknown, collapse = "', '"),
                         paste(ANALYSIS_ROLES, collapse = " or ")))
  }
  n_primary <- sum(known == "primary")
  if (n_primary == 0) {
    found[[length(found) + 1]] <- list(
      name = "Analyses",
      messages = paste(
        "No analysis is marked primary. Every analysis here is a sensitivity",
        "analysis, so the plan states nothing to conclude from -- mark the one",
        "that carries the study's answer."))
  } else if (n_primary > 1) {
    found[[length(found) + 1]] <- list(
      name = "Analyses",
      messages = sprintf(paste(
        "%d analyses are marked primary. That is right only if the study has",
        "co-primary estimands; otherwise one of them is a sensitivity analysis."),
        n_primary))
  }
  found
}

# Shared blocks ---------------------------------------------------------------
#
# Templates that share a block share its input ids. That is safe because only one
# template is ever in the DOM, and it is what lets a time-at-risk window carry
# over when you switch between two types that both have one.

# A visible mini-heading above a group of related fields. The groupings used to
# exist only as code comments -- invisible in the rendered card.
section_heading <- function(text) {
  shiny::div(class = "text-muted small fw-semibold text-uppercase mt-3 mb-2", text)
}

tar_ui <- function(ns, pf) shiny::tagList(
  shiny::tags$label(class = "form-label fw-semibold", "Time at risk"),
  bslib::layout_columns(
    col_widths = c(3, 3, 3, 3),
    shiny::numericInput(ns("tar_start_offset"), "Start (days)", value = pf("tar_start_offset", 0), width = "100%"),
    shiny::selectInput(ns("tar_start_anchor"), "Anchored on", ANCHORS,
                selected = pf("tar_start_anchor", ANCHORS[1]), width = "100%"),
    shiny::numericInput(ns("tar_end_offset"), "End (days)", value = pf("tar_end_offset", 0), width = "100%"),
    shiny::selectInput(ns("tar_end_anchor"), "Anchored on", ANCHORS,
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

strat_ui <- function(ns, pf) bslib::layout_columns(
  col_widths = c(6, 6),
  shiny::textAreaInput(ns("stratifications"), "Stratifications (one per line)",
                join_lines(pf("stratifications", character(0))), rows = 4, width = "100%",
                placeholder = "Sex\n10-year age bands"),
  shiny::textAreaInput(ns("sensitivity_analyses"), "Sensitivity analyses (one per line)",
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
strata_ui <- function(ns, pf) shiny::tagList(
  shiny::selectizeInput(
    ns("strata"), "Strata", choices = character(0),
    selected = strata_tokens(pf("strata", list())), multiple = TRUE, width = "100%",
    options = list(create = TRUE,
                   placeholder = "Columns on the denominator cohort; comma to cross (sex, age_group)")
  ),
  # Inert without strata -- the estimators then return only the overall
  # estimate -- so the choice is offered only when strata exist. The checkbox
  # keeps its value while hidden, so strata removed and re-added get the
  # user's earlier choice back.
  shiny::conditionalPanel(
    condition = sprintf("(input['%s'] || []).length > 0", ns("strata")),
    shiny::checkboxInput(ns("include_overall_strata"), "Also report an overall (unstratified) result",
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
  shiny::uiOutput(ns("denominator_summary"))
}

# cohort is the cohorts$by_name() entry for whatever the denominator picker
# holds, or NULL when it names a cohort that has not been written down --
# `picked` is the raw picker value, because an EMPTY pick and a DANGLING pick
# are different states: nothing chosen yet deserves a prompt, not an
# accusation about a cohort that was never named.
denominator_summary <- function(cohort, picked = NULL) {
  if (is.null(cohort)) {
    picked <- trimws(as.character(picked %||% "")[1])
    if (is.na(picked) || !nzchar(picked)) {
      return(shiny::p(class = "text-muted small mb-3",
               "Pick a denominator cohort to see what it fixes and the cohort set
                this analysis runs over."))
    }
    return(shiny::div(
      class = "alert alert-warning py-2 small mb-3",
      sprintf("'%s' is not defined on the Cohorts tab, so nothing can be inherited from it.",
              picked)
    ))
  }
  if (!is_denominator_kind(cohort$kind)) {
    return(shiny::div(
      class = "alert alert-warning py-2 small mb-3",
      sprintf(paste("'%s' is not a denominator cohort, so it fixes no study period, age groups,",
                    "sex or time at risk. Set its kind on the Cohorts tab."),
              cohort$name %||% "This cohort")
    ))
  }
  denominator_panel(cohort, "Inherited from this cohort -- set it on the Cohorts tab:")
}

# The outcome and censoring slots, checked against the kind of the cohort each
# names. A HINT, not a validate() problem, and the distinction is the whole
# design: crossing the two slots is usually a slip, but it is legitimately how a
# real plan reads -- death is the outcome of one analysis and the censoring event
# of the next, and whichever kind that one cohort carries, the other use looks
# wrong. A problem would block a save on a correct plan; a hint sits on the card
# and can be ignored.
#
# So this fires ONLY on a definite crossing, where the cohort is labelled as the
# other slot's role. An `other`, `target` or kindless cohort in either slot says
# nothing -- a plain cohort reused as an outcome is ordinary, and the kind was
# never a promise about which analysis would consume it. What is unambiguously
# wrong (a generated denominator in either slot) stays in validate(), where it
# blocks.
cohort_role_notes_ui <- function(ns, pf) {
  shiny::uiOutput(ns("cohort_role_notes"))
}

cohort_role_notes <- function(outcome, censor = NULL) {
  crossed <- function(cohort, is_kind, slot, labelled) {
    if (is.null(cohort)) return(NULL)
    if (!identical(canonical_cohort_kind(cohort$kind), is_kind)) return(NULL)
    sprintf(paste("'%s' is the %s here, but its kind is %s. That is fine if the",
                  "same cohort plays both parts -- otherwise check the pickers."),
            cohort$name %||% "This cohort", slot, labelled)
  }
  notes <- c(crossed(outcome, "censor",  "outcome",   "Censoring"),
             crossed(censor,  "outcome", "censoring cohort", "Outcome"))
  if (!length(notes)) return(NULL)
  shiny::div(class = "alert alert-warning py-2 small mb-3",
      lapply(notes, function(n) shiny::div(n)))
}

# The panel itself: the facts grid plus the generated cohort set, one styled
# block. Shared by the analysis card's summary above and the cohort card's own
# preview (mod_cohorts.R), so the two views can never disagree -- only the
# sentences differ. `intro` sits above the facts; `lead` is passed through to
# denominator_cohort_set_ui().
denominator_panel <- function(cohort, intro, lead = NULL) {
  fact <- function(label, value) {
    shiny::div(class = "col", shiny::tags$span(class = "text-muted", label), shiny::tags$br(), shiny::tags$strong(value))
  }
  none <- function(x) {
    x <- as.character(unlist(x %||% character(0)))
    if (!length(x) || !any(nzchar(x))) "--" else paste(x, collapse = ", ")
  }
  # Never unlist() a bound pair -- a JSON null upper bound would collapse.
  bounds <- function(pairs, open = "Inf") {
    if (!length(pairs)) return("--")
    paste(format_bound_list(pairs, open = open), collapse = " | ")
  }
  shiny::div(
    class = "border rounded bg-body-tertiary p-3 mb-3 small",
    shiny::div(class = "text-muted mb-2", intro),
    shiny::div(
      class = "row row-cols-auto gap-3",
      # cohortDateRange is one key holding two dates, either of which may be null.
      # date_bound() indexes it; unlist() would collapse a null and shift the pair.
      fact("Cohort date range", {
        dr    <- cohort[["cohortDateRange"]]
        start <- date_bound(dr, 1)
        end   <- date_bound(dr, 2)
        if (is.null(start) && is.null(end)) "--"
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
    denominator_cohort_set_ui(cohort, lead = lead)
  )
}

# The axes above are a cohort SET, and the analysis runs on every cohort in it --
# three age groups and two sexes is six cohorts, not one. Spelling them out is the
# point: it is the only place the author sees what the arguments they typed will
# actually generate, and how fast it multiplies.
#
# EVERY cohort is listed. There used to be a cap, with the remainder reported as
# "... and N more not listed here", because an uncapped list ran the card off the
# page. The scroll container below removed that constraint, and a truncated list
# was the weaker answer anyway: the whole reason for spelling the set out is to
# show what the arguments generate, and a reader who cannot see the last entry
# cannot check it.

# `lead` is a function(n) -> the sentence above the list, because the sentence
# differs by context: the analysis card says "the analysis runs on all of them",
# while the cohort card (which may predate any analysis) says what the
# requirements will generate. NULL keeps the analysis-card wording.
denominator_cohort_set_ui <- function(cohort, lead = NULL) {
  set <- denominator_cohort_set(cohort)
  n   <- length(set)
  msg <- if (is.null(lead)) {
    sprintf("This cohort set generates %d cohort%s, and the analysis runs on %s:",
            n, if (n == 1) "" else "s", if (n == 1) "it" else "all of them")
  } else {
    lead(n)
  }
  shiny::div(
    class = "mt-3",
    shiny::div(class = "text-muted mb-1", msg),
    # The list is BOUNDED AND SCROLLED, not laid out in full. The cap alone was
    # not enough: a correct denominator multiplies fast -- 15 age groups x 3
    # sexes x 3 prior-observation values is 135 cohorts -- so even the capped 100
    # ran the card past the bottom of the layout column. Scrolling keeps the
    # whole list reachable while the card stays the size of a card. Its own
    # border and background say the region scrolls; `overflow: auto` also catches
    # a long line rather than pushing the column sideways.
    shiny::div(
      style = "max-height: 18rem; overflow: auto;",
      class = "border rounded bg-body px-2 py-1",
      # ps-5, not ps-3. A list marker sits in the OL's left padding, so a padding
      # narrower than the marker pushes it outside the content box -- which the
      # scroll container above then clips. Now that the list is uncapped the
      # marker can reach four digits, which ps-5 still clears in this font size.
      shiny::tags$ol(
        class = "mb-0 ps-5 font-monospace",
        lapply(set, function(x) shiny::tags$li(format_denominator_cohort(x)))
      )
    )
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
  # The form starts with no sex chosen, so an unset sex is an EMPTY list, which
  # %||% does not catch. Unset means the generator's own default applies: "Both",
  # one cohort holding both sexes, which CAN be split.
  sex      <- as.character(unlist(cohort$sex %||% character(0)))
  if (!length(sex)) sex <- "Both"
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
