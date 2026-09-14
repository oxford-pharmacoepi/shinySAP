# Analysis type registry ------------------------------------------------------
#
# Each analysis type -- one of the schema's analysis types -- has its own
# parameter block. This file holds the registry and the blocks templates share;
# the templates themselves are one file each, R/analysis_type_<name>.R.
#
# Shiny sources R/ in C-locale alphabetical order into one shared environment and
# does NOT recurse into subdirectories -- so template files must sit flat in R/,
# and this file must sort before them ("analysis_registry.R" < "analysis_type_*.R").
# It also sorts before cohort_kinds.R, dynamic_items.R, problems.R and utils.R,
# so nothing here may call them -- or shinySAP:: -- at the top level. Inside a
# function body they are fine.

# The half of the card every analysis shares; everything else is `parameters`.
ANALYSIS_COMMON_FIELDS <- c("id", "name", "data_source_id", "type")

# Ids already taken by the common half and by item_card(). No template may reuse
# one, or the card would carry a duplicate input id.
RESERVED_INPUT_IDS <- c(ANALYSIS_COMMON_FIELDS, "remove", "duplicate", "box", "type_fields")

# Ids a template renders that are outputs, not inputs: they hold no value, so
# they are exempt from the collect/flatten round-trip check in the tests.
DISPLAY_ONLY_IDS <- c("denominator_summary")

# Unbounded day counts ----------------------------------------------------------
#
# Several estimator arguments are a number of days defaulting to Inf (washout,
# follow-up). A number field cannot hold Inf, and blanking it has to keep
# meaning "never stated", so "unbounded" gets a checkbox beside it:
#
#   box clear, field blank  -> NULL    the author never said
#   box clear, field n      -> n       n days, 0 included
#   box ticked              -> Inf     whatever the field still holds
#
# The SAP holds the number (Inf included; the package's JSON layer writes Inf as
# absent, which the estimator reads as its own default).
parse_unbounded_days <- function(x, unbounded = FALSE) {
  if (isTRUE(unbounded)) return(Inf)
  n <- num_or_null(x)
  if (is.null(n) || n < 0) NULL else n
}

# The inverse, for pf(): the number field shows a finite value, the checkbox
# carries Inf. `p[key] <- list(NULL)` keeps the key with a NULL value; `p$key <-
# NULL` would delete it and `$` partial matching would then find the _unbounded
# key instead.
unbounded_days_prefill <- function(p, key) {
  v <- p[[key]]
  p[[paste0(key, "_unbounded")]] <- !is.null(v) && is.infinite(v)
  p[key] <- list(if (is.null(v) || is.infinite(v)) NULL else v)
  p
}

# One number-plus-checkbox pair, as the templates render it.
unbounded_days_ui <- function(ns, pf, key, label, unbounded_label) {
  shiny::div(
    shiny::numericInput(ns(key), label, value = pf(key, NULL), min = 0, step = 1, width = "100%"),
    shiny::checkboxInput(ns(paste0(key, "_unbounded")), unbounded_label,
                         value = isTRUE(pf(paste0(key, "_unbounded"), FALSE)))
  )
}

# The registry ----------------------------------------------------------------

ANALYSIS_TEMPLATES <- list()
analysis_registry_env <- environment()

# A template is a set of pieces that mirror one another, so a field cannot be
# added to the form without also being serialised and read back:
#
#   hint        one line shown above the block, or NULL
#   ui          function(ns, pf) -> the type's inputs; input ids ARE the schema
#               parameter names
#   collect     function(input) -> the type's `parameters`, reading ONLY its own
#               input ids, omitting anything unauthored
#   pickers     input ids that pick another item, by entity: list(cohorts =,
#               strata =)
#   denominator the input id naming the denominator cohort; the strata picker
#               and the denominator summary are driven from it (NULL if none)
#   flatten     function(parameters) -> prefill keys; the inverse of collect()
#   package     which library the generated estimator call comes from
register_analysis_template <- function(type_id, hint = NULL, ui, collect,
                                       pickers = list(), denominator = NULL,
                                       flatten = function(p) p,
                                       package = "IncidencePrevalence") {
  analysis_registry_env$ANALYSIS_TEMPLATES[[type_id]] <- list(
    hint = hint, ui = ui, collect = collect, pickers = pickers,
    denominator = denominator, flatten = flatten, package = package
  )
}

EMPTY_ANALYSIS_TEMPLATE <- list(
  hint = "This analysis type has no fields in this app.",
  ui = function(ns, pf) NULL,
  collect = function(input) list(),
  pickers = list(),
  denominator = NULL,
  flatten = function(p) list(),
  package = NULL
)

analysis_template <- function(type) {
  type <- as.character(type %||% "")
  if (!nzchar(type)) return(EMPTY_ANALYSIS_TEMPLATE)
  ANALYSIS_TEMPLATES[[type]] %||% EMPTY_ANALYSIS_TEMPLATE
}

analysis_type_label <- function(type) {
  types <- shinySAP::sapSchemaTypes("analysis")
  hit <- names(types)[match(as.character(type %||% ""), types)]
  if (is.na(hit)) as.character(type %||% "") else hit
}

# Every schema type must have a template, or a saved analysis of that type would
# lose its parameters on load. Called from app.R once the registry is complete.
assert_templates_cover_schema <- function() {
  missing_analyses <- setdiff(shinySAP::sapSchemaTypes("analysis"), names(ANALYSIS_TEMPLATES))
  missing_cohorts  <- setdiff(shinySAP::sapSchemaTypes("cohort"), names(COHORT_TEMPLATES))
  if (length(missing_analyses) || length(missing_cohorts)) {
    stop("Schema types without a template: ",
         paste(c(missing_analyses, missing_cohorts), collapse = ", "))
  }
  invisible(TRUE)
}

# Shared blocks ---------------------------------------------------------------
#
# Templates that share a block share its input ids. That is safe because only one
# template is ever in the DOM.

section_heading <- function(text) {
  shiny::div(class = "text-muted small fw-semibold text-uppercase mt-3 mb-2", text)
}

# The cohort pickers every IncidencePrevalence estimator starts with.
denominator_outcome_ui <- function(ns, pf) bslib::layout_columns(
  col_widths = c(6, 6),
  entity_picker(ns("denominator_cohort_id"), "Denominator cohort", pf("denominator_cohort_id"),
                placeholder = "Population at risk"),
  entity_picker(ns("outcome_cohort_id"), "Outcome cohort", pf("outcome_cohort_id"),
                placeholder = "Event being counted")
)

# The *_cohort_ids_of_interest arguments: which cohort_definition_ids of a
# denominator / outcome cohort SET to use. Blank means all, the estimator's own
# default. Free tokens, because the ids are assigned when the set is generated.
cohort_ids_of_interest_ui <- function(ns, pf, keys = c("denominator", "outcome")) {
  inputs <- lapply(keys, function(key) {
    id <- paste0(key, "_cohort_ids_of_interest")
    current <- chr_vec(pf(id, character(0)))
    shiny::selectizeInput(ns(id), sprintf("%s cohort ids of interest", tools::toTitleCase(key)),
                          choices = current, selected = current, multiple = TRUE, width = "100%",
                          options = list(create = TRUE, placeholder = "Blank = every cohort in the set"))
  })
  do.call(bslib::layout_columns, c(list(col_widths = rep(12 / length(keys), length(keys))), inputs))
}

# Structured strata: a list of variable groups naming columns on the denominator
# cohort table -- list("sex", c("sex", "age_group")) is one stratification by sex
# and another by their cross. Each token is one group; a comma inside a token
# crosses its variables. The choices are the columns the chosen denominator
# carries (analysis_item_server() keeps them in step); `free` lets any column be
# typed, for estimators whose strata are columns this app cannot see.
strata_ui <- function(ns, pf, free = FALSE) shiny::tagList(
  shiny::selectizeInput(
    ns("strata"), "Strata", choices = chr_vec(pf("strata", character(0))),
    selected = chr_vec(pf("strata", character(0))), multiple = TRUE, width = "100%",
    options = list(
      create = free,
      placeholder = paste("Columns on the target/denominator cohort;",
                          "comma to cross (sex, age_group)")
    )
  ),
  # Inert without strata -- the estimators then return only the overall
  # estimate -- so the choice is offered only when strata exist.
  shiny::conditionalPanel(
    condition = sprintf("(input['%s'] || []).length > 0", ns("strata")),
    shiny::checkboxInput(ns("include_overall_strata"), "Also report an overall (unstratified) result",
                         value = isTRUE(pf("include_overall_strata", TRUE)), width = "100%")
  )
)

# Selectize tokens -> the SAP's list of groups; NULL when there are none.
parse_strata <- function(x) {
  toks <- chr_vec(x)
  groups <- lapply(toks, function(tok) chr_vec(strsplit(tok, ",", fixed = TRUE)[[1]]))
  groups <- groups[vapply(groups, length, integer(1)) > 0]
  vec_or_null(groups)
}

# The SAP's list of groups -> selectize tokens, for pf() on the way back in.
strata_tokens <- function(groups) {
  if (!length(groups)) return(character(0))
  vapply(groups, function(g) paste(as.character(unlist(g)), collapse = ", "), character(1))
}

# A read-only echo of what the chosen denominator already fixes, so nobody
# re-specifies an age band the cohort has already decided. The item server fills
# it; a template gets the block by dropping this placeholder into its ui().
denominator_summary_ui <- function(ns, pf) shiny::uiOutput(ns("denominator_summary"))

# `cohort` is the cohorts$by_id() entry for whatever the denominator picker
# holds, or NULL; `picked` is the raw picker value, because an EMPTY pick and a
# DANGLING pick are different states.
denominator_summary <- function(cohort, picked = NULL) {
  if (is.null(cohort)) {
    picked <- trimws(as.character(picked %||% "")[1])
    if (is.na(picked) || !nzchar(picked)) {
      return(shiny::p(class = "text-muted small mb-3",
                      "Pick a denominator cohort to see what it fixes and the cohort set this analysis runs over."))
    }
    return(shiny::div(class = "alert alert-warning py-2 small mb-3",
                      sprintf("'%s' is not defined on the Cohorts tab, so nothing can be inherited from it.", picked)))
  }
  if (!is_denominator_type(cohort$type)) {
    return(shiny::div(
      class = "alert alert-warning py-2 small mb-3",
      sprintf(paste("'%s' is not a denominator cohort, so it fixes no age groups, sex or time at risk.",
                    "Set its type on the Cohorts tab."),
              cohort$name %||% "This cohort")
    ))
  }
  denominator_panel(cohort, "Inherited from this cohort -- set it on the Cohorts tab:")
}

# Every input id a template's ui() creates, recovered by handing it a namespace
# that records instead of namespacing. The tests use it to check for collisions
# and for the mirror invariant.
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
