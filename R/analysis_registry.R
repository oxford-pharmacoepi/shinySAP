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

# Types renamed after SAPs had already been saved under the old label. Without
# this, selectInput() drops a `selected` it cannot find in `choices`, the browser
# falls back to the first option, and the analysis silently changes type.
ANALYSIS_TYPE_ALIASES <- c("Incidence rate" = "Incidence")

# The half of the card every analysis type shares, and the only keys load() lifts
# straight from the file without going through a template.
ANALYSIS_COMMON_FIELDS <- c("name", "analysis_type", "description", "data_sources")

# Ids already taken by the common half and by item_card(). No template may reuse
# one, or the card would carry a duplicate input id.
RESERVED_INPUT_IDS <- c(ANALYSIS_COMMON_FIELDS, "remove", "box", "type_fields")

ANCHORS           <- c("cohort start", "cohort end")
DENOMINATOR_UNITS <- c("person-years", "person-months", "person-days")
PREVALENCE_TYPES  <- c("Point prevalence", "Period prevalence")

# The registry ----------------------------------------------------------------
#
# Populated at source time by the analysis_type_*.R files. Lookup is by key, so
# the order they register in does not matter.
ANALYSIS_TEMPLATES <- list()

# A template is four things that mirror one another, so a field cannot be added
# to the form without also being serialised and read back:
#
#   hint     one line shown above the block, or NULL
#   ui       function(ns, pf) -> the type's inputs
#   collect  function(input) -> the type's JSON, reading ONLY its own input ids
#   pickers  input ids that pick a cohort or a CDM source, by entity
#   flatten  function(params) -> prefill keys; the inverse of collect's nesting,
#            or identity if collect nests nothing
register_analysis_template <- function(type, hint = NULL, ui, collect,
                                       pickers = list(), flatten = function(p) p) {
  ANALYSIS_TEMPLATES[[type]] <<- list(
    hint = hint, ui = ui, collect = collect, pickers = pickers, flatten = flatten
  )
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

# Every input id a template's ui() creates, recovered by handing it a namespace
# that records instead of namespacing. Saves maintaining the id list by hand in a
# third place; the tests use it to check for collisions.
template_field_ids <- function(tmpl) {
  ids <- character(0)
  rec_ns <- function(x) {
    ids <<- c(ids, x)
    x
  }
  tmpl$ui(rec_ns, function(key, default = NULL) default)
  unique(ids)
}
