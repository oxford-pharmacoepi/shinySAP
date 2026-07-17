# Sourced by testthat before every test file.
#
# utils.R and dynamic_items.R come first here, unlike under Shiny's
# loadSupport(): the templates call %||% and entity_picker() when they run, and
# the tests run them. The templates are then globbed exactly as the app finds
# them, so a new analysis_type_*.R file is covered by the checks without
# touching the tests.

library(jsonlite)
library(shiny)
library(bslib)

app_root <- normalizePath(testthat::test_path("..", ".."))
source(file.path(app_root, "R", "utils.R"))
source(file.path(app_root, "R", "dynamic_items.R"))
source(file.path(app_root, "R", "analysis_registry.R"))
source(file.path(app_root, "R", "cohort_kinds.R"))   # the analysis validators read it
source(file.path(app_root, "R", "mod_cdm_changes.R")) # migrate_cdm_change reads CDM_CHANGE_TYPES
source(file.path(app_root, "R", "mod_cohorts.R"))     # test-renames drives cohorts_server
for (f in sort(list.files(file.path(app_root, "R"),
                          pattern = "^analysis_type_.*\\.R$", full.names = TRUE))) {
  source(f)
}

picker_ids <- function(tmpl) unlist(tmpl$pickers, use.names = FALSE) %||% character(0)

round_trip <- function(tmpl, input) {
  params <- tmpl$collect(input)
  rt     <- fromJSON(as.character(sap_json(params)), simplifyVector = FALSE)
  list(json = rt, flat = tmpl$flatten(rt), pf = prefiller(tmpl$flatten(rt)))
}

# The cohort index as cohorts$by_name() hands it to a validator: keyed by name,
# with the JSON shapes (sex a vector, age_groups a list of numeric pairs).
#
# Note the target cohort and the target denominator are separate entries. They are
# different objects: the first is defined by entry criteria, the second is
# generated from it by generateTargetDenominatorCohortSet().
#
# No strata_variables here: as of 0.4.1 the columns a denominator carries are not
# a field on it. generateDenominatorCohortSet() makes age_group and sex, so
# cohort_strata_variables() reads STRATA_VARIABLES off the kind instead.
cohorts_idx <- list(
  "Metformin new users"   = list(kind = "target", entry_events = list("First dispensation")),
  "Metformin denominator" = list(kind = "target_denominator",
                                 targetCohortTable = "Metformin new users",
                                 timeAtRisk = list(c(0, Inf)), sex = list("Both"),
                                 ageGroup = list(c(0, 17), c(18, 64))),
  "Men only"              = list(kind = "denominator", sex = list("Male"),
                                 ageGroup = list(c(18, 64))),
  "Lactic acidosis"       = list(kind = "outcome", sex = list("Both"))
)
