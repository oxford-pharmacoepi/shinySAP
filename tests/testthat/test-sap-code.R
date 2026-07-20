# The generated code IS the deliverable now -- a SAP is meant to be executed, not
# only read -- so these check the call text against the package signatures rather
# than merely that something was produced.

test_that("a denominator cohort renders generateDenominatorCohortSet in signature order", {
  co <- list(
    name = "General population", kind = "denominator",
    cohortDateRange = list("2015-08-01", "2016-07-31"),
    ageGroup = list(c(0, 5), c(6, 17), c(18, 64), c(65, 150)),
    sex = list("Both"), daysPriorObservation = list(0),
    requirementInteractions = FALSE
  )
  code <- cohort_r_code(co)

  expect_match(code, "^generateDenominatorCohortSet\\(")
  expect_match(code, 'name\\s+= "general_population"', perl = TRUE)
  expect_match(code, 'as\\.Date\\(c\\("2015-08-01", "2016-07-31"\\)\\)', perl = TRUE)
  expect_match(code, "list\\(c\\(0, 5\\), c\\(6, 17\\), c\\(18, 64\\), c\\(65, 150\\)\\)", perl = TRUE)
  expect_match(code, "requirementInteractions = FALSE", perl = TRUE)

  # Signature order: cohortDateRange, ageGroup, sex, daysPriorObservation,
  # requirementInteractions. A reordered call still runs, but a SAP that reads
  # differently from the package reference invites exactly the misreading this
  # whole file exists to prevent.
  pos <- vapply(c("cohortDateRange", "ageGroup", "sex", "daysPriorObservation",
                  "requirementInteractions"),
                function(a) regexpr(paste0("\n  ", a), code, fixed = FALSE)[[1]], numeric(1))
  expect_false(is.unsorted(pos))
})

test_that("a target denominator adds targetCohortTable, timeAtRisk and requirementsAtEntry", {
  code <- cohort_r_code(list(
    name = "Vaccinated population", kind = "target_denominator",
    targetCohortTable = "Influenza vaccine recipients",
    timeAtRisk = list(c(0, 30), c(31, 60)),
    ageGroup = list(c(18, 64)), sex = list("Both"), daysPriorObservation = list(365),
    requirementsAtEntry = TRUE, requirementInteractions = TRUE
  ))

  expect_match(code, "^generateTargetDenominatorCohortSet\\(")
  expect_match(code, 'targetCohortTable\\s+= "influenza_vaccine_recipients"', perl = TRUE)
  expect_match(code, "timeAtRisk\\s+= list\\(c\\(0, 30\\), c\\(31, 60\\)\\)", perl = TRUE)
  expect_match(code, "requirementsAtEntry\\s+= TRUE", perl = TRUE)
})

# The generators are the only things IncidencePrevalence creates. A plain cohort
# is instantiated elsewhere, so inventing a call for it would be a lie.
test_that("a plain cohort produces no generator call", {
  expect_null(cohort_r_code(list(name = "Outcome", kind = "outcome",
                                 entry_events = list("First diagnosis [cs_x]"))))
})

test_that("an unbounded time at risk survives as Inf, not a dropped argument", {
  code <- cohort_r_code(list(
    name = "D", kind = "target_denominator", targetCohortTable = "T",
    timeAtRisk = list(list(0, NULL)),      # what [0, null] reads back as
    ageGroup = list(c(0, 150)), sex = list("Both"), daysPriorObservation = list(0)
  ))
  expect_match(code, "timeAtRisk\\s+= c\\(0, Inf\\)", perl = TRUE)
})

# The bug this whole change started from: unlist() flattened the strata groups, so
# two separate stratifications and one crossed stratification rendered alike.
test_that("strata groups keep their nesting", {
  point <- function(strata) analysis_r_code(list(
    analysis_type = "estimatePointPrevalence",
    parameters = list(denominatorTable = "D", outcomeTable = "O",
                      interval = "years", timePoint = "start",
                      strata = strata, includeOverallStrata = TRUE)))

  separate <- point(list(list("sex"), list("age_group")))
  crossed  <- point(list(list("sex", "age_group")))

  expect_match(separate, 'strata\\s+= list\\("sex", "age_group"\\)', perl = TRUE)
  expect_match(crossed,  'strata\\s+= list\\(c\\("sex", "age_group"\\)\\)', perl = TRUE)
  expect_false(identical(separate, crossed))
})

test_that("period prevalence emits the arguments point prevalence does not have", {
  code <- analysis_r_code(list(
    analysis_type = "estimatePeriodPrevalence",
    parameters = list(denominatorTable = "General population", outcomeTable = "Flu vaccine",
                      interval = "years", completeDatabaseIntervals = TRUE,
                      fullContribution = TRUE, level = "person",
                      strata = list(list("age_group"), list("sex")),
                      includeOverallStrata = TRUE)))

  expect_match(code, "^estimatePeriodPrevalence\\(")
  expect_match(code, "completeDatabaseIntervals = TRUE", perl = TRUE)
  expect_match(code, "fullContribution\\s+= TRUE", perl = TRUE)
  expect_match(code, 'level\\s+= "person"', perl = TRUE)
  expect_false(grepl("timePoint", code, fixed = TRUE))
})

test_that("point prevalence emits timePoint and none of the period arguments", {
  code <- analysis_r_code(list(
    analysis_type = "estimatePointPrevalence",
    parameters = list(denominatorTable = "D", outcomeTable = "O",
                      interval = "months", timePoint = "middle")))
  expect_match(code, 'timePoint\\s+= "middle"', perl = TRUE)
  for (arg in c("fullContribution", "completeDatabaseIntervals", "level")) {
    expect_false(grepl(arg, code, fixed = TRUE))
  }
})

test_that("incidence emits censoring, washout and repeatedEvents", {
  code <- analysis_r_code(list(
    analysis_type = "estimateIncidence",
    parameters = list(denominatorTable = "D", outcomeTable = "O", censorTable = "C",
                      interval = "years", completeDatabaseIntervals = TRUE,
                      outcomeWashout = list(365), repeatedEvents = FALSE)))
  expect_match(code, "^estimateIncidence\\(")
  expect_match(code, 'censorTable\\s+= "c"', perl = TRUE)
  expect_match(code, "outcomeWashout\\s+= 365", perl = TRUE)
  expect_match(code, "repeatedEvents\\s+= FALSE", perl = TRUE)
})

# Inf serialises as JSON null under na = "null", so [null] must not read as
# "empty" and silently drop back to the package default.
test_that("an unbounded outcome washout renders as Inf", {
  code <- analysis_r_code(list(
    analysis_type = "estimateIncidence",
    parameters = list(denominatorTable = "D", outcomeTable = "O",
                      outcomeWashout = list(NULL))))
  expect_match(code, "outcomeWashout\\s+= Inf", perl = TRUE)
})

# A restricted analysis must say so: dropping these made a SAP run on 2 of 8
# denominator cohorts read exactly like one run on all 8.
test_that("selected cohort ids are emitted, and null means all", {
  restricted <- analysis_r_code(list(
    analysis_type = "estimatePointPrevalence",
    parameters = list(denominatorTable = "D", outcomeTable = "O",
                      denominatorCohortId = list(2, 5))))
  expect_match(restricted, "denominatorCohortId\\s+= c\\(2, 5\\)", perl = TRUE)

  all_ids <- analysis_r_code(list(
    analysis_type = "estimatePointPrevalence",
    parameters = list(denominatorTable = "D", outcomeTable = "O",
                      denominatorCohortId = NULL)))
  expect_false(grepl("denominatorCohortId", all_ids, fixed = TRUE))
})

# An undecided field falls back to the documented default rather than having a
# choice invented for it -- the rule the cards already follow.
test_that("undecided arguments are omitted rather than defaulted", {
  code <- cohort_r_code(list(name = "D", kind = "denominator",
                             sex = list(), ageGroup = list(),
                             daysPriorObservation = list()))
  for (arg in c("sex", "ageGroup", "daysPriorObservation", "cohortDateRange")) {
    expect_false(grepl(arg, code, fixed = TRUE))
  }
})

test_that("an analysis type with no package function maps to nothing", {
  expect_true(is.na(analysis_estimator("Other")))
  expect_true(is.na(analysis_estimator("")))
  expect_null(analysis_r_code(list(analysis_type = "Other", parameters = list())))
  # The registry key alone cannot say point or period, so it maps to neither.
  expect_true(is.na(analysis_estimator("Prevalence")))
  expect_identical(analysis_estimator("Incidence rate"), "estimateIncidence")
})

test_that("cohort names that collapse onto one table name are reported", {
  found <- table_name_collisions(list(
    list(name = "Flu vaccine"), list(name = "Flu  vaccine!"), list(name = "Other")))
  expect_length(found, 1)
  expect_identical(found[[1]]$name, "flu_vaccine")
  expect_length(table_name_collisions(list(list(name = "A"), list(name = "B"))), 0)
})

test_that("the whole-SAP script generates cohorts before the estimates that use them", {
  script <- sap_r_script(list(
    cohorts = list(
      list(name = "General population", kind = "denominator", sex = list("Both"),
           ageGroup = list(c(0, 150)), daysPriorObservation = list(0)),
      list(name = "Flu vaccine", kind = "outcome")),
    proposed_analyses = list(list(
      name = "Period prevalence of vaccination",
      analysis_type = "estimatePeriodPrevalence",
      parameters = list(denominatorTable = "General population",
                        outcomeTable = "Flu vaccine", interval = "years")))))

  expect_lt(regexpr("generateDenominatorCohortSet", script, fixed = TRUE)[[1]],
            regexpr("estimatePeriodPrevalence", script, fixed = TRUE)[[1]])
  # The estimate points at the table the generator above created.
  expect_match(script, 'name\\s+= "general_population"', perl = TRUE)
  expect_match(script, 'denominatorTable\\s+= "general_population"', perl = TRUE)
})

# Minimum cell count ------------------------------------------------------------

test_that("the script suppresses every estimate at the study threshold", {
  script <- sap_r_script(list(
    study = list(min_cell_count = 5),
    cohorts = list(list(name = "General population", kind = "denominator",
                        sex = list("Both"), ageGroup = list(c(0, 150)),
                        daysPriorObservation = list(0))),
    proposed_analyses = list(
      list(name = "Point prevalence", analysis_type = "estimatePointPrevalence",
           parameters = list(denominatorTable = "General population", outcomeTable = "O")),
      list(name = "Period prevalence", analysis_type = "estimatePeriodPrevalence",
           parameters = list(denominatorTable = "General population", outcomeTable = "O")))))

  # Each estimate is named so the two can be combined and suppressed together.
  expect_match(script, "point_prevalence_1 <- estimatePointPrevalence\\(", perl = TRUE)
  expect_match(script, "period_prevalence_2 <- estimatePeriodPrevalence\\(", perl = TRUE)
  expect_match(script, "omopgenerics::bind\\(", perl = TRUE)
  expect_match(script, "omopgenerics::suppress\\(results, minCellCount = 5\\)", perl = TRUE)
  # Suppression is the LAST step: it applies to results that already exist.
  expect_gt(regexpr("suppress", script, fixed = TRUE)[[1]],
            regexpr("estimatePeriodPrevalence", script, fixed = TRUE)[[1]])
})

# Binding a single object is pointless, so one estimate is suppressed directly.
test_that("a single estimate is suppressed without a bind", {
  code <- suppression_r_code(5, "only_1")
  expect_match(code, "results <- only_1", fixed = TRUE)
  expect_false(grepl("bind", code, fixed = TRUE))
})

# A plan that never stated a threshold must not have one invented for it: the
# generated script would then claim an export rule the SAP does not contain.
test_that("no stated threshold emits no suppression", {
  expect_null(suppression_r_code(NULL, "a_1"))
  expect_null(suppression_r_code(NA, "a_1"))
  expect_null(suppression_r_code(5, character(0)))
})

test_that("analyses with no package call still get unique variable names", {
  vars <- estimate_var_names(list(list(name = "Same"), list(name = "Same"), list()))
  expect_length(unique(vars), 3)
})

# Picker grouping ---------------------------------------------------------------

test_that("cohort choices group by kind, in the registry's order", {
  groups <- grouped_cohort_choices(list(
    "Gen pop"   = list(kind = "denominator"),
    "Flu"       = list(kind = "outcome"),
    "Vacc pop"  = list(kind = "target_denominator"),
    "Recipients" = list(kind = "target")))

  expect_identical(names(groups),
                   c("Denominator (general population)", "Target denominator (from a cohort)",
                     "Target cohort (population of interest)", "Outcome"))
  expect_identical(groups[["Outcome"]], "Flu")
})

# A cohort with no kind chosen is a validation problem elsewhere; hiding it here
# would make a picker unable to reach a cohort that exists.
test_that("kindless cohorts get their own trailing group", {
  groups <- grouped_cohort_choices(list("A" = list(kind = "outcome"), "B" = list()))
  expect_identical(names(groups)[[length(groups)]], "No kind chosen")
  expect_identical(groups[["No kind chosen"]], "B")
  expect_identical(grouped_cohort_choices(list()), list())
})

# Free text is the reason the pickers are grouped rather than filtered, so a
# value naming no defined cohort has to survive -- selectize drops any selection
# it cannot find among its options.
test_that("a free-text value is kept, in a group that says what it is", {
  choices <- picker_choices(list("Outcome" = "Flu"), "Not yet defined")
  expect_identical(choices[["Not defined on the Cohorts tab"]], "Not yet defined")
  expect_true("" %in% unlist(choices, use.names = FALSE))
  expect_true("Flu" %in% unlist(choices, use.names = FALSE))

  # A value already among the groups is not duplicated into the extras group.
  expect_null(picker_choices(list("Outcome" = "Flu"), "Flu")[["Not defined on the Cohorts tab"]])
  # Flat choices (data sources, strata) keep the old behaviour exactly.
  expect_identical(picker_choices(c("a", "b"), "c"), c("", "a", "b", "c"))
})
