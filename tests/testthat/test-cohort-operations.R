# Typed cohort operations: a plain cohort's logic as data rather than sentences.
#
# The point of the type is that one source produces BOTH the prose a reviewer
# reads and the code the study runs, so the two cannot describe different
# cohorts. Most of these tests are that invariant from one side or the other.

fl_cohort <- function(ops) {
  list(name = "Follicular lymphoma 5-year", kind = "target", operations = ops)
}

standard_ops <- list(
  list(op = "concept_cohort", codelist = "cs_fl"),
  list(op = "require_first_entry"),
  list(op = "pad_cohort_end", days = 1825)
)

# Code generation ---------------------------------------------------------------

test_that("operations become a CohortConstructor pipeline in order", {
  code <- cohort_operations_code(fl_cohort(standard_ops))
  expect_match(code, "cdm$follicular_lymphoma_5_year <- conceptCohort(", fixed = TRUE)
  expect_match(code, 'name       = "follicular_lymphoma_5_year"', fixed = TRUE)
  expect_match(code, "requireIsFirstEntry()", fixed = TRUE)
  expect_match(code, "padCohortEnd(days = 1825)", fixed = TRUE)
  # Order is the meaning, so it has to survive into the pipeline.
  expect_lt(regexpr("requireIsFirstEntry", code, fixed = TRUE)[[1]],
            regexpr("padCohortEnd", code, fixed = TRUE)[[1]])
})

# A cohort authored as free text is untouched: nothing infers operations from
# sentences, which is the guess this whole design exists to avoid.
test_that("a cohort with no operations generates no cohort code", {
  expect_null(cohort_operations_code(list(
    name = "Outcome", kind = "outcome",
    entry_events = list("Diagnosis of X [cs_x]"),
    exit_criteria = list("End of continuous observation"))))
})

test_that("a step with no generated call becomes a visible TODO, not a silent gap", {
  code <- cohort_operations_code(fl_cohort(list(
    list(op = "concept_cohort", codelist = "cs_fl"),
    list(op = "custom", text = "Restrict to histologically confirmed cases"))))
  expect_match(code, "# TODO: Restrict to histologically confirmed cases", fixed = TRUE)
})

test_that("an unregistered op is reported rather than dropped", {
  code <- cohort_operations_code(fl_cohort(list(
    list(op = "concept_cohort", codelist = "cs_fl"),
    list(op = "require_moon_phase"))))
  expect_match(code, "No cohort operation is registered as 'require_moon_phase'",
               fixed = TRUE)
})

# Without an entry op nothing creates the cohort, so the pipeline would start
# from an object that does not exist.
test_that("operations that never create the cohort say so in the script", {
  code <- cohort_operations_code(fl_cohort(list(list(op = "require_first_entry"))))
  expect_match(code, "no entry operation", fixed = TRUE)
})

test_that("requireDemographics renders the package's own argument shapes", {
  code <- cohort_operations_code(fl_cohort(list(
    list(op = "concept_cohort", codelist = "cs_fl"),
    list(op = "require_demographics", age_range = list(c(18, 150)),
         sex = "Female", min_prior_observation = 365))))
  # Layout depends on whether the call fits one line, so match the arguments
  # rather than the spacing; the wrapped form is checked below.
  expect_match(code, "requireDemographics\\(", perl = TRUE)
  expect_match(code, "ageRange\\s+= list\\(c\\(18, 150\\)\\)", perl = TRUE)
  expect_match(code, 'sex\\s+= "Female"', perl = TRUE)
  expect_match(code, "minPriorObservation\\s+= 365", perl = TRUE)
  # Every line of a wrapped step sits INSIDE the pipeline, closing paren included.
  for (line in strsplit(code, "\n", fixed = TRUE)[[1]][-(1:5)]) {
    expect_match(line, "^  ", perl = TRUE)
  }
})

test_that("requireInDateRange renders an open bound as NA, like cohortDateRange", {
  code <- cohort_operations_code(fl_cohort(list(
    list(op = "concept_cohort", codelist = "cs_fl"),
    list(op = "require_in_date_range", start = "2010-01-01"))))
  expect_match(code, 'requireInDateRange(dateRange = as.Date(c("2010-01-01", NA)))',
               fixed = TRUE)
})

# Prose -------------------------------------------------------------------------

# The direction is the point: the sentences are generated FROM the operations.
test_that("operations generate the prose the document shows", {
  lines <- cohort_operations_prose(fl_cohort(standard_ops))
  expect_length(lines, 3)
  expect_match(lines[[1]], "[cs_fl]", fixed = TRUE)
  expect_match(lines[[2]], "first entry", fixed = TRUE)
  # padCohortEnd() clamps to observation end itself, so the plan can state the
  # "whichever comes first" the free-text form could never pin down. It extends
  # the EXIT date, which is what the sentence says -- "after entry" would only be
  # true alongside an entry that ends on the record's start date.
  expect_match(lines[[3]], "Extend the exit date by 1825 days", fixed = TRUE)
  expect_match(lines[[3]], "whichever comes first", fixed = TRUE)
})

test_that("a cohort with no operations has no generated prose", {
  expect_length(cohort_operations_prose(list(name = "X", kind = "target")), 0)
})

# Validation --------------------------------------------------------------------

test_that("valid operations report no problems", {
  expect_length(cohort_operations_problems(fl_cohort(standard_ops), "cs_fl"), 0)
})

test_that("an entry citing an unknown codelist is a problem", {
  errs <- cohort_operations_problems(fl_cohort(standard_ops), "cs_other")
  expect_length(errs, 1)
  expect_match(errs[[1]], "not on the Codelists tab", fixed = TRUE)
})

test_that("operations with no entry step are a problem", {
  errs <- cohort_operations_problems(fl_cohort(list(list(op = "require_first_entry"))))
  expect_match(paste(errs, collapse = " "), "never create the cohort", fixed = TRUE)
})

test_that("an entry step that is not first is a problem", {
  errs <- cohort_operations_problems(fl_cohort(list(
    list(op = "require_first_entry"),
    list(op = "concept_cohort", codelist = "cs_fl"))), "cs_fl")
  expect_match(paste(errs, collapse = " "), "has to come first", fixed = TRUE)
})

test_that("two entry steps are a problem", {
  errs <- cohort_operations_problems(fl_cohort(list(
    list(op = "concept_cohort", codelist = "cs_fl"),
    list(op = "concept_cohort", codelist = "cs_fl"))), "cs_fl")
  expect_match(paste(errs, collapse = " "), "only the first step can", fixed = TRUE)
})

test_that("a step missing a required field is a problem naming the step", {
  errs <- cohort_operations_problems(fl_cohort(list(
    list(op = "concept_cohort", codelist = "cs_fl"),
    list(op = "pad_cohort_end"))), "cs_fl")
  expect_match(paste(errs, collapse = " "), "Step 2", fixed = TRUE)
  expect_match(paste(errs, collapse = " "), "how many days", fixed = TRUE)
})

test_that("a cohort with no operations is not validated as if it had some", {
  expect_length(cohort_operations_problems(list(name = "X", kind = "target")), 0)
})

# Concept sets ------------------------------------------------------------------

# The SAP names a codelist without carrying its codes, so the script cannot
# know the path -- but the conceptCohort() call references the variable either
# way, so the assignment is emitted as runnable code with a placeholder path:
# left unfilled it fails loudly at importCodelist(), with the TODO above it as
# the trail back to the plan, rather than at a bare reference nothing explains.
#
# A placeholder rather than the export's by-category path, for two reasons that
# have to stay true together: the appendix is read before OmopStudyBuilder has
# laid out any study directory, and the preview's render session (see
# inst/sap_preview.Rmd) does not load sap_study_export.R -- so this function
# must not call into it.
test_that("a cited codelist becomes code with a placeholder path, not a comment", {
  code <- concept_set_r_code(list(name = "cs_fl", category = "Index event"))
  expect_match(code, "# TODO", fixed = TRUE)
  expect_match(code, "'cs_fl'", fixed = TRUE)
  # The assignment is code (start of line), not part of the comment block.
  expect_match(code, "\ncs_fl <- omopgenerics::importCodelist", fixed = TRUE)
  expect_match(code, 'path = "<path/to/cs_fl.csv>"', fixed = TRUE)
})

# The script ---------------------------------------------------------------------

# Dependency order: a concept set exists before the cohort entering on it, and
# that cohort before the estimate pointing at it.
test_that("the script builds concept sets, then cohorts, then estimates", {
  script <- sap_r_script(list(
    study = list(min_cell_count = 5),
    codelists = list(list(name = "cs_fl")),
    cohorts = list(
      list(name = "FL", kind = "target", operations = standard_ops),
      list(name = "General population", kind = "denominator", sex = list("Both"),
           ageGroup = list(c(0, 150)), daysPriorObservation = list(0))),
    proposed_analyses = list(list(
      name = "PP", analysis_type = "estimatePointPrevalence",
      parameters = list(denominatorTable = "General population", outcomeTable = "FL")))))

  pos <- function(s) regexpr(s, script, fixed = TRUE)[[1]]
  expect_lt(pos("codelist 'cs_fl'"), pos("conceptCohort("))
  expect_lt(pos("conceptCohort("), pos("generateDenominatorCohortSet("))
  expect_lt(pos("generateDenominatorCohortSet("), pos("estimatePointPrevalence("))
  # The cohort table the pipeline creates is the one the estimate points at.
  expect_match(script, 'outcomeTable     = "fl"', fixed = TRUE)
})

# A codelist nothing enters on is cited in prose, not in code, so marking it
# would leave an unused TODO in the script.
test_that("only codelists a typed entry cites get a block in the script", {
  script <- sap_r_script(list(
    codelists = list(list(name = "cs_used"), list(name = "cs_unused")),
    cohorts = list(list(name = "FL", kind = "target", operations = list(
      list(op = "concept_cohort", codelist = "cs_used"))))))
  expect_true(grepl("cs_used <- omopgenerics::importCodelist", script, fixed = TRUE))
  expect_false(grepl("cs_unused", script, fixed = TRUE))
})

# An entry naming several codelists ----------------------------------------------
#
# conceptCohort() creates ONE COHORT PER CODELIST ENTRY, so several named
# together are several cohorts in one table. That is what lets a study with six
# outcomes sharing an exit rule run one estimator call instead of six, and it is
# why `codelist` reads as a vector rather than a name.

many_ops <- list(
  list(op = "concept_cohort", codelist = list("cs_fl", "cs_mm"), exit = "event_start_date"),
  list(op = "require_first_entry"),
  list(op = "pad_cohort_end", days = 1825)
)

test_that("several codelists become one conceptCohort call over c() of them", {
  code <- cohort_operations_code(fl_cohort(many_ops))
  expect_match(code, "conceptSet = c(cs_fl, cs_mm)", fixed = TRUE)
  # One table, not one per codelist.
  expect_equal(length(gregexpr("conceptCohort(", code, fixed = TRUE)[[1]]), 1)
})

# The single-name form is the one every SAP written before this used, so it has
# to keep rendering as a bare variable rather than c() of one.
test_that("one codelist still renders unwrapped", {
  expect_match(cohort_operations_code(fl_cohort(standard_ops)),
               "conceptSet = cs_fl", fixed = TRUE)
})

test_that("prose names every codelist the entry cites", {
  line <- cohort_operations_prose(fl_cohort(many_ops))[[1]]
  expect_match(line, "[cs_fl, cs_mm]", fixed = TRUE)
})

test_that("every cited codelist gets a block in the script, not just the first", {
  script <- sap_r_script(list(
    codelists = list(list(name = "cs_fl"), list(name = "cs_mm")),
    cohorts   = list(fl_cohort(many_ops))))
  expect_true(grepl("cs_fl <- omopgenerics::importCodelist", script, fixed = TRUE))
  expect_true(grepl("cs_mm <- omopgenerics::importCodelist", script, fixed = TRUE))
})

# Reading only the first name would leave the rest looking uncited, which is the
# opposite of the reference check's job.
test_that("an unknown codelist is reported wherever it sits in the list", {
  errs <- cohort_operations_problems(fl_cohort(many_ops), "cs_fl")
  expect_length(errs, 1)
  expect_match(errs[[1]], "cs_mm", fixed = TRUE)
  expect_no_match(errs[[1]], "cs_fl", fixed = TRUE)
})

test_that("all cited codelists resolving is no problem", {
  expect_length(cohort_operations_problems(fl_cohort(many_ops), c("cs_fl", "cs_mm")), 0)
})

# One cohort per codelist is the point, so a repeat would collide in the set.
test_that("naming the same codelist twice is a problem", {
  errs <- cohort_operations_problems(
    fl_cohort(list(list(op = "concept_cohort", codelist = list("cs_fl", "cs_fl")))),
    "cs_fl")
  expect_match(paste(errs, collapse = " "), "more than once", fixed = TRUE)
})

test_that("the codelist reference check sees every name an entry cites", {
  expect_setequal(extract_codelist_refs(fl_cohort(many_ops)), c("cs_fl", "cs_mm"))
})

# Round trip through the card -----------------------------------------------------

# A card that dropped a key it cannot yet edit would delete the only executable
# description of the cohort on the next autosave.
test_that("a cohort card preserves operations it cannot yet edit", {
  tmpl <- cohort_template("target")
  pf   <- tmpl$flatten(list(name = "FL", kind = "target", operations = standard_ops))
  # flatten() hands the card the JSON its hidden field is rebuilt from...
  expect_true(nzchar(pf$operations_json))
  # ...and collect() reads that same field back into the operations it came from.
  back <- tmpl$collect(list(entry_events = "", inclusion_criteria = "",
                            exit_criteria = "", operations_json = pf$operations_json))
  expect_length(back$operations, 3)
  expect_identical(back$operations[[1]]$op, "concept_cohort")
  # JSON has one number type, so the round trip may hand back an integer.
  expect_equal(back$operations[[3]]$days, 1825)
})

test_that("a cohort card with no operations collects an empty list, not junk", {
  tmpl <- cohort_template("target")
  back <- tmpl$collect(list(entry_events = "Diagnosis [cs_x]", inclusion_criteria = "",
                            exit_criteria = "", operations_json = "[]"))
  expect_length(back$operations, 0)
  expect_identical(as.character(back$entry_events), "Diagnosis [cs_x]")
})

# A hand-broken file must not take the whole cohorts tab down with it.
test_that("unreadable operations JSON degrades to none rather than erroring", {
  expect_length(parse_operations("{not json"), 0)
  expect_length(parse_operations(NULL), 0)
})

# Codelist references -------------------------------------------------------------

# A typed entry names its codelist in a field, not in [brackets], but it is the
# same reference and earns the same check.
test_that("a typed entry's codelist counts as a codelist reference", {
  refs <- extract_codelist_refs(fl_cohort(standard_ops))
  expect_identical(refs, "cs_fl")
  found <- codelist_reference_problems(list(fl_cohort(standard_ops)), "cs_fl")
  expect_length(found, 0)
})

test_that("a typed entry citing an undefined codelist is reported by the list check", {
  found <- codelist_reference_problems(list(fl_cohort(standard_ops)), "cs_other")
  expect_true(length(found) >= 1)
})

# Entry exit date -----------------------------------------------------------------

# padCohortEnd() adds days to the cohort END, so "N days after index" is only
# true when the episode ends on the record's start date. The two ops decide it
# together, which is why the entry carries `exit`.
test_that("an entry can end the episode on the record's start date", {
  code <- cohort_operations_code(fl_cohort(list(
    list(op = "concept_cohort", codelist = "cs_fl", exit = "event_start_date"),
    list(op = "pad_cohort_end", days = 1825))))
  expect_match(code, 'exit       = "event_start_date"', fixed = TRUE)
})

test_that("the default exit is left implicit rather than restated", {
  code <- cohort_operations_code(fl_cohort(standard_ops))
  expect_false(grepl("exit", code, fixed = TRUE))
})

test_that("an unknown entry exit value is a problem", {
  errs <- cohort_operations_problems(fl_cohort(list(
    list(op = "concept_cohort", codelist = "cs_fl", exit = "whenever"))), "cs_fl")
  expect_match(paste(errs, collapse = " "), "event_end_date", fixed = TRUE)
})

# The prose has to say what padCohortEnd does -- extend the EXIT -- not "after
# entry", which is only true alongside the entry op above.
test_that("pad_cohort_end prose describes extending the exit date", {
  line <- cohort_operations_prose(fl_cohort(standard_ops))[[3]]
  expect_match(line, "Extend the exit date by 1825 days", fixed = TRUE)
  expect_match(line, "whichever comes first", fixed = TRUE)
})

# Library header -------------------------------------------------------------------
#
# Derived from what the script actually calls, so a plan never tells its reader
# to load a package it has no use for.

test_that("the script loads only the packages its blocks call", {
  denom_only <- sap_r_script(list(cohorts = list(list(
    name = "D", kind = "denominator", sex = list("Both"),
    ageGroup = list(c(0, 150)), daysPriorObservation = list(0)))))
  expect_match(denom_only, "library(IncidencePrevalence)", fixed = TRUE)
  expect_false(grepl("library(CohortConstructor)", denom_only, fixed = TRUE))

  cohort_only <- sap_r_script(list(cohorts = list(list(
    name = "FL", kind = "target",
    operations = list(list(op = "concept_cohort", codelist = "cs_fl"))))))
  expect_match(cohort_only, "library(CohortConstructor)", fixed = TRUE)
  expect_false(grepl("library(IncidencePrevalence)", cohort_only, fixed = TRUE))
})

test_that("the library header comes first, before anything it is needed for", {
  script <- sap_r_script(list(cohorts = list(list(
    name = "FL", kind = "target",
    operations = list(list(op = "concept_cohort", codelist = "cs_fl"))))))
  expect_lt(regexpr("library(", script, fixed = TRUE)[[1]],
            regexpr("conceptCohort(", script, fixed = TRUE)[[1]])
})

# omopgenerics::suppress() is written namespaced, so it needs no library() -- and
# a SAP that generates nothing must not emit a header at all.
test_that("a script with nothing to generate has no library header", {
  expect_identical(sap_r_script(list()), "")
})

test_that("an op that emits no call contributes no library", {
  script <- sap_r_script(list(cohorts = list(list(
    name = "FL", kind = "target",
    operations = list(list(op = "custom", text = "done by hand"))))))
  expect_false(grepl("library(", script, fixed = TRUE))
})

# Binding several definitions into one table -------------------------------------
#
# The op that lets ONE estimator call cover what would otherwise be several
# identical analyses. estimatePrevalence()'s outcomeTable takes one table -- a
# vector fails with "You can only read one table of a cdm_reference" -- but a
# table may hold many cohorts and the estimator reports each separately. That is
# already why six cancers are one call; this lets definitions built by different
# pipelines share a call too -- PROVIDED their cohort names are disjoint, which
# is the collision check at the bottom of this file.
#
# Verified against omopgenerics 1.4.1: the generated bind runs verbatim, ids are
# renumbered in argument order, and one estimatePeriodPrevalence() over the
# result reports each constituent.

three <- c("RBC 5-year partial", "RBC 2-year partial", "RBC complete")
bound <- function(cohorts = three) list(
  name = "RBC all definitions", kind = "outcome",
  operations = list(list(op = "bind_cohorts", cohorts = as.list(cohorts))))
# Distinct codelist per cohort by default: conceptCohort() names cohorts AFTER
# their codelists, and same-named cohorts cannot bind.
built <- function(name, codelist = paste0("cs_", gsub("\\W+", "_", tolower(name)))) {
  list(name = name, kind = "outcome",
       operations = list(list(op = "concept_cohort", codelist = codelist)))
}

# bind() returns a CDM REFERENCE with the table attached, not a cohort table, so
# it is `cdm <- ` and never `cdm$x <- `. Getting this wrong does not fail loudly:
# it leaves the table unattached and every estimate on it dies at the partner.
test_that("a bind is assigned to cdm, not into a cdm slot", {
  code <- cohort_operations_code(bound())
  expect_match(code, "cdm <- omopgenerics::bind(", fixed = TRUE)
  expect_false(grepl("cdm$rbc_all_definitions <- omopgenerics::bind", code, fixed = TRUE))
  expect_match(code, 'name = "rbc_all_definitions"', fixed = TRUE)
  for (nm in c("cdm$rbc_5_year_partial", "cdm$rbc_2_year_partial", "cdm$rbc_complete")) {
    expect_match(code, nm, fixed = TRUE)
  }
})

test_that("a bind needs no library: it is namespaced", {
  expect_length(cohort_operations_packages(bound()), 0)
})

test_that("the bind prose names every cohort it combines", {
  line <- cohort_operations_prose(bound())[[1]]
  for (nm in three) expect_match(line, nm, fixed = TRUE)
})

# Steps after a bind cannot pipe from it -- the table is attached by the call
# itself -- so they become a second statement piping from the attached table.
test_that("a step after a bind pipes from the attached table", {
  code <- cohort_operations_code(list(
    name = "RBC all definitions", kind = "outcome",
    operations = list(list(op = "bind_cohorts", cohorts = as.list(three)),
                      list(op = "require_first_entry"))))
  expect_match(code, "cdm$rbc_all_definitions <- cdm$rbc_all_definitions |>", fixed = TRUE)
  expect_match(code, "requireIsFirstEntry()", fixed = TRUE)
})

# The op's own validate(), which sees names only.
test_that("a bind must name two or more cohorts it can resolve", {
  probs <- function(co, cohorts = three) cohort_operations_problems(co, cohort_names = cohorts)
  expect_match(probs(bound(three[1])), "two or more", fixed = TRUE)
  expect_match(probs(bound(character(0))), "must name the cohorts", fixed = TRUE)
  expect_match(probs(bound(c(three[1], three[1]))), "more than once", fixed = TRUE)
  expect_match(probs(bound(c(three[1], "Nowhere"))), "does not define", fixed = TRUE)
  expect_match(probs(bound(c(three[1], "RBC all definitions"))),
               "cannot include the cohort it defines", fixed = TRUE)
  expect_length(probs(bound()), 0)
})

# What the op cannot see: whether the cohorts it binds are BUILT, built FIRST,
# or denominators. All three end as `cdm$<table>` read before it exists.
test_that("binding a cohort the script never creates is reported", {
  found <- bound_cohort_problems(list(
    list(name = three[1], kind = "outcome", entry_events = list("prose only")),
    bound(three[1:1])))
  expect_length(found, 1)
  expect_match(found[[1]]$messages, "never creates", fixed = TRUE)
})

test_that("binding a cohort defined later is reported, with the fix", {
  found <- bound_cohort_problems(c(list(bound()), lapply(three, built)))
  expect_length(found, 1)
  expect_match(found[[1]]$messages[[1]], "defines AFTER it", fixed = TRUE)
  expect_match(found[[1]]$messages[[1]], "move", fixed = TRUE)
})

test_that("binding a denominator cohort set is reported", {
  found <- bound_cohort_problems(list(
    list(name = "Denom", kind = "denominator", sex = list("Both"),
         ageGroup = list(c(0, 150)), daysPriorObservation = list(0)),
    bound("Denom")))
  expect_length(found, 1)
  expect_match(found[[1]]$messages, "denominator", fixed = TRUE)
})

test_that("cohorts bound in the right order raise nothing", {
  expect_length(bound_cohort_problems(c(lapply(three, built), list(bound()))), 0)
})

# The fourth thing the op cannot see, and the one bind() itself would only say at
# the data partner: constituents whose tables hold the SAME cohort name.
# conceptCohort() names one cohort per codelist, after the codelist, so
# definitions that differ only in exit collide -- bind() aborts on a duplicated
# cohort_name, and the estimates it labels would be indistinguishable anyway.
test_that("binding definitions that share a codelist is reported", {
  found <- bound_cohort_problems(list(
    built(three[1], codelist = "cs_fl"), built(three[2], codelist = "cs_fl"),
    bound(three[1:2])))
  expect_length(found, 1)
  expect_match(found[[1]]$messages, "cohort named [cs_fl]", fixed = TRUE)
  expect_match(found[[1]]$messages, three[1], fixed = TRUE)
  expect_match(found[[1]]$messages, three[2], fixed = TRUE)
})

# The collision reaches THROUGH a nested bind: a bound table holds the union of
# its constituents' names.
test_that("a shared codelist is found through a nested bind", {
  found <- bound_cohort_problems(list(
    built("A", codelist = "cs_shared"), built("B"),
    list(name = "AB", kind = "outcome",
         operations = list(list(op = "bind_cohorts", cohorts = list("A", "B")))),
    built("C", codelist = "cs_shared"),
    list(name = "All", kind = "outcome",
         operations = list(list(op = "bind_cohorts", cohorts = list("AB", "C"))))))
  expect_length(found, 1)
  expect_identical(found[[1]]$name, "All")
  expect_match(found[[1]]$messages, "cs_shared", fixed = TRUE)
})

# A constituent whose table holds names the plan cannot know (a prose cohort is
# already reported as never built; this one has a custom entry) stays out of the
# collision check rather than being guessed at.
test_that("unknowable constituent names are not guessed at", {
  custom <- list(name = "Hand-built", kind = "outcome",
                 operations = list(list(op = "custom", text = "by hand"),
                                   list(op = "require_first_entry")))
  expect_length(bound_cohort_problems(list(
    built(three[1]), custom, bound(c(three[1], "Hand-built")))), 0)
})
