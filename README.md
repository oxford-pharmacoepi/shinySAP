
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinySAP

[![R-CMD-check](https://github.com/oxford-pharmacoepi/shinySAP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/oxford-pharmacoepi/shinySAP/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/oxford-pharmacoepi/shinySAP/graph/badge.svg)](https://app.codecov.io/gh/oxford-pharmacoepi/shinySAP)

An R package for capturing a Statistical Analysis Plan (SAP) in a
structured, machine-readable form, with a Shiny app for writing one. The
package defines what a SAP *is* – a versioned schema of sections, typed
components and their parameters – and gives you constructors, validation
and a JSON file format. The app is a form over exactly that schema:
everything it saves is a `sap` object the package can read, check and
turn into runnable study code.

## Running

Install the package, then launch the app:

``` r
pak::local_install()
shinySAP::shinySap()
```

The app saves each SAP as one JSON file in the folder you launch it
from. Point it somewhere else with:

``` r
shinySAP::shinySap(outputDir = "~/saps")
# or, before launching
options(shinySAP.output_dir = "~/saps")
```

The app is a plain Shiny app directory shipped inside the package, so
`shiny::runApp(system.file("app", package = "shinySAP"))` works too. The
document preview needs a pandoc; `shinySap()` finds RStudio’s copy when
there is no other, and otherwise runs the app with the preview disabled.

## Sections

The tabs are the schema’s sections, one for one.

| Tab | Captures |
|----|----|
| **Study** | `study_id`, title, authors, SAP version, description |
| **Data sources** | The databases the study runs against: a name behind an immutable id |
| **Modifications** | Changes applied to a database before analysis, typed by the schema (`trim_observation_period` with a date range) and pointing at the data sources they apply to |
| **Codelists** | A named codelist with its **codes**: an omopgenerics `codelist`, `codelist_with_details` or `concept_set_expression`, uploaded from the csv/json those functions export and carried inside the SAP |
| **Cohorts** | Name, data sources and a **type** that decides the parameters: `concept_cohort` (a codelist, exit and overlap rule for `CohortConstructor::conceptCohort()`), `denominator` and `target_denominator` (the arguments of the IncidencePrevalence generators), or `target` (a cohort instantiated outside the plan) |
| **Analyses** | Name, data sources and a **type** that decides the parameters: `incidence`, `point_prevalence`, `period_prevalence` (IncidencePrevalence), `single_event_survival`, `competing_risk_survival` (CohortSurvival), or `other` |
| **Review** | The problems `checkSap()` and the app’s own checks report, the live JSON, the document preview, and downloads as JSON or Word |

Every item carries an immutable **id** minted when it is added (`ds_1`,
`coh_3`, `an_2`). Every reference between items – a cohort’s codelist,
an analysis’s denominator – is by id, so renaming something never breaks
what points at it. The pickers show names and store ids; a reference to
an item that has since been removed is reported on Review rather than
silently dropped.

Saving sits in the navbar: one file per SAP, created on the first save
(you choose the folder once) and rewritten in place by every save and
autosave after it. **Load SAP** reads a saved file back into every
section, and adopts it as the working file.

## The SAP object

The package is the source of truth. `data-raw/v<version>/schema.csv`
lists every field of every object – its path, value type, whether it is
required, and which collection an id field refers into – and `types.csv`
lists the types a component may take. Both are baked into the package
and read through `sapSchemaVersion()`, `sapSchemaTypes()`,
`sapSchemaFields()` and `sapSchemaVocabulary()`; the app builds its
dropdowns and parameter blocks from them, so a type added to the schema
reaches the UI without an app change.

``` r
library(shinySAP)

study    <- newSapStudy("C1-001", "Prevalence of rare blood cancers", authors = "E. Burn")
source   <- newSapDataSource("ds_1", "CPRD GOLD", structure(list(), class = "data_source_description"))
codelist <- newSapCodelist("cl_1", "Follicular lymphoma", "codelist",
                           omopgenerics::newCodelist(list(follicular_lymphoma = c(4147411L, 4300704L))))
cohort   <- newSapCohort("coh_1", "Follicular lymphoma", "ds_1", "concept_cohort",
                         list(codelist_id = "cl_1", exit = "event_start_date", overlap = "merge"))
denom    <- newSapCohort("coh_2", "General population", "ds_1", "denominator",
                         list(age_group = list(c(0, 150)), sex = "Both",
                              days_prior_observation = 365, requirement_interactions = TRUE))
analysis <- newSapAnalysis("an_1", "Point prevalence", "ds_1", "point_prevalence",
                           list(denominator_cohort_id = "coh_2", outcome_cohort_id = "coh_1",
                                interval = "years", time_point = "start"))

sap <- createSap(study, dataSources = list(source), codelists = list(codelist),
                 cohorts = list(cohort, denom), analyses = list(analysis))

checkSap(sap)             # list() -- no problems
writeSap(sap, "sap.json")
readSap("sap.json")
```

The constructors validate each component as it is built; `createSap()`
and every CRUD function (`addCohort()`, `updateSapComponent()`,
`removeSapComponent()`, …) re-validate the whole document, so a
reference to an id nothing defines, or a removal of something still
referenced, is refused. `newSap(x, validate = FALSE)` builds a
work-in-progress SAP without aborting, and `checkSap()` returns its
problems as a list of `{path, code, message}` – which is how the app
works: it keeps an unvalidated `sap` object and shows `checkSap()`’s
findings on Review.

## The file

`writeSap()` / `readSap()` (and `sapToJson()` / `sapFromJson()` for
strings) serialise the object by the schema: every key is a schema path
and every value is encoded by its value type, so nothing app-specific
ever reaches the file.

``` json
{
  "sap_schema_version": "0.1.0",
  "generated_at": "2026-09-14T11:13:15+0100",
  "study": {
    "study_id": "C1-001",
    "title": "Prevalence of rare blood cancers",
    "authors": ["Edward Burn", "Marti Catala"],
    "version": "v1.0.0"
  },
  "data_sources": [
    { "id": "ds_1", "name": "CPRD GOLD" },
    { "id": "ds_2", "name": "SIDIAP" }
  ],
  "data_source_modifications": [],
  "codelists": [
    {
      "id": "cl_1",
      "name": "Diabetes",
      "type": "codelist",
      "content": { "diabetes": [201826, 443238] }
    }
  ],
  "cohorts": [
    {
      "id": "coh_1",
      "name": "Diabetes",
      "data_source_id": ["ds_1", "ds_2"],
      "type": "concept_cohort",
      "parameters": { "codelist_id": "cl_1", "exit": "event_end_date", "overlap": "merge" }
    },
    {
      "id": "coh_2",
      "name": "General population",
      "type": "denominator",
      "parameters": {
        "age_group": [[0, 17], [18, 150]],
        "sex": "Both",
        "days_prior_observation": 365,
        "requirement_interactions": true
      }
    }
  ],
  "analyses": [
    {
      "id": "an_1",
      "name": "Incidence of diabetes",
      "type": "incidence",
      "parameters": {
        "denominator_cohort_id": "coh_2",
        "outcome_cohort_id": "coh_1",
        "interval": "years",
        "complete_database_intervals": true,
        "washout": 365,
        "repeated_events": false,
        "strata": [["sex"]],
        "include_overall_strata": true
      }
    }
  ]
}
```

The conventions, all decided by the schema’s value types:

- Dates are ISO strings. A `date_range` with one open bound writes
  `null` for that bound (`["2010-01-01", null]`); a range with neither
  bound is omitted.
- `age_group` and `time_at_risk` are lists of `[lower, upper]` pairs; an
  infinite upper bound is `null` inside the pair and reads back as
  `Inf`.
- A scalar day count whose estimator default is `Inf` (`washout`,
  `outcome_washout`, `follow_up_days`, …) is **omitted** when it is
  `Inf`: the generated call leaves the argument out and the package
  default applies.
- A field the author never set is omitted, never written as `null`. An
  empty `parameters` object is `{}`. Vector fields are always arrays.
- A codelist’s `content` is encoded by its `type` – `{"set": [ids]}` for
  a codelist, `{"set": [{row}, ...]}` for a codelist with details or a
  concept set expression – and reads back through the omopgenerics
  constructors, so the object that comes out has the class the schema
  requires.
- Keys the schema does not define survive a read, so `checkSap()` can
  report them as `unknown_field`.

A file written by the standalone app before this rewrite
(`sap_schema_version` `0.4.x`) is refused: there is no migration layer,
and `NEWS.md` keeps that history as the rationale for how the fields
were shaped.

## Generated code

The app turns a SAP into the R that runs it – shown as the last appendix
of the document preview, and written as an OmopStudyBuilder study
directory by `write_study_files()`. Every parameter reaches the call
under its estimator’s argument name, in signature order, and an argument
the author never decided is omitted so the package default applies.
Cohort references are by id in the SAP and by **table name** in the
code: the table is a slug of the cohort’s name (readable for the
reviewer who signs the plan), and two names that collapse to one table
are reported.

| SAP | Generated call |
|----|----|
| codelist | `omopgenerics::importCodelist()` (or the with-details / concept-set-expression reader) from the files the study export writes beside the script |
| `concept_cohort` | `CohortConstructor::conceptCohort(cdm, conceptSet, name, exit, overlap)` |
| `denominator` / `target_denominator` | `IncidencePrevalence::generateDenominatorCohortSet()` / `generateTargetDenominatorCohortSet()` |
| `target` | nothing: the cohort is instantiated outside this script and referred to by table name |
| `incidence`, `point_prevalence`, `period_prevalence` | `IncidencePrevalence::estimateIncidence()` / `estimatePointPrevalence()` / `estimatePeriodPrevalence()` |
| `single_event_survival`, `competing_risk_survival` | `CohortSurvival::estimateSingleEventSurvival()` / `estimateCompetingRiskSurvival()`; `censor_on_date` and `censor_on_date_variable` both feed `censorOnDate`, which takes a Date or a column name |
| `other` | nothing; the analysis is listed with a note |

An item whose `data_source_id` names fewer than all the study’s data
sources is guarded with `omopgenerics::cdmName(cdm) %in% c(...)`, so one
script can ship to every data partner. A guarded estimate yields
`omopgenerics::emptySummarisedResult()` where it does not run, so the
variable exists everywhere and the final `omopgenerics::bind()` +
`suppress(minCellCount = 5)` step never fails on a missing object. The
schema carries no minimum cell count, so the suppression step states
omopgenerics’ default and says so in a comment.

## Validation

Two layers, both reported on **Review** and neither blocking a save – a
SAP is drafted over many sittings and an incomplete one still has to be
checkpointed:

- **Structural**, from `shinySAP::checkSap()`: missing required fields,
  wrong value types, unknown fields, duplicate ids, references to ids
  nothing defines.
- **Semantic**, in `inst/app/R/problems.R`: the handful of checks the
  schema cannot express – that a denominator slot names a denominator,
  that strata are columns the chosen denominator carries and can still
  vary, that a target denominator is not built from itself, that an
  analysis does not run where one of its cohorts is never built, that
  two cohort names do not collapse to one table.

## Adding an analysis type

1.  Add its rows to `data-raw/v<version>/types.csv` and `schema.csv`,
    and rebuild the package data with `Rscript data-raw/sysdata.R`. The
    package now accepts and validates the type;
    `sapSchemaTypes("analysis")` lists it.
2.  Register a template for its parameter block in
    `inst/app/R/analysis_type_<name>.R` with
    `register_analysis_template()`: `ui` builds the inputs (their ids
    **are** the parameter keys), `collect` reads them back into the
    parameters, `flatten` undoes any nesting so a saved file repopulates
    the form. The app refuses to start while a schema type has no
    template.
3.  Add its argument map to `ANALYSIS_ARGS` in `inst/app/R/sap_code.R` –
    the estimator, its package, and schema key -\> argument name in
    signature order.

Cohort types follow the same pattern with `register_cohort_type()` in
`inst/app/R/cohort_kinds.R` and `cohort_r_code()` in `sap_code.R`.

## Layout

One package. The Shiny app lives in `inst/app` as a plain Shiny app
directory: `shiny::runApp()` sources its `R/` folder (alphabetically,
non-recursively) and sets the working directory to it, and everything it
needs from the package it calls as `shinySAP::<fn>()`.

    DESCRIPTION                  package metadata; shiny and the OMOP packages are Suggests
    R/sap.R                      SAP constructors and CRUD
    R/sap_schema.R               schema accessors, checkSap(), validateSap()
    R/sap_json.R                 sapToJson(), sapFromJson(), writeSap(), readSap()
    R/shiny_app.R                shinySap()
    R/sysdata.rda                the built schema tables (see data-raw/)
    data-raw/v<version>/         schema.csv + types.csv, ground truth for what a SAP is
    data-raw/sysdata.R           builds R/sysdata.rda from them

    inst/app/app.R               the Shiny app: ui, server, shinyApp()
    inst/app/R/                  sourced by loadSupport(), alphabetically, before app.R
      analysis_registry.R        analysis template registry and shared input blocks
      analysis_type_*.R          one parameter template per analysis type
      cohort_kinds.R             cohort type templates
      dynamic_items.R            add/remove/duplicate machinery, id minting, pickers
      mod_*.R                    one Shiny module per section
      problems.R                 checkSap() problems regrouped by item, plus semantic checks
      sap_code.R                 SAP -> runnable R
      sap_study_export.R         SAP -> a study directory (scripts and codelist files)
      utils.R                    small helpers
    inst/app/sap_preview.Rmd     SAP -> HTML/Word preview
    inst/app/reference.docx      Word styles (regenerate with data-raw/make_reference_docx.R)

    tests/testthat/              the package suite: test-sap-*.R for the package,
                                 test-app-*.R for the app (helper-app.R sources inst/app/R)
    tests/testthat/fixtures/     sample SAPs in the package schema
    NEWS.md                      package news, then the retired app-private schema history
    scripts/precheck.sh          every gate CI applies, runnable step by step

Each repeating item is a real Shiny module inserted with `insertUI`, not
a re-rendered block, so adding or removing one never resets its
siblings.

## Tests

One suite covers the package and the app. From the repo root:

``` r
devtools::test()
```

or `scripts/precheck.sh`, which also lints, parses every file, installs
the package into a scratch library, smoke-tests `shinySap()` and renders
the preview. The app tests skip where shiny is not installed, and
`test-app-generated-runs.R` – which executes the generated denominator
and incidence code against IncidencePrevalence’s mock database rather
than grepping it – skips where `IncidencePrevalence`, `CDMConnector` or
`duckdb` are absent.
