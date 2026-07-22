# shinySAP

A Shiny app for writing a Statistical Analysis Plan (SAP) through a structured
form rather than a prose document. Everything the user enters is captured on the
backend as a single JSON dictionary and written to `output/`.

## Sections

| Tab | Captures |
| --- | --- |
| **Study** | Title, study code, authors, SAP version, date, rationale & background, aim / research question, specific objectives, and an amendment history (version, date, description of change) |
| **CDM Sources** | The databases the study runs against — name, short key, country/region |
| **CDM Changes** | Extra CDM validations and database-specific alterations applied before analysis — a change type (extra validation; subset a CDM table; limit observation periods; remap concepts; remove people with no year of birth or sex data; remove people with implausible dates; other), the data sources it applies to, and a description |
| **Codelists** | The concept sets cohorts cite in `[square brackets]` — name, an optional category (Index event, Comorbidity, Medicine, …, which the document groups by), description/provenance, and the codes themselves, uploaded from a `.csv` (concept_id column), `.txt` (one code per line) or `.json` (plain array or Atlas concept-set expression). The codes are stored in the SAP JSON, so the plan is self-contained |
| **Cohorts** | Name, kind and the CDM sources the cohort is built against (the SAP-level counterpart of the generators' `cdm` argument) — plus a set of inputs that **depends on the cohort's kind**. A denominator is *generated*, so it asks for the generator's arguments (cohort date range, age groups, sex, prior observation); a target denominator asks for those plus the target cohort and the time at risk; every other kind (target, outcome, comparator, censoring, strata) is a plain cohort *definition* — entry events (citing the codelist inline in `[square brackets]`), inclusion & exclusion criteria (including the index-date rule), exit criteria |
| **Analyses** | Name, analysis type, CDM sources it runs on — plus a set of inputs that **depends on the analysis type** (an incidence asks for a denominator, a washout and an interval; a prevalence asks for time points instead) |
| **Review** | Live JSON preview, document preview, and download as JSON or Word. Saving sits in the navbar: one file per SAP, created on the first save (you choose the folder once) and rewritten in place by every save and autosave after it — loading a SAP adopts that file |

**Load SAP** sits in the navbar, visible from every tab: it reloads a saved
plan into all sections at once.

CDM sources, CDM changes, cohorts and analyses are repeating sections — use
**Add** to append another, **Remove** to drop one.

Sections cross-reference each other. The cohort pickers on Analyses are
populated from the cohorts you have defined; the data-source pickers on CDM
Changes and Analyses are populated from the CDM Sources tab. All of them
accept free text, so you can reference something you have not written down yet.

## Running

Dependencies are pinned with [renv](https://rstudio.github.io/renv/); the exact
package versions (and the R version they were locked against) live in
`renv.lock`. First time in a fresh clone, restore them:

```r
# in R, with the repo root as the working directory
renv::restore()
shiny::runApp()
```

renv activates automatically through the project `.Rprofile` on every later
session, so after that first restore it is just:

```r
shiny::runApp("path/to/shinySAP")
```

To upgrade dependencies, run `renv::update()` followed by `renv::snapshot()`
and commit the resulting `renv.lock`.

By default the app writes to `output/` relative to the working directory. Point
it somewhere else with:

```r
options(shinySAP.output_dir = "~/saps")
shiny::runApp("path/to/shinySAP")
```

## Output

Files are named `sap-<study-code-or-title-slug>-v<version>.json`, one file per
SAP, rewritten in place by every save and autosave. The shape is stable:
optional text fields serialise to `null` when blank, and list fields are always
JSON arrays even when they hold a single entry.

```json
{
  "sap_schema_version": "0.4.22",
  "generated_at": "2026-07-09T14:02:11+0100",
  "study": {
    "title": "Metformin and lactic acidosis",
    "study_code": "P3-C1-006",
    "authors": ["A. Researcher"],
    "version": "1.1",
    "min_cell_count": 5,
    "date": "2026-07-09",
    "background": "...",
    "aim": "The aim of this study is to ...",
    "objectives": [
      { "id": "obj_1", "text": "Estimate the incidence of lactic acidosis" }
    ],
    "amendments": [
      { "version": "1.1", "date": "2026-07-09",
        "description": "Added the SIDIAP data source." }
    ]
  },
  "cdm_sources": [
    {
      "name": "CPRD GOLD",
      "source_key": "cprd",
      "country": "United Kingdom"
    }
  ],
  "cdm_changes": [
    {
      "change_type": "Subset a CDM table",
      "data_sources": ["cprd"],
      "description": "Restrict drug_exposure to records with a valid quantity."
    }
  ],
  "codelists": [
    {
      "name": "cs_metformin",
      "category": "Medicine",
      "description": "CodelistGenerator, ATC A10BA02",
      "source_file": "metformin_codes.csv",
      "vocabulary_version": "v5.0 31-AUG-23",
      "concept_set_expression": [
        { "concept_id": "1503297", "excluded": false,
          "descendants": true, "mapped": false }
      ],
      "codes": [
        { "code": "1503297", "name": "metformin" },
        { "code": "40164929", "name": "metformin hydrochloride 500 MG Oral Tablet" }
      ]
    }
  ],
  "cohorts": [
    {
      "name": "Metformin new users",
      "kind": "target",
      "data_sources": ["cprd"],
      "entry_events": ["Metformin dispensation [cs_metformin]"],
      "inclusion_criteria": ["Index on the first occurrence ever",
                             "Aged 18 or over at index"],
      "exit_criteria": ["End of continuous observation"],
      "operations": [
        { "op": "concept_cohort", "codelist": "cs_metformin" },
        { "op": "require_first_entry" },
        { "op": "require_demographics", "age_range": [[18, 150]] },
        { "op": "exit_at_observation_end" }
      ]
    },
    {
      "name": "Metformin denominator",
      "kind": "target_denominator",
      "data_sources": ["cprd"],
      "targetCohortTable": "Metformin new users",
      "cohortDateRange": ["2015-01-01", "2024-12-31"],
      "timeAtRisk": [[0, 30], [31, null]],
      "ageGroup": [[0, 17], [18, 64], [65, 150]],
      "sex": ["Both"],
      "daysPriorObservation": [365],
      "requirementsAtEntry": true,
      "requirementInteractions": true
    }
  ],
  "proposed_analyses": [
    {
      "name": "Incidence of lactic acidosis",
      "analysis_type": "Incidence",
      "objectives": ["obj_1"],
      "data_sources": ["cprd"],
      "parameters": {
        "denominatorTable": "Metformin denominator",
        "outcomeTable": "Lactic acidosis",
        "censorTable": null,
        "denominatorCohortId": null,
        "outcomeCohortId": null,
        "censorCohortId": null,
        "interval": ["years"],
        "completeDatabaseIntervals": true,
        "outcomeWashout": [365],
        "repeatedEvents": false,
        "strata": [["sex"], ["sex", "age_group"]],
        "includeOverallStrata": true
      }
    }
  ]
}
```

An analysis carries four keys of its own — `name`, `analysis_type`, `objectives`
(the objectives it answers, many-to-many) and `data_sources` — and everything
else under `parameters`. Which keys appear there
is decided by `analysis_type`, so a reader can tell "no comparator, because this
is an incidence analysis" from "the comparator was left blank".

**A cohort's `kind` decides what it carries**, because the kinds are not the same
object. A denominator is a cohort *set* produced by
`generateDenominatorCohortSet()`; a `target_denominator` is produced by
`generateTargetDenominatorCohortSet()`. **A denominator kind's keys are its
generator's argument names, in the generator's own order** — so the JSON reads as
the call it describes, and nothing can drift:

| `denominator` | `target_denominator` |
| --- | --- |
| | `targetCohortTable` |
| `cohortDateRange` | `cohortDateRange` |
| | `timeAtRisk` |
| `ageGroup` | `ageGroup` |
| `sex` | `sex` |
| `daysPriorObservation` | `daysPriorObservation` |
| | `requirementsAtEntry` |
| `requirementInteractions` | `requirementInteractions` |

`cdm` is a live database handle, not a plan field, and `name` is the cohort's own
name in the common half of the card, so neither is repeated in the block.
`targetCohortId` is not captured yet — cohort IDs are handled internally for now.
Neither kind carries entry criteria: a denominator cohort set is *generated*, not
defined by them. Every other kind — `target`, `outcome`, `comparator`, `censor`,
`strata` — is a plain cohort *definition*: entry events, inclusion and exit
criteria, a concept set, and none of the generator arguments.

The distinction that matters most is between a **target** and a **target
denominator**. A target cohort is defined by entry criteria ("first metformin
dispensation"); a target denominator is *generated from* that target, restricted
to the time each person spends in it. They are two entries, not one — which is
why the example above has both.

**Time at risk belongs to the denominator, not the analysis**, and so do the
cohort date range, the age groups and the sex. Two analyses on the same
denominator cannot disagree about them, and an analysis inherits them rather than
restating them — the Analyses tab shows a read-only echo of what the chosen
denominator already fixes.

**`cohortDateRange` is ONE key holding TWO dates**, because that is what the
argument takes — its default is literally `as.Date(c(NA, NA))`. A missing bound is
`null`, meaning exactly what `NA` means to the generator: use the earliest (or
latest) observation period in the database. So `["2015-01-01", null]` is
"from 2015 to the end of the data", and `[null, null]` is the argument's own
default.

**`ageGroup` and `timeAtRisk` are lists of numeric pairs** — `[[0, 17], [18,
64]]` and `[[0, 30], [31, null]]` — matching `ageGroup = list(c(0, 17), c(18,
64))` and `timeAtRisk = list(c(0, 30), c(31, Inf))`. Each interval generates its
own cohort. JSON has no `Infinity`, so an unbounded upper bound is written
`null`: `[31, null]` is "day 31 onwards". Both of a time-at-risk pair's bounds are
counted in days from *target cohort entry* — there is no anchoring on cohort end,
which is why the pre-0.3.2 anchors could not survive the migration.

**The Incidence `parameters` map 1:1 onto
`IncidencePrevalence::estimateIncidence()`** — the keys *are* the argument names,
flat and in the function's own order, with no wrapper object. If a field is not one
of that function's arguments, it is not part of an Incidence analysis. So there is
no "rate per 1,000" and no denominator unit — those are presentation choices made
downstream when the result is tabled — and no sensitivity-analysis list, because
"re-run with a 30-day washout" is a second call, not an argument to this one. `cdm`
is a runtime database handle, so it is the only argument absent. The three
`*CohortId` arguments select which cohorts of a set to use; `null` (the default)
means all of them.

**`strata` is a list of variable groups**, naming columns on the denominator
cohort: `[["sex"], ["sex", "age_group"]]` means one stratification by sex and
another by the cross of sex and age group — exactly
`strata = list("sex", c("sex", "age_group"))`. The only columns available are
`age_group` and `sex` — the ones `generateDenominatorCohortSet()` puts on the
denominator table — and an analysis may only stratify by those. This is **not** a
field on the cohort: the generator makes those two columns and no others, so there
is nothing for an author to decide, and the strata picker on an analysis offers
exactly them.

**`outcomeWashout` is a number of days**, matching
`estimateIncidence(outcomeWashout =)`, which takes a number and defaults to `Inf`.
It is written as a **one-element numeric array**, for the same reason
`timeAtRisk` is a list of pairs: JSON has no `Infinity`, so `Inf` travels as
`null` *inside* an array, leaving a bare `null` to keep its schema-wide meaning of
"absent".

| JSON | Means |
| --- | --- |
| `[365]` | a 365-day washout |
| `[0]` | no washout |
| `[null]` | `Inf` — unbounded, one event per person |
| `null` | the author never said |

All three of the first rows are numbers, and all four states are distinct — which
is what the incidence validator needs. A 0-day washout is a substantively
different analysis from an unstated one, and an unbounded washout is different
again. `estimateIncidence()` defaults to `Inf`, but the SAP deliberately refuses
to inherit that silently and makes the author choose.

Loading a saved file back into the form (**Load SAP**, in the navbar)
repopulates every section, so a SAP can be revised and re-saved.

### Schema versions

`0.2.0` added `cdm_sources` and renamed `analyses` to `proposed_analyses`.
Loading a `0.1.0` file still works — its `analyses` are read into Proposed
Analyses.

`0.3.0` gave each analysis type its own set of inputs and moved the
type-specific fields under `parameters`. It also replaced a cohort's `role`
(Target / Comparator / …) with `kind` (`denominator`, `target_denominator`, …),
and moved `time_at_risk` from the analysis onto the cohort.

Older files still load. `migrate_sap()` in `R/utils.R` runs before any section
does: a cohort's old `role` is aliased to a `kind`; an analysis with no
`parameters` is read from its top-level keys; `"Incidence rate"` is read as
`"Incidence"`; and the generic form's `target_cohort` is read as the incidence
`denominator_cohort`. (`0.3.2` below revised how `time_at_risk` is carried across,
so the account there supersedes this one.)

`0.3.1` made the Incidence `parameters` map 1:1 onto `estimateIncidence()`: it
added `strata` (as variable groups) and `include_overall_strata`, and dropped
`reporting` (`denominator_unit`, `rate_multiplier`) and `sensitivity_analyses`,
none of which are arguments to that function. A `0.3.0` file still loads — those
fields are simply discarded.

`0.3.2` did the same for cohorts: a cohort's `kind` now decides which fields it
carries, and each denominator kind carries exactly its generator's arguments.
`study_period_*` became `cohort_date_range_*`; `prior_observation_days` became
`days_prior_observation` (a list, since the generator takes a vector);
`age_groups` became numeric pairs rather than free text; and
`requirement_interactions`, `requirements_at_entry` and `strata_variables` were
added. `washout_days` was **dropped** — neither generator takes a washout, it is
`estimateIncidence(outcomeWashout =)`, which the Incidence analysis already
captures.

`0.3.2` also made the Incidence `outcome_washout` **numeric**, matching
`estimateIncidence(outcomeWashout =)`. It was the string `"unbounded"` (or a bare
number); it is now a one-element numeric array, with `Inf` as `[null]` — the same
Inf-as-null-inside-an-array rule the cohort's `age_groups` and `time_at_risk` use.
Both older shapes still load.

`0.4.0` added cohort sets — a cohort gained `parent_cohort`, and prevalence
gained `denominatorCohortId` / `outcomeCohortId` (null = all IDs in the set) —
and aligned Prevalence with the estimators the way `0.3.1` aligned Incidence:
`parameters` use the argument names and order, `strata` replaces
`stratifications`, `sensitivity_analyses` is dropped there, and a prevalence
`analysis_type` names the estimator (`estimatePointPrevalence` /
`estimatePeriodPrevalence`).

`0.4.1` **dropped `strata_variables`** from the denominator kinds.
`generateDenominatorCohortSet()` puts `age_group` and `sex` on the denominator
table and nothing else, so the columns an analysis may stratify by were never the
author's to choose. As a textarea it could only agree with that or be wrong — and
being wrong was the dangerous case: naming an extra ETL column made the strata
picker offer it and the validator accept it, for a `strata =` that the generator
would never satisfy. The columns are now fixed (`STRATA_VARIABLES` in
`R/cohort_kinds.R`). An older file's declared columns are dropped on load, and an
analysis stratified by a column beyond those two now fails validation — which is
the honest outcome, since it was never going to run.

`0.4.2` **aligned the denominator kinds with their generators**, the way `0.3.1`
aligned Incidence with `estimateIncidence()` and `0.4.0` aligned Prevalence with
the prevalence estimators. Every key is now the argument name it feeds, in the
generator's own order: `target_cohort` → `targetCohortTable`, `time_at_risk` →
`timeAtRisk`, `age_groups` → `ageGroup`, `days_prior_observation` →
`daysPriorObservation`, `requirements_at_entry` → `requirementsAtEntry`,
`requirement_interactions` → `requirementInteractions`. The two keys
`cohort_date_range_start` / `_end` became the single `cohortDateRange` pair the
argument actually takes. The input ids are the argument names too, so a field and
the key it produces cannot drift apart. Older files still load: `migrate_cohort()`
renames the old keys and folds the two date keys into the pair.

`0.4.3` finished aligning **Incidence** the same way. `0.3.1` had matched the *set*
of fields to `estimateIncidence()` but still wrote them snake_case and wrapped in
an `estimand` object the function has no concept of. Now the keys are the argument
names, flat and in signature order — `denominator_cohort` → `denominatorTable`,
`outcome_cohort` → `outcomeTable`, `censor_cohort` → `censorTable`,
`complete_database_intervals` → `completeDatabaseIntervals`, `outcome_washout` →
`outcomeWashout`, `repeated_events` → `repeatedEvents`, `include_overall_strata` →
`includeOverallStrata`, and `interval` / `strata` unchanged. The `estimand` wrapper
is gone, and the three `*CohortId` arguments (`denominatorCohortId`,
`outcomeCohortId`, `censorCohortId`; `null` = all cohorts in the set) are now
present, matching the signature and the Prevalence template. The Incidence
`flatten()` un-nests any old `estimand` and renames every field, so a pre-0.4.3
file loads unchanged.

Older files still load, and `migrate_sap()` in `R/utils.R` runs before any
section does. Beyond the aliasing above, it repairs two things it cannot leave
alone:

- A pre-0.3.2 analysis named a **plain target cohort** as its denominator and
  carried its own `time_at_risk`. IncidencePrevalence has no such object, so the
  missing denominator is *synthesised* — one per (target cohort, time at risk)
  pair, since that is exactly what one generator call produces — and the analysis
  is repointed at it. Two analyses sharing a target and a window therefore share
  one denominator; a different window gets its own. This cannot be done in a
  template's `flatten()`, which can only rewrite its own analysis, not add a
  cohort.
- The old anchored `time_at_risk` (`start_offset_days`, `start_anchor`,
  `end_offset_days`, `end_anchor`) becomes a single `[start, end]` interval. The
  anchors are dropped: the API has nowhere to put them, because both bounds are
  relative to target cohort entry.

An old cohort `role` of `Target` becomes the `target` **kind — a plain cohort**,
not a denominator. Mapping it to one would silently discard its entry events,
inclusion criteria and concept set, which a denominator's block does not carry.

Saving writes the current schema version (`0.4.0`) back out.

### Validation

A template may declare a `validate(params, cohorts)` that returns a character
vector of problems — for instance, that an incidence analysis's denominator is
not actually a denominator cohort, or that it stratifies by sex on a male-only
cohort. Problems are listed on **Review**. They do **not** block saving: a
SAP is drafted over many sittings and an incomplete one still has to be
checkpointed, so saving with outstanding problems warns rather than refuses.

### Adding an analysis type's form

Each type's inputs live in their own file, `R/analysis_type_<name>.R`, which
calls `register_analysis_template()` with four mirrored pieces: `ui` builds the
inputs, `collect` reads them back into JSON, `pickers` names the ids that pick a
cohort or a CDM source, and `flatten` undoes whatever nesting `collect` did so a
saved file can repopulate the form. Copy an existing one and edit it — that is
the whole wiring, no other file needs touching.

A type listed in `ANALYSIS_TYPES` with no template of its own falls back to
`"Other"`, the generic form, so the app keeps working while the rest are filled
in. `R/analysis_registry.R` holds the registry, the type resolver, and the input
blocks templates share (time at risk, stratifications); its header comment sets
out the rules, including why the template files have to sit flat in `R/`.

## Layout

```
app.R                     UI, server, and the reactive that assembles the JSON
R/utils.R                 JSON helpers, slugify, save/read
R/dynamic_items.R         add/remove machinery and pickers for repeating sections
R/analysis_registry.R     analysis type registry, resolver, shared input blocks
R/analysis_type_*.R       one input template per analysis type
R/mod_study.R             Study metadata
R/mod_cdm_sources.R       Section: CDM Sources
R/mod_cdm_changes.R       Section: CDM Changes
R/mod_cohorts.R           Section: Cohorts
R/mod_analyses.R          Section: Analyses
R/mod_review.R            Review, save, download
tests/testthat/           testthat suite: JSON contract, templates, migrations
tests/testthat.R          runner (Rscript tests/testthat.R)
output/                   Saved SAPs
```

Each repeating item is a real Shiny module inserted with `insertUI`, not a
re-rendered block, so adding or removing one never resets its siblings.

## Tests

The suite needs `testthat`, which `renv::restore()` installs along with
everything else. From the repo root:

```sh
Rscript tests/testthat.R
```
