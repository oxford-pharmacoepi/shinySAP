# shinySAP

A Shiny app for writing a Statistical Analysis Plan (SAP) through a structured
form rather than a prose document. Everything the user enters is captured on the
backend as a single JSON dictionary and written to `output/`.

## Sections

| Tab | Captures |
| --- | --- |
| **Study** | Title, acronym, authors, SAP version, date, background, objectives |
| **CDM Sources** | The databases the study runs against — name, short key, data type, country, custodian, population size, CDM & vocabulary version, snapshot/release, data lock point, observation period, description |
| **CDM Changes** | Changes to the common data model the study depends on — table, field, change type, CDM version, data source, description, rationale |
| **Cohorts** | Name, cohort ID, description — plus a set of inputs that **depends on the cohort's kind**. A denominator is *generated*, so it asks for the generator's arguments (cohort date range, age groups, sex, prior observation); a target denominator asks for those plus the target cohort and the time at risk; every other kind (target, outcome, comparator, censoring, strata) is a plain cohort *definition* — entry events, inclusion & exclusion criteria, exit criteria, concept set |
| **Proposed Analyses** | Name, analysis type, CDM sources it runs on — plus a set of inputs that **depends on the analysis type** (an incidence asks for a denominator, a washout and an interval; a prevalence asks for time points instead) |
| **Review & Save** | Live JSON preview, save to `output/`, download, and reload a saved SAP |

CDM sources, CDM changes, cohorts and analyses are repeating sections — use
**Add** to append another, **Remove** to drop one.

Sections cross-reference each other. The cohort pickers on Proposed Analyses are
populated from the cohorts you have defined; the data-source pickers on CDM
Changes and Proposed Analyses are populated from the CDM Sources tab. All of them
accept free text, so you can reference something you have not written down yet.

## Running

```r
install.packages(c("shiny", "bslib", "jsonlite"))
shiny::runApp("path/to/shinySAP")
```

By default the app writes to `output/` relative to the working directory. Point
it somewhere else with:

```r
options(shinySAP.output_dir = "~/saps")
shiny::runApp("path/to/shinySAP")
```

## Output

Files are named `sap-<study-title-slug>-<timestamp>.json`. The shape is stable:
optional text fields serialise to `null` when blank, and list fields are always
JSON arrays even when they hold a single entry.

```json
{
  "sap_schema_version": "0.4.0",
  "generated_at": "2026-07-09T14:02:11+0100",
  "study": {
    "title": "Metformin and lactic acidosis",
    "acronym": "MALA",
    "authors": ["A. Researcher"],
    "version": "1.0",
    "date": "2026-07-09",
    "background": "...",
    "objectives": ["Estimate the incidence of lactic acidosis"]
  },
  "cdm_sources": [
    {
      "name": "CPRD GOLD",
      "source_key": "cprd",
      "data_type": "Primary care records",
      "country": "United Kingdom",
      "custodian": "MHRA",
      "population_size": 12000000,
      "cdm_version": "5.4",
      "vocabulary_version": "v5.0 30-AUG-24",
      "release_date": "2026-01-15",
      "data_lock": "2025-12-31",
      "observation_period_start": "1987-01-01",
      "observation_period_end": "2025-12-31",
      "description": "..."
    }
  ],
  "cdm_changes": [
    {
      "cdm_table": "drug_exposure",
      "cdm_field": "days_supply",
      "change_type": "ETL fix",
      "cdm_version": "5.4",
      "data_source": "CPRD GOLD",
      "description": "Impute missing days_supply from quantity.",
      "rationale": "12% of records are null."
    }
  ],
  "cohorts": [
    {
      "name": "Metformin new users",
      "kind": "target",
      "cohort_id": 1001,
      "description": "...",
      "entry_events": ["First metformin dispensation"],
      "inclusion_criteria": ["Aged 18 or over at index"],
      "exit_criteria": ["End of continuous observation"],
      "concept_set": "cs_metformin"
    },
    {
      "name": "Metformin denominator",
      "kind": "target_denominator",
      "cohort_id": 1002,
      "description": "...",
      "target_cohort": "Metformin new users",
      "time_at_risk": [[0, 30], [31, null]],
      "requirements_at_entry": true,
      "cohort_date_range_start": "2015-01-01",
      "cohort_date_range_end": "2024-12-31",
      "age_groups": [[0, 17], [18, 64], [65, 150]],
      "sex": ["Both"],
      "days_prior_observation": [365],
      "requirement_interactions": true
    }
  ],
  "proposed_analyses": [
    {
      "name": "Incidence of lactic acidosis",
      "analysis_type": "Incidence",
      "data_sources": ["CPRD GOLD"],
      "parameters": {
        "denominator_cohort": "Metformin denominator",
        "outcome_cohort": "Lactic acidosis",
        "censor_cohort": null,
        "estimand": {
          "interval": ["years"],
          "complete_database_intervals": true,
          "outcome_washout": [365],
          "repeated_events": false,
          "strata": [["sex"], ["sex", "age_group"]],
          "include_overall_strata": true
        }
      }
    }
  ]
}
```

An analysis carries three keys of its own — `name`, `analysis_type` and
`data_sources` — and everything else under `parameters`. Which keys appear there
is decided by `analysis_type`, so a reader can tell "no comparator, because this
is an incidence analysis" from "the comparator was left blank".

**A cohort's `kind` decides what it carries**, because the kinds are not the same
object. A denominator is a cohort *set* produced by
`generateDenominatorCohortSet()`, so it carries that function's arguments —
`cohort_date_range`, `age_groups`, `sex`, `days_prior_observation`,
`requirement_interactions` — and no entry criteria, because nothing about it is
defined by them. A `target_denominator` is produced by
`generateTargetDenominatorCohortSet()`, so it carries the same arguments plus the
`target_cohort` it is generated from, the `time_at_risk`, and
`requirements_at_entry`. Every other kind — `target`, `outcome`, `comparator`,
`censor`, `strata` — is a plain cohort *definition*: entry events, inclusion and
exit criteria, a concept set, and none of the generator arguments.

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

**`age_groups` and `time_at_risk` are lists of numeric pairs** — `[[0, 17], [18,
64]]` and `[[0, 30], [31, null]]` — matching `ageGroup = list(c(0, 17), c(18,
64))` and `timeAtRisk = list(c(0, 30), c(31, Inf))`. Each interval generates its
own cohort. JSON has no `Infinity`, so an unbounded upper bound is written
`null`: `[31, null]` is "day 31 onwards". Both of a time-at-risk pair's bounds are
counted in days from *target cohort entry* — there is no anchoring on cohort end,
which is why the pre-0.3.2 anchors could not survive the migration.

**The Incidence `parameters` map 1:1 onto
`IncidencePrevalence::estimateIncidence()`.** If a field is not one of that
function's arguments, it is not part of an Incidence analysis. So there is no
"rate per 1,000" and no denominator unit — those are presentation choices made
downstream when the result is tabled — and no sensitivity-analysis list, because
"re-run with a 30-day washout" is a second call, not an argument to this one. The
three `*CohortId` arguments are absent too: a cohort's id belongs to the cohort,
and the Cohorts tab already carries it.

**`strata` is a list of variable groups**, naming columns on the denominator
cohort: `[["sex"], ["sex", "age_group"]]` means one stratification by sex and
another by the cross of sex and age group — exactly
`strata = list("sex", c("sex", "age_group"))`. The only columns available are
`age_group` and `sex` — the ones `generateDenominatorCohortSet()` puts on the
denominator table — and an analysis may only stratify by those. This is **not** a
field on the cohort: the generator makes those two columns and no others, so there
is nothing for an author to decide, and the strata picker on an analysis offers
exactly them.

**`outcome_washout` is a number of days**, matching
`estimateIncidence(outcomeWashout =)`, which takes a number and defaults to `Inf`.
It is written as a **one-element numeric array**, for the same reason
`time_at_risk` is a list of pairs: JSON has no `Infinity`, so `Inf` travels as
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

Loading a saved file back into the form (**Review & Save → Load a SAP...**)
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
cohort. Problems are listed on **Review & Save**. They do **not** block saving: a
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
R/mod_analyses.R          Section: Proposed Analyses
R/mod_review.R            Review, save, download, load
tests/testthat/           testthat suite: JSON contract, templates, migrations
tests/testthat.R          runner (Rscript tests/testthat.R)
output/                   Saved SAPs
```

Each repeating item is a real Shiny module inserted with `insertUI`, not a
re-rendered block, so adding or removing one never resets its siblings.

## Tests

The suite needs `testthat` (`install.packages("testthat")`). From the repo root:

```sh
Rscript tests/testthat.R
```
