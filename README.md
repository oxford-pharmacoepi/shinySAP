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
| **Cohorts** | Name, role (target / comparator / outcome / strata), cohort ID, entry events, inclusion & exclusion criteria, exit criteria, prior observation, washout, concept set |
| **Proposed Analyses** | Name, analysis type, target / comparator / outcome cohorts, CDM sources it runs on, time at risk, covariates, stratifications, statistical method, effect measure, sensitivity analyses |
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
  "sap_schema_version": "0.2.0",
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
      "role": "Target",
      "cohort_id": 1001,
      "description": "...",
      "entry_events": ["First metformin dispensation"],
      "inclusion_criteria": ["Aged 18 or over at index"],
      "exit_criteria": ["End of continuous observation"],
      "prior_observation_days": 365,
      "washout_days": 365,
      "concept_set": "cs_metformin"
    }
  ],
  "proposed_analyses": [
    {
      "name": "Incidence of lactic acidosis",
      "analysis_type": "Incidence rate",
      "description": "...",
      "target_cohort": "Metformin new users",
      "comparator_cohort": null,
      "outcome_cohort": "Lactic acidosis",
      "data_sources": ["CPRD GOLD"],
      "time_at_risk": {
        "start_offset_days": 1,
        "start_anchor": "cohort start",
        "end_offset_days": 0,
        "end_anchor": "cohort end"
      },
      "covariates": ["Age at index", "Sex"],
      "stratifications": ["Sex"],
      "statistical_method": "Poisson regression",
      "effect_measure": "Incidence rate ratio",
      "sensitivity_analyses": ["30-day washout"]
    }
  ]
}
```

Loading a saved file back into the form (**Review & Save → Load a SAP...**)
repopulates every section, so a SAP can be revised and re-saved.

### Schema versions

`0.2.0` added `cdm_sources` and renamed `analyses` to `proposed_analyses`.
Loading a `0.1.0` file still works — its `analyses` are read into Proposed
Analyses, and saving writes it back out as `0.2.0`.

## Layout

```
app.R                   UI, server, and the reactive that assembles the JSON
R/utils.R               JSON helpers, slugify, save/read
R/dynamic_items.R       add/remove machinery and pickers for repeating sections
R/mod_study.R           Study metadata
R/mod_cdm_sources.R     Section: CDM Sources
R/mod_cdm_changes.R     Section: CDM Changes
R/mod_cohorts.R         Section: Cohorts
R/mod_analyses.R        Section: Proposed Analyses
R/mod_review.R          Review, save, download, load
tests/test_sap_json.R   Round-trip check on the JSON contract
output/                 Saved SAPs
```

Each repeating item is a real Shiny module inserted with `insertUI`, not a
re-rendered block, so adding or removing one never resets its siblings.

## Tests

```sh
Rscript tests/test_sap_json.R
```
