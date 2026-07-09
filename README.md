# shinySAP

A Shiny app for writing a Statistical Analysis Plan (SAP) through a structured
form rather than a prose document. Everything the user enters is captured on the
backend as a single JSON dictionary and written to `output/`.

## Sections

| Tab | Captures |
| --- | --- |
| **Study** | Title, acronym, authors, SAP version, date, background, objectives |
| **CDM Changes** | Changes to the common data model the study depends on — table, field, change type, CDM version, data source, description, rationale |
| **Cohorts** | Name, role (target / comparator / outcome / strata), cohort ID, entry events, inclusion & exclusion criteria, exit criteria, prior observation, washout, concept set |
| **Analyses** | Name, analysis type, target / comparator / outcome cohorts, time at risk, covariates, stratifications, statistical method, effect measure, sensitivity analyses |
| **Review & Save** | Live JSON preview, save to `output/`, download, and reload a saved SAP |

CDM changes, cohorts and analyses are repeating sections — use **Add** to append
another, **Remove** to drop one. The cohort pickers in the Analyses tab are
populated from the cohorts you have defined, but accept free text so you can
reference a cohort you have not written down yet.

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
  "sap_schema_version": "0.1.0",
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
  "analyses": [
    {
      "name": "Incidence of lactic acidosis",
      "analysis_type": "Incidence rate",
      "description": "...",
      "target_cohort": "Metformin new users",
      "comparator_cohort": null,
      "outcome_cohort": "Lactic acidosis",
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

## Layout

```
app.R                   UI, server, and the reactive that assembles the JSON
R/utils.R               JSON helpers, slugify, save/read
R/dynamic_items.R       add/remove machinery for the repeating sections
R/mod_study.R           Study metadata
R/mod_cdm_changes.R     Section: CDM Changes
R/mod_cohorts.R         Section: Cohorts
R/mod_analyses.R        Section: Analyses
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
