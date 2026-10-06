# Create a SAP cohort component

Create a SAP cohort component

## Usage

``` r
newSapCohort(
  id,
  name = NULL,
  dataSourceId = character(),
  type = NULL,
  parameters = list(),
  validate = TRUE
)
```

## Arguments

- id:

  Immutable cohort identifier.

- name:

  Display name of the cohort.

- dataSourceId:

  Identifier of the data source(s) used by the cohort.

- type:

  Cohort type.

- parameters:

  Type-specific cohort parameters.

- validate:

  Whether to require a complete study. With `FALSE` a draft is built
  from whatever is given and
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md)
  reports what is missing.

## Value

An object of class `sap_cohort`.
