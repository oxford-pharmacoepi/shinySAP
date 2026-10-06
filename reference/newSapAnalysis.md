# Create a SAP analysis component

Create a SAP analysis component

## Usage

``` r
newSapAnalysis(
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

  Immutable analysis identifier.

- name:

  Display name of the analysis.

- dataSourceId:

  Identifier of the data source(s) used by the analysis.

- type:

  Analysis type.

- parameters:

  Type-specific analysis parameters.

- validate:

  Whether to require a complete study. With `FALSE` a draft is built
  from whatever is given and
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md)
  reports what is missing.

## Value

An object of class `sap_analysis`.
