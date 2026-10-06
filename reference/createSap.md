# Create a complete SAP from its components

Create a complete SAP from its components

## Usage

``` r
createSap(
  study,
  dataSources = list(),
  dataSourceModifications = list(),
  codelists = list(),
  cohorts = list(),
  analyses = list(),
  validate = TRUE
)
```

## Arguments

- study:

  A `sap_study` object.

- dataSources:

  List of `sap_data_source` objects.

- dataSourceModifications:

  List of `sap_data_source_modification` objects.

- codelists:

  List of `sap_codelist` objects.

- cohorts:

  List of `sap_cohort` objects.

- analyses:

  List of `sap_analysis` objects.

- validate:

  Whether to validate the complete SAP. With `FALSE` the document is
  assembled from drafts and
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md)
  reports its problems.

## Value

An object of class `sap`.
