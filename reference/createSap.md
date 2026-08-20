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
  analyses = list()
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

## Value

An object of class `sap`.
