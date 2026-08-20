# Create a SAP cohort component

Create a SAP cohort component

## Usage

``` r
newSapCohort(id, name, dataSourceId, type, parameters = list())
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

## Value

An object of class `sap_cohort`.
