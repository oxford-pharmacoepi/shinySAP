# Create a SAP analysis component

Create a SAP analysis component

## Usage

``` r
newSapAnalysis(id, name, dataSourceId, type, parameters = list())
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

## Value

An object of class `sap_analysis`.
