# Create a data-source modification component

Create a data-source modification component

## Usage

``` r
newSapDataSourceModification(
  id,
  name = NULL,
  type = NULL,
  dataSourceId = character(),
  parameters = list(),
  validate = TRUE
)
```

## Arguments

- id:

  Immutable modification identifier.

- name:

  Display name of the modification.

- type:

  Data-source modification type.

- dataSourceId:

  Identifier of the affected data source(s).

- parameters:

  Type-specific modification parameters.

- validate:

  Whether to require a complete study. With `FALSE` a draft is built
  from whatever is given and
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md)
  reports what is missing.

## Value

An object of class `sap_data_source_modification`.
