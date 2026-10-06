# Create a SAP data-source component

Create a SAP data-source component

## Usage

``` r
newSapDataSource(id, name = NULL, description = NULL, validate = TRUE)
```

## Arguments

- id:

  Immutable data-source identifier.

- name:

  Display name of the data source.

- description:

  An optional `data_source_description` object.

- validate:

  Whether to require a complete study. With `FALSE` a draft is built
  from whatever is given and
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md)
  reports what is missing.

## Value

An object of class `sap_data_source`.
