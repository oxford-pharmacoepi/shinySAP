# Create a SAP codelist component from an external codelist object

Create a SAP codelist component from an external codelist object

## Usage

``` r
newSapCodelist(id, name = NULL, type = NULL, content = NULL, validate = TRUE)
```

## Arguments

- id:

  Immutable codelist identifier.

- name:

  Display name of the codelist.

- type:

  Codelist object type.

- content:

  External codelist object matching `type`.

- validate:

  Whether to require a complete study. With `FALSE` a draft is built
  from whatever is given and
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md)
  reports what is missing.

## Value

An object of class `sap_codelist`.
