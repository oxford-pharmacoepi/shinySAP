# Read a SAP from JSON

Read a SAP from JSON

## Usage

``` r
sapFromJson(json, validate = FALSE)
```

## Arguments

- json:

  A JSON string.

- validate:

  Whether to run
  [`validateSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/validateSap.md)
  on the result. The default is `FALSE` so that a work-in-progress SAP
  can be loaded and its problems reported with
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md).

## Value

A `sap` object.
