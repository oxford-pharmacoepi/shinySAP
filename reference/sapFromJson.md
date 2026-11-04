# Read a SAP from JSON

Read a SAP from JSON

## Usage

``` r
sapFromJson(json, validate = TRUE)
```

## Arguments

- json:

  A JSON string.

- validate:

  Whether to run
  [`validateSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/validateSap.md)
  on the result. `FALSE` loads a work-in-progress SAP so its problems
  can be reported with
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md).

## Value

A `sap` object.
