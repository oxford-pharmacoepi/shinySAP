# Import a SAP from a JSON file

Import a SAP from a JSON file

## Usage

``` r
importSap(path, validate = TRUE)
```

## Arguments

- path:

  File to read.

- validate:

  Whether to run
  [`validateSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/validateSap.md)
  on the result. `FALSE` loads a work-in-progress SAP so its problems
  can be reported with
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md).

## Value

A `sap` object.
