# Read a SAP from a JSON file

Read a SAP from a JSON file

## Usage

``` r
readSap(path, validate = FALSE)
```

## Arguments

- path:

  File to read.

- validate:

  Whether to run
  [`validateSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/validateSap.md)
  on the result. The default is `FALSE` so that a work-in-progress SAP
  can be loaded and its problems reported with
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md).

## Value

A `sap` object.
