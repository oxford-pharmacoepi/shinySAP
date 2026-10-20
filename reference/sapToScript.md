# Render a SAP as a single runnable R script

Render a SAP as a single runnable R script

## Usage

``` r
sapToScript(sap)
```

## Arguments

- sap:

  A SAP object from
  [`readSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/readSap.md)
  or the CRUD constructors.

## Value

A single character string containing a runnable R script, or `""` when
the SAP has no codable content.
