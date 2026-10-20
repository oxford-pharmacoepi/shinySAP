# Generate the labelled code blocks for a SAP

The blocks are ordered: Libraries, Codelists, Cohorts, Denominator
cohort sets, Estimates, Result suppression. Each entry is a list with
fields `group`, `title`, `code`, `note`, and `pkgs`.

## Usage

``` r
sapScriptSections(sap)
```

## Arguments

- sap:

  A SAP object from
  [`readSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/readSap.md)
  or the CRUD constructors.

## Value

A list of block entries.
