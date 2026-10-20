# Check a SAP for structural and semantic problems

Combines the structural checks from
[`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md)
with semantic validation (cohort type constraints, strata validity, data
source coverage). Useful for programmatic SAP construction — for example
by an LLM — where the Shiny UI is not available to surface warnings.

## Usage

``` r
sapProblems(sap)
```

## Arguments

- sap:

  A SAP object from
  [`readSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/readSap.md)
  or
  [`createSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/createSap.md).

## Value

A list of problems. Each element is a list with:

- `name`:

  The item or section the problem belongs to.

- `messages`:

  A character vector of problem descriptions.

An empty list means no problems were found.
