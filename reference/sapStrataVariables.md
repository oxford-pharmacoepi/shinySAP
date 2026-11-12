# Strata variables a cohort can be stratified by

Only denominator-type cohorts carry the columns the strata estimators
need.

## Usage

``` r
sapStrataVariables(cohort)
```

## Arguments

- cohort:

  A cohort object from a SAP's `$cohorts`, or `NULL`.

## Value

A character vector of column names; empty when the cohort is not a
denominator.
