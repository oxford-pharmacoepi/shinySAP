# Detect cohorts whose names collide on the same table identifier

Detect cohorts whose names collide on the same table identifier

## Usage

``` r
sapTableNameCollisions(cohorts)
```

## Arguments

- cohorts:

  A list of cohort objects (from a SAP's `$cohorts`).

## Value

A character vector of problem messages; empty when no collision exists.
