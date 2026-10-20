# Convert a cohort display name to its generated table identifier

Convert a cohort display name to its generated table identifier

## Usage

``` r
cohortTableName(x)
```

## Arguments

- x:

  A character string; a display name.

## Value

A valid, lowercase identifier at most 63 characters long, or
`NA_character_` when `x` cannot be made into one.
