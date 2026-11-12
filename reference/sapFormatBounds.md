# Format numeric bound pairs as labels

Turns the `[lower, upper]` pairs a SAP stores for age groups and time at
risk into `"lower, upper"` strings, with an open upper bound shown as
`open`.

## Usage

``` r
sapFormatBounds(pairs, open = "Inf")
```

## Arguments

- pairs:

  A list of length-2 numeric vectors, e.g. `list(c(0, 17), c(18, Inf))`.

- open:

  The label for an open (infinite or missing) upper bound.

## Value

A character vector, one element per pair; empty for no pairs.
