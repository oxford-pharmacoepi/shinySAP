# Display label for a SAP item

Display label for a SAP item

## Usage

``` r
sapItemLabel(item, noun)
```

## Arguments

- item:

  A SAP item (cohort, analysis, etc.).

- noun:

  Fallback noun, e.g. `"cohort"`.

## Value

The item's `$name`, or `"Untitled <noun>"` when unnamed.
