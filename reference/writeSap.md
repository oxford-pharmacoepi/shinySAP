# Write a SAP to a JSON file

Write a SAP to a JSON file

## Usage

``` r
writeSap(sap, path, stamp = TRUE)
```

## Arguments

- sap:

  A `sap` object or a named list in SAP shape.

- path:

  File to write; its directory is created if needed.

- stamp:

  Whether to set `generated_at` to now in the output. `FALSE` keeps the
  value the object carries.

## Value

The path, invisibly.
