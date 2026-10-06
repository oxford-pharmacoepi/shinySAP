# Serialise a SAP to JSON

Serialise a SAP to JSON

## Usage

``` r
sapToJson(sap, pretty = TRUE, stamp = FALSE)
```

## Arguments

- sap:

  A `sap` object or a named list in SAP shape.

- pretty:

  Whether to indent the output.

- stamp:

  Whether to set `generated_at` to now in the output. `FALSE` keeps the
  value the object carries.

## Value

A JSON string (class `json`).
