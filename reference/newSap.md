# Create a structured Statistical Analysis Plan

Create a structured Statistical Analysis Plan

## Usage

``` r
newSap(x, sapSchemaVersion = currentSapSchemaVersion(), validate = TRUE)
```

## Arguments

- x:

  A named R list containing the SAP fields.

- sapSchemaVersion:

  A valid SAP schema version, it must be a choice of
  [`supportedSapSchemaVersions()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/supportedSapSchemaVersions.md).
  Current recommended version: "0.1.0".

- validate:

  Whether to validate the complete SAP before returning it.

## Value

An object of class `sap`.
