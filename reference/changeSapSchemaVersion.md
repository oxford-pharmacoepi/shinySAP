# Change the SAP schema version of a sap obejct

Change the SAP schema version of a sap obejct

## Usage

``` r
changeSapSchemaVersion(sap, sapSchemaVersion = currentSapSchemaVersion())
```

## Arguments

- sap:

  An `omop_sap` object.

- sapSchemaVersion:

  A valid SAP schema version, it must be a choice of
  [`supportedSapSchemaVersions()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/supportedSapSchemaVersions.md).
  Current recommended version: "0.1.0".

## Value

The original `sap` object addapted to the new schema.
