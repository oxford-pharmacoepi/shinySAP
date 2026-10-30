# Supported SAP schema versions

These are all the supported SAP schema versions, although the latest is
always recommended. See latest with
[`currentSapSchemaVersion()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/currentSapSchemaVersion.md).
Use
[`changeSapSchemaVersion()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/changeSapSchemaVersion.md)
to change the version of a `sap` object.

## Usage

``` r
supportedSapSchemaVersions()
```

## Value

All versions of the SAP that are supported

## Examples

``` r
library(shinySAP)

supportedSapSchemaVersions()
#> [1] "0.1.0"
```
