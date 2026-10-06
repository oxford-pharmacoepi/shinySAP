# Replace the study metadata of a SAP

Replace the study metadata of a SAP

## Usage

``` r
updateStudy(sap, study, validate = TRUE)
```

## Arguments

- sap:

  A `sap` object.

- study:

  A `sap_study` object.

- validate:

  Whether to validate the whole SAP after the change. An id already in
  use is refused either way.

## Value

The updated `sap` object.
