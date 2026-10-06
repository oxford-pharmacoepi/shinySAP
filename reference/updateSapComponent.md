# Replace a component in a SAP, matched by id

Replace a component in a SAP, matched by id

## Usage

``` r
updateSapComponent(sap, component, validate = TRUE)
```

## Arguments

- sap:

  A `sap` object.

- component:

  A `sap_*` component whose id already exists in the SAP.

- validate:

  Whether to validate the whole SAP after the change. An id already in
  use is refused either way.

## Value

The updated `sap` object.
