# Add a component to a SAP

Add a component to a SAP

## Usage

``` r
addSapComponent(sap, component, validate = TRUE)
```

## Arguments

- sap:

  A `sap` object.

- component:

  A `sap_*` component object.

- validate:

  Whether to validate the whole SAP after the change. An id already in
  use is refused either way.

## Value

The updated `sap` object.
