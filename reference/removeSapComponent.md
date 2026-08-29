# Remove a component from a SAP by id

Removal is refused (with a `missing_reference` validation error) while
any other component still references the id.

## Usage

``` r
removeSapComponent(sap, collection, id)
```

## Arguments

- sap:

  A `sap` object.

- collection:

  One of the SAP collection names.

- id:

  The component id.

## Value

The updated `sap` object.
