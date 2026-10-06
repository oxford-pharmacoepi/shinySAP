# Remove a component from a SAP by id

With `validate = TRUE` removal is refused (with a `missing_reference`
validation error) while any other component still references the id.
With `validate = FALSE` the component is removed and
[`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md)
reports the dangling reference.

## Usage

``` r
removeSapComponent(sap, collection, id, validate = TRUE)
```

## Arguments

- sap:

  A `sap` object.

- collection:

  One of the SAP collection names.

- id:

  The component id.

- validate:

  Whether to validate the whole SAP after the change. An id already in
  use is refused either way.

## Value

The updated `sap` object.
