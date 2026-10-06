# Mint the next id for a SAP collection

Ids are `<prefix>_<n>` (`ds_1`, `coh_3`, `an_2`). The next one is one
past the highest number in use anywhere in the SAP – ids are unique
across collections – and in `taken`, never the lowest free one, so a
deleted item's id is never reissued to a new item while the caller
remembers it.

## Usage

``` r
newSapId(sap, collection, taken = character())
```

## Arguments

- sap:

  A `sap` object.

- collection:

  One of the SAP collection names.

- taken:

  Further ids to treat as in use, e.g. ids removed earlier in a session.

## Value

A single string.
