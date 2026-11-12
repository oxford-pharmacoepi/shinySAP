# Display names of a SAP's data sources

Display names of a SAP's data sources

## Usage

``` r
sapDataSourceNames(sap, ids = NULL)
```

## Arguments

- sap:

  A SAP object.

- ids:

  Data source ids to look up; `NULL` (the default) means every data
  source in the SAP, in order.

## Value

A character vector of names. An id with no name, or one the SAP does not
know, is returned as the id itself.
