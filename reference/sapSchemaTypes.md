# Types defined for a SAP object

Types defined for a SAP object

## Usage

``` r
sapSchemaTypes(object)
```

## Arguments

- object:

  One of `"data_source_modification"`, `"codelist"`, `"cohort"` or
  `"analysis"`.

## Value

A named character vector: names are the human labels from the schema,
values are the type ids. Ready for a `selectInput()` `choices`.
