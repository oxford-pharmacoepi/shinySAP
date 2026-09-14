# Schema fields for a SAP object

Schema fields for a SAP object

## Usage

``` r
sapSchemaFields(object, typeId = NULL)
```

## Arguments

- object:

  The SAP object name (`"sap"`, `"study"`, `"data_source"`,
  `"data_source_modification"`, `"codelist"`, `"cohort"`, `"analysis"`).

- typeId:

  Optional type id; when given, the type-specific rows are included
  alongside the common ones.

## Value

A data frame with one row per field: `path`, `node_type`, `value_type`,
`json_type`, `required`, `default_json`, `order`, `ref`.
