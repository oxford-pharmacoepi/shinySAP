# Export a SAP to a JSON file

Export a SAP to a JSON file

## Usage

``` r
exportSap(
  sap,
  path = getwd(),
  fileName = "sap_{study_id}_{version}.json",
  stamp = TRUE
)
```

## Arguments

- sap:

  A `sap` object or a named list in SAP shape.

- path:

  Existing directory to write into.

- fileName:

  Name of the file. `{study_id}` and `{version}` are replaced by the
  study's values (`unknown` when not yet set). A directory in `fileName`
  overrides `path`; a missing extension becomes `.json` and any other
  extension is replaced with a warning.

- stamp:

  Whether to set `generated_at` to now in the output. `FALSE` keeps the
  value the object carries.

## Value

The path of the written file, invisibly.
