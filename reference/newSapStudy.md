# Create the study metadata component of a SAP

Create the study metadata component of a SAP

## Usage

``` r
newSapStudy(
  studyId = NULL,
  title = NULL,
  authors = character(),
  version = "v1.0.0",
  description = NULL,
  validate = TRUE
)
```

## Arguments

- studyId:

  Immutable study identifier.

- title:

  Study title.

- authors:

  Character vector of study authors.

- version:

  Study version label.

- description:

  Optional study description.

- validate:

  Whether to require a complete study. With `FALSE` a draft is built
  from whatever is given and
  [`checkSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/checkSap.md)
  reports what is missing.

## Value

An object of class `sap_study`.
