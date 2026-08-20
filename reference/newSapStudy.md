# Create the study metadata component of a SAP

Create the study metadata component of a SAP

## Usage

``` r
newSapStudy(
  studyId,
  title,
  authors = character(),
  version = "v1.0.0",
  description = NULL
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

## Value

An object of class `sap_study`.
