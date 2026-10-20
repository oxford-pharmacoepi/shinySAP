# Write an OmopStudyBuilder study directory from a SAP

Fills `codelist/codelistCreation.R`, `cohorts/instantiateCohorts.R`, and
`analyses/estimates.R` under `dir`, creating subdirectories as needed.
Also writes the codelist JSON files that the generated R code reads
back. Never removes or overwrites files the study harness owns
(`codeToRun.R`, `runStudy.R`).

## Usage

``` r
writeSapStudy(sap, dir)
```

## Arguments

- sap:

  A SAP object from
  [`readSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/readSap.md)
  or the CRUD constructors.

- dir:

  The study root directory (must exist).

## Value

The paths written, invisibly.
