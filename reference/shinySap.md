# Launch the SAP authoring app

Opens the Shiny application for writing a Statistical Analysis Plan
against the package schema. The app saves each SAP as one JSON file (see
[`writeSap()`](https://oxford-pharmacoepi.github.io/shinySAP/reference/writeSap.md))
in `outputDir`.

## Usage

``` r
shinySap(
  outputDir = getOption("shinySAP.output_dir", getwd()),
  launch.browser = interactive(),
  ...
)
```

## Arguments

- outputDir:

  Folder the app saves SAPs into. Defaults to the current working
  directory. Also settable beforehand with
  `options(shinySAP.output_dir = )`.

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

- ...:

  Further arguments to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), e.g.
  `port`.

## Value

Whatever [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
returns, invisibly.
