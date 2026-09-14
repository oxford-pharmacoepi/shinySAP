# The Shiny app ------------------------------------------------------------------
#
# inst/app is a plain Shiny app DIRECTORY (app.R + R/), not package code: it is
# installed verbatim and run with shiny::runApp(), which sources its R/ folder
# and sets the working directory to it. Everything the app needs from this
# package it calls as shinySAP::<fn>(); shiny and friends are Suggests, so the
# core package stays free of a UI dependency.

#' Launch the SAP authoring app
#'
#' Opens the Shiny application for writing a Statistical Analysis Plan against
#' the package schema. The app saves each SAP as one JSON file (see
#' [writeSap()]) in `outputDir`.
#'
#' @param outputDir Folder the app saves SAPs into. Defaults to the current
#'   working directory. Also settable beforehand with
#'   `options(shinySAP.output_dir = )`.
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Further arguments to [shiny::runApp()], e.g. `port`.
#' @return Whatever [shiny::runApp()] returns, invisibly.
#' @export
shinySap <- function(outputDir = getOption("shinySAP.output_dir", getwd()),
                     launch.browser = interactive(), # nolint: object_name_linter. (shiny::runApp's own name)
                     ...) {
  for (pkg in c("shiny", "bslib", "htmltools")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_abort(c(
        x = paste0("Package {.pkg ", pkg, "} is needed to run the app."),
        i = paste0("Install it with {.code install.packages(\"", pkg, "\")}.")
      ))
    }
  }
  appDir <- system.file("app", package = "shinySAP")
  if (!nzchar(appDir)) {
    cli::cli_abort(c(x = "The app directory is not installed with this copy of shinySAP."))
  }
  omopgenerics::assertCharacter(outputDir, length = 1, na = FALSE, nm = "outputDir")
  options(shinySAP.output_dir = normalizePath(outputDir, mustWork = FALSE))
  ensurePandoc()
  invisible(shiny::runApp(appDir, launch.browser = launch.browser, ...))
}

# The document preview needs a pandoc. A CI runner and RStudio have one; a bare
# terminal on a Mac usually does not, but RStudio's copy is at a known place.
# Nothing here is fatal: without pandoc the app runs and only the preview is off.
ensurePandoc <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) return(invisible(FALSE))
  if (rmarkdown::pandoc_available()) return(invisible(TRUE))
  candidates <- c(
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64",
    "/usr/lib/rstudio/bin/quarto/bin/tools"
  )
  for (dir in candidates) {
    if (dir.exists(dir)) {
      Sys.setenv(RSTUDIO_PANDOC = dir)
      if (rmarkdown::pandoc_available()) return(invisible(TRUE))
    }
  }
  cli::cli_inform(c(
    i = "No pandoc found: the app runs, but the document preview is disabled.",
    i = "Install pandoc or run the app from RStudio."
  ))
  invisible(FALSE)
}
