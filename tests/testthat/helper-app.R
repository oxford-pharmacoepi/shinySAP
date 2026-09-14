# Sourced by testthat before every test file: the Shiny app's R/ folder, loaded
# the way shiny::runApp() loads it -- every file in C-locale alphabetical order
# into ONE environment -- so an ordering bug (a top-level call into a file that
# sorts later) fails here exactly as it would in the app.
#
# Under devtools/pkgload, system.file() is shimmed to inst/app; under
# R CMD check it is the installed copy. Either way the app code is what ships.
app_dir <- system.file("app", package = "shinySAP")

app_env <- new.env(parent = globalenv())
if (nzchar(app_dir) && requireNamespace("shiny", quietly = TRUE) &&
    requireNamespace("bslib", quietly = TRUE)) {
  for (f in sort(list.files(file.path(app_dir, "R"), pattern = "[.][Rr]$", full.names = TRUE),
                 method = "radix")) {
    sys.source(f, envir = app_env)
  }
}

# Every test file that touches the app starts with this.
skip_without_app <- function() {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if(!nzchar(app_dir), "inst/app is not available")
}

# Reach an app function without attaching the environment.
app <- function(name) get(name, envir = app_env, inherits = FALSE)

# A complete, valid SAP exercising every type, for the code generator, the
# preview and the problem checks. Mirrors tests/testthat/fixtures/sap-c1-001-v1.0.0.json.
fixture_sap <- function() readSap(testthat::test_path("fixtures", "sap-c1-001-v1.0.0.json"))

# A shiny input-like list with `$` access, for template collect() functions.
fake_input <- function(...) list(...)

# names() that is character(0), not NULL, for an empty list.
nm <- function(x) if (is.null(names(x))) character(0) else names(x)
