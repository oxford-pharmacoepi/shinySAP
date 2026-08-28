# The app's suite. Not part of the package's tests/ any more: the app is a Shiny
# directory, so there is no namespace to test_check() -- R/ is sourced the way
# loadSupport() sources it, and helper-app.R then runs as testthat's helper.
#
#   Rscript app/tests/run.R          (from the repo root)
library(testthat)

args <- commandArgs(trailingOnly = FALSE)
here <- dirname(sub("^--file=", "", grep("^--file=", args, value = TRUE)[1]))
appDir <- normalizePath(file.path(here, ".."))

env <- new.env(parent = globalenv())
for (f in list.files(file.path(appDir, "R"), pattern = "[.][Rr]$", full.names = TRUE)) {
  sys.source(f, envir = env)
}

setwd(appDir)
test_dir(file.path(appDir, "tests"), env = env, stop_on_failure = TRUE)
