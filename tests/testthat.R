# Run from the repo root:  Rscript tests/testthat.R
library(testthat)
test_check("shinySAP", stop_on_failure = TRUE)
