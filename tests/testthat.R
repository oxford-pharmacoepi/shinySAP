# Run from THIS directory (test_check resolves testthat/ against the working
# directory, and R CMD check runs it from here):  cd tests && Rscript testthat.R
# Or just:  scripts/precheck.sh tests
library(testthat)
test_check("shinySAP", stop_on_failure = TRUE)
