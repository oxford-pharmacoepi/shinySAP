# Sourced first: shiny::runApp() loads R/ in C-locale order, where digits sort
# before every letter, and app.R only after all of R/. Attaching here means any
# file in R/ may call a package function at source time.
library(shinySAP)
