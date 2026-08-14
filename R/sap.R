
#' Create an OMOP Standardised Analysis Plan
#'
#' @param x An R list to create a `sap` object.
#'
#' @returns A sap object.
#' @export
#'
newSap <- function(x) {
  structure(x, class = "omop_sap")
}
