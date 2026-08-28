# Internal helpers for the SAP core -------------------------------------------

# NOT base R's `%||%` (added in 4.4.0), which only tests is.null(). A SAP read
# back from JSON carries absent values as NA and empty collections as length-0,
# and every call site here means "missing" in that wider sense -- so shadowing
# base is deliberate. Dropping it silently changes constructSap()'s defaulting.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}
