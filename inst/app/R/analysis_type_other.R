# Analysis template: other -----------------------------------------------------
#
# The schema's `other` type carries no parameters: it names an analysis this
# plan does not structure. The card has the common half and nothing else.

register_analysis_template(
  "other",
  hint = paste("An analysis with no structured parameters in the schema. The plan records",
               "that it exists, its name and its data sources; no code is generated for it."),
  ui = function(ns, pf) NULL,
  collect = function(input) list(),
  flatten = function(p) list(),
  package = NULL
)
