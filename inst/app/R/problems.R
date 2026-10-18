# Problems shown on the Review tab ---------------------------------------------
#
# Validation logic lives in the package (shinySAP::sapProblems). This file
# only provides the app-local name aliases that other app files use, and
# delegates to the package so app.R can keep calling format_check_problems()
# and semantic_problems() by their original names.

sap_item_by_id        <- shinySAP::sapItemById
item_label            <- shinySAP::sapItemLabel
is_denominator_type   <- shinySAP::sapIsDenominatorType
table_name_collisions <- shinySAP::sapTableNameCollisions

format_check_problems <- shinySAP:::format_check_problems
semantic_problems     <- shinySAP:::semantic_problems
