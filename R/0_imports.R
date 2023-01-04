#' @keywords internal
"_PACKAGE"

#------------------------------------------------------------------------------#

#' @importFrom rlang call2 abort eval_tidy warn enquos expr
#' @importFrom purrr map_dfr
#' @importFrom tibble is_tibble as_tibble tibble
#' @importFrom stats predict model.frame model.response setNames
#' @importFrom dials new_quant_param
#' @importFrom utils head
#' @importFrom dplyr %>%
#'
#' @importFrom parsnip multi_predict
#' @export
parsnip::multi_predict

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics tunable
#' @export
generics::tunable

#' @importFrom dials mtry_prop
#' @export
dials::mtry_prop

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    ".pred_1", ".pred_2", ".pred_class", ".row_number", "object", "new_data", "name",
    ".pred", "(Intercept)", "committee", "rule", "trials", "terms",
    "column", "conds", "cover", "coverage", "error", "esterr", "estimate", "feature", "hival",
    "less_than", "level", "loval", "num_conditions", "rule_comp", "rule_id", "rule_num",
    "split_id", "split_value", "statistic", "term", "value", "lift", "ok", "trial"
  )
)
