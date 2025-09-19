#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dials new_quant_param
#' @importFrom dplyr as_tibble
#' @importFrom dplyr tibble
#' @importFrom purrr map_dfr
#' @importFrom rlang call2
#' @importFrom rlang enquos
#' @importFrom rlang eval_tidy
#' @importFrom rlang expr
#' @importFrom rlang warn
#' @importFrom stats model.frame
#' @importFrom stats model.response
#' @importFrom stats predict
#' @importFrom stats setNames
#' @importFrom utils head
## usethis namespace: end
NULL

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
    ".pred",
    ".pred_1",
    ".pred_2",
    ".pred_class",
    ".row_number",
    "(Intercept)",
    "column",
    "committee",
    "conds",
    "cover",
    "coverage",
    "error",
    "esterr",
    "estimate",
    "feature",
    "hival",
    "less_than",
    "level",
    "lift",
    "loval",
    "name",
    "new_data",
    "num_conditions",
    "object",
    "ok",
    "rule",
    "rule_comp",
    "rule_id",
    "rule_num",
    "split_id",
    "split_value",
    "statistic",
    "term",
    "terms",
    "trial",
    "trials",
    "value"
  )
)
