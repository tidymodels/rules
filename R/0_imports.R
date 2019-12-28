#' @importFrom rlang enquo call2 abort eval_tidy warn new_quosure empty_env
#' @importFrom rlang enquos expr
#' @importFrom purrr map_dfr
#' @importFrom tibble is_tibble as_tibble tibble
#' @importFrom parsnip set_new_model multi_predict
#' @importFrom stats predict model.frame model.response setNames
#' @importFrom dials new_quant_param
#' @importFrom tidyr nest
#' @importFrom utils head globalVariables
#' @importFrom dplyr %>% bind_rows
#'
#' @export
parsnip::multi_predict

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    ".pred_1", ".pred_2", ".pred_class", ".rows", "object", "new_data", "name",
    ".pred"
  )
)
