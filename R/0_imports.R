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

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # These define the model functions in the parsnip model database
  make_cubist()
  make_c5()
  make_rule_fit()
}

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    ".pred_1", ".pred_2", ".pred_class", ".rows", "object", "new_data", "name"
  )
)
