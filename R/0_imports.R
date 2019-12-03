#' @importFrom rlang enquo call2 abort eval_tidy warn new_quosure empty_env
#' @importFrom rlang enquos expr
#' @importFrom purrr map_dfr
#' @importFrom tibble is_tibble as_tibble tibble
#' @importFrom parsnip set_new_model
#' @importFrom withr with_options
#' @importFrom stats predict
#' @importFrom dials new_quant_param
#' @importFrom tidyr nest

# ------------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # These define the model functions in the parsnip model database
  make_cubist()
  make_c5()
}
