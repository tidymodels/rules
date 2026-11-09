make_c5 <- function() {
  parsnip::set_model_engine("C5_rules", "classification", "C5.0")
  parsnip::set_dependency("C5_rules", "C5.0", "C50", "classification")
  parsnip::set_dependency("C5_rules", "C5.0", "rules", "classification")

  parsnip::set_fit(
    model = "C5_rules",
    eng = "C5.0",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "rules", fun = "c5_fit"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "C5_rules",
    eng = "C5.0",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_model_arg(
    model = "C5_rules",
    eng = "C5.0",
    parsnip = "trees",
    original = "trials",
    func = list(pkg = "dials", fun = "trees", range = c(1L, 100L)),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "C5_rules",
    eng = "C5.0",
    parsnip = "min_n",
    original = "minCases",
    func = list(pkg = "dials", fun = "min_n", range = c(2L, 40L)),
    has_submodel = FALSE
  )

  ## engine arguments
  parsnip::set_model_arg(
    model = "C5_rules",
    eng = "C5.0",
    parsnip = "CF",
    original = "CF",
    func = list(pkg = "dials", fun = "confidence_factor"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "C5_rules",
    eng = "C5.0",
    parsnip = "noGlobalPruning",
    original = "noGlobalPruning",
    func = list(pkg = "dials", fun = "no_global_pruning"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "C5_rules",
    eng = "C5.0",
    parsnip = "winnow",
    original = "winnow",
    func = list(pkg = "dials", fun = "predictor_winnowing"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "C5_rules",
    eng = "C5.0",
    parsnip = "fuzzyThreshold",
    original = "fuzzyThreshold",
    func = list(pkg = "dials", fun = "fuzzy_thresholding"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "C5_rules",
    eng = "C5.0",
    parsnip = "bands",
    original = "bands",
    func = list(pkg = "dials", fun = "rule_bands"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "C5_rules",
    eng = "C5.0",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data),
        type = "class"
      )
    )
  )

  parsnip::set_pred(
    model = "C5_rules",
    eng = "C5.0",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = prob_matrix_to_tibble,
      func = c(fun = "predict"),
      args = list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data),
        type = "prob"
      )
    )
  )
}
