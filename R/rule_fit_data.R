make_rule_fit <- function() {
  parsnip::set_model_engine("rule_fit", "classification", "xrf")
  parsnip::set_model_engine("rule_fit", "regression", "xrf")
  parsnip::set_dependency("rule_fit", "xrf", "xrf", "classification")
  parsnip::set_dependency("rule_fit", "xrf", "xrf", "regression")
  parsnip::set_dependency("rule_fit", "xrf", "rules", "classification")
  parsnip::set_dependency("rule_fit", "xrf", "rules", "regression")

  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth", range = c(1L, 10L)),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "trees",
    original = "nrounds",
    func = list(pkg = "dials", fun = "trees", range = c(5L, 100L)),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "learn_rate",
    original = "eta",
    func = list(pkg = "dials", fun = "learn_rate", range = c(-10, 0)),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "mtry",
    original = "colsample_bynode",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "min_n",
    original = "min_child_weight",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "loss_reduction",
    original = "gamma",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "sample_size",
    original = "subsample",
    func = list(pkg = "dials", fun = "sample_prop", range = c(0.50, 0.95)),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = TRUE
  )

  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "stop_iter",
    original = "early_stop",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "rule_fit",
    eng = "xrf",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "xgb_control"),
      func = c(pkg = "rules", fun = "xrf_fit"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "rule_fit",
    eng = "xrf",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "rule_fit",
    eng = "xrf",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = organize_xrf_pred,
      func = c(fun = "xrf_pred"),
      args = list(
        object = quote(object),
        new_data = quote(new_data),
        lambda = quote(object$fit$lambda),
        type = "response"
      )
    )
  )

  parsnip::set_fit(
    model = "rule_fit",
    eng = "xrf",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "xgb_control"),
      func = c(pkg = "rules", fun = "xrf_fit"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "rule_fit",
    eng = "xrf",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "rule_fit",
    eng = "xrf",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = NULL, fun = "xrf_pred"),
      args = list(
        object = quote(object),
        new_data = quote(new_data),
        lambda = quote(object$fit$lambda),
        type = "response" # post-processed into classes
      )
    )
  )

  parsnip::set_pred(
    model = "rule_fit",
    eng = "xrf",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = NULL, fun = "xrf_pred"),
      args = list(
        object = quote(object),
        new_data = quote(new_data),
        lambda = quote(object$fit$lambda),
        type = "prob"
      )
    )
  )

  # ---------------------------------------------------------------------------
  # qrf engine

  parsnip::set_model_engine("rule_fit", "classification", "qrf")
  parsnip::set_model_engine("rule_fit", "regression", "qrf")
  parsnip::set_dependency("rule_fit", "qrf", "qrf", "classification")
  parsnip::set_dependency("rule_fit", "qrf", "qrf", "regression")
  parsnip::set_dependency("rule_fit", "qrf", "rules", "classification")
  parsnip::set_dependency("rule_fit", "qrf", "rules", "regression")

  # Regression args
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "qrf",
    parsnip = "trees",
    original = "iterations",
    func = list(pkg = "dials", fun = "trees", range = c(1, 100)),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "qrf",
    parsnip = "penalty",
    original = "penalty",
    func = list(pkg = "dials", fun = "penalty", range = c(-5, -1 / 2)),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "qrf",
    parsnip = "mixture",
    original = "mixture",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )
  # regression engine arguments
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "qrf",
    parsnip = "rules",
    original = "rules",
    func = list(pkg = "dials", fun = "max_rules"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "qrf",
    parsnip = "unbiased",
    original = "unbiased",
    func = list(pkg = "dials", fun = "unbiased_rules"),
    has_submodel = FALSE
  )

  # Classification args
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "qrf",
    parsnip = "min_n",
    original = "minCases",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "qrf",
    parsnip = "sample_size",
    original = "sample",
    func = list(pkg = "dials", fun = "sample_prop"),
    has_submodel = FALSE
  )
  # Classification engine args

  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "qrf",
    parsnip = "bands",
    original = "bands",
    func = list(pkg = "dials", fun = "rule_bands"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "qrf",
    parsnip = "winnow",
    original = "winnow",
    func = list(pkg = "dials", fun = "predictor_winnowing"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "qrf",
    parsnip = "noGlobalPruning",
    original = "noGlobalPruning",
    func = list(pkg = "dials", fun = "no_global_pruning"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "qrf",
    parsnip = "CF",
    original = "CF",
    func = list(pkg = "dials", fun = "confidence_factor"),
    has_submodel = FALSE
  )

  # Regression fit
  parsnip::set_fit(
    model = "rule_fit",
    eng = "qrf",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "qrf", fun = "qrf"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "rule_fit",
    eng = "qrf",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "rule_fit",
    eng = "qrf",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = organize_qrf_numeric,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "numeric"
      )
    )
  )

  # Classification fit
  parsnip::set_fit(
    model = "rule_fit",
    eng = "qrf",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "qrf", fun = "qrf"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "rule_fit",
    eng = "qrf",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "rule_fit",
    eng = "qrf",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = organize_qrf_class,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "class"
      )
    )
  )

  parsnip::set_pred(
    model = "rule_fit",
    eng = "qrf",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "prob"
      )
    )
  )
}
