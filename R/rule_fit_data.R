make_rule_fit <- function() {

  parsnip::set_new_model("rule_fit")

  parsnip::set_model_mode("rule_fit", "classification")
  parsnip::set_model_mode("rule_fit", "regression")

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("rule_fit", "classification", "xrf")
  parsnip::set_model_engine("rule_fit", "regression", "xrf")
  parsnip::set_dependency("rule_fit", "xrf", "xrf")
  parsnip::set_dependency("rule_fit", "xrf", "rules")

  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "trees",
    original = "nrounds",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "learn_rate",
    original = "eta",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rule_fit",
    eng = "xrf",
    parsnip = "mtry",
    original = "colsample_bytree",
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
    func = list(pkg = "dials", fun = "sample_prop"),
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

  parsnip::set_fit(
    model = "rule_fit",
    eng = "xrf",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("object", "data"),
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
      protect = c("object", "data"),
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
        type = "response"  # post-processed into classes
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

}
