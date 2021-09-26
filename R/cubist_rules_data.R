

make_cubist <- function() {
  parsnip::set_model_engine("cubist_rules", "regression", "Cubist")
  parsnip::set_dependency("cubist_rules", "Cubist", "Cubist")
  parsnip::set_dependency("cubist_rules", "Cubist", "rules")

  parsnip::set_fit(
    model = "cubist_rules",
    eng = "Cubist",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "rules", fun = "cubist_fit"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "cubist_rules",
    eng = "Cubist",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_model_arg(
    model = "cubist_rules",
    eng = "Cubist",
    parsnip = "committees",
    original = "committees",
    func = list(pkg = "rules", fun = "committees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "cubist_rules",
    eng = "Cubist",
    parsnip = "neighbors",
    original = "neighbors",
    func = list(pkg = "dials", fun = "neighbors"),
    has_submodel = TRUE
  )

  parsnip::set_model_arg(
    model = "cubist_rules",
    eng = "Cubist",
    parsnip = "max_rules",
    original = "max_rules",
    func = list(pkg = "rules", fun = "max_rules"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "cubist_rules",
    eng = "Cubist",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          neighbors = rlang::expr(rules::get_neighbors(object$spec$args))
        )
    )
  )

  parsnip::set_pred(
    model = "cubist_rules",
    eng = "Cubist",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          neighbors = rlang::expr(rules::get_neighbors(object$spec$args))
        )
    )
  )

}
