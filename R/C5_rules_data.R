
make_c5 <- function() {

  parsnip::set_new_model("C5_rules")

  parsnip::set_model_mode("C5_rules", "classification")

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("C5_rules", "classification", "C5.0")
  parsnip::set_dependency("C5_rules", "C5.0", "C50")
  parsnip::set_dependency("C5_rules", "C5.0", "rules")


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

  parsnip::set_model_arg(
    model = "C5_rules",
    eng = "C5.0",
    parsnip = "trees",
    original = "trials",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "C5_rules",
    eng = "C5.0",
    parsnip = "min_n",
    original = "minCases",
    func = list(pkg = "dials", fun = "min_n"),
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
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
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
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "prob"
        )
    )
  )

}

