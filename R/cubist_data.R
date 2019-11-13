make_cubist <- function() {

  parsnip::set_new_model("cubist")

  parsnip::set_model_mode("cubist", "regression")

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("cubist", "regression", "Cubist")
  parsnip::set_dependency("cubist", "Cubist", "Cubist")

  parsnip::set_fit(
    model = "cubist",
    eng = "Cubist",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "Cubist", fun = "cubist"),
      defaults = list()
    )
  )

  parsnip::set_model_arg(
    model = "cubist",
    eng = "Cubist",
    parsnip = "committees",
    original = "committees",
    func = list(pkg = "rules", fun = "committees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "cubist",
    eng = "Cubist",
    parsnip = "neighbors",
    original = "neighbors",
    func = list(pkg = "dials", fun = "neighbors"),
    has_submodel = TRUE
  )

  parsnip::set_pred(
    model = "cubist",
    eng = "Cubist",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "response"
        )
    )
  )

  parsnip::set_pred(
    model = "cubist",
    eng = "Cubist",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = expr(object$fit), newdata = expr(new_data))
    )
  )

}
