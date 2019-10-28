set_new_model("rule_fit")

set_model_mode("rule_fit", "classification")
set_model_mode("rule_fit", "regression")

# ------------------------------------------------------------------------------

set_model_engine("rule_fit", "classification", "xrf")
set_model_engine("rule_fit", "regression", "xrf")
set_dependency("rule_fit", "xrf", "xrf")

set_model_arg(
  model = "rule_fit",
  eng = "xrf",
  parsnip = "tree_depth",
  original = "max_depth",
  func = list(pkg = "dials", fun = "tree_depth"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rule_fit",
  eng = "xrf",
  parsnip = "trees",
  original = "nrounds",
  func = list(pkg = "dials", fun = "trees"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rule_fit",
  eng = "xrf",
  parsnip = "learn_rate",
  original = "eta",
  func = list(pkg = "dials", fun = "learn_rate"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rule_fit",
  eng = "xrf",
  parsnip = "mtry",
  original = "colsample_bytree",
  func = list(pkg = "dials", fun = "mtry"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rule_fit",
  eng = "xrf",
  parsnip = "min_n",
  original = "min_child_weight",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rule_fit",
  eng = "xrf",
  parsnip = "loss_reduction",
  original = "gamma",
  func = list(pkg = "dials", fun = "loss_reduction"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rule_fit",
  eng = "xrf",
  parsnip = "sample_size",
  original = "subsample",
  func = list(pkg = "dials", fun = "sample_size"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rule_fit",
  eng = "xrf",
  parsnip = "penalty",
  original = "lambda",
  func = list(pkg = "dials", fun = "penalty"),
  has_submodel = TRUE
)

set_fit(
  model = "rule_fit",
  eng = "xrf",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("object", "data"),
    func = c(pkg = "rules", fun = "xrf_formula"),
    defaults = list()
  )
)

set_pred(
  model = "rule_fit",
  eng = "xrf",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "xgb_pred"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

set_pred(
  model = "rule_fit",
  eng = "xrf",
  mode = "regression",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "xgb_pred"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

set_fit(
  model = "rule_fit",
  eng = "xrf",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(pkg = "parsnip", fun = "xgb_train"),
    defaults = list(nthread = 1, verbose = 0)
  )
)

set_pred(
  model = "rule_fit",
  eng = "xrf",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = function(x, object) {
      if (is.vector(x)) {
        x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
      } else {
        x <- object$lvl[apply(x, 1, which.max)]
      }
      x
    },
    func = c(pkg = NULL, fun = "xgb_pred"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

set_pred(
  model = "rule_fit",
  eng = "xrf",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      if (is.vector(x)) {
        x <- tibble(v1 = 1 - x, v2 = x)
      } else {
        x <- as_tibble(x)
      }
      colnames(x) <- object$lvl
      x
    },
    func = c(pkg = NULL, fun = "xgb_pred"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

set_pred(
  model = "rule_fit",
  eng = "xrf",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "xgb_pred"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

# ------------------------------------------------------------------------------

set_model_engine("rule_fit", "classification", "pre")
set_dependency("rule_fit", "pre", "C50")

set_model_arg(
  model = "rule_fit",
  eng = "pre",
  parsnip = "trees",
  original = "trials",
  func = list(pkg = "dials", fun = "trees"),
  has_submodel = TRUE
)
set_model_arg(
  model = "rule_fit",
  eng = "pre",
  parsnip = "min_n",
  original = "minCases",
  func = list(pkg = "dials", fun = "min_n"),
  has_submodel = FALSE
)
set_model_arg(
  model = "rule_fit",
  eng = "pre",
  parsnip = "sample_size",
  original = "sample",
  func = list(pkg = "dials", fun = "sample_size"),
  has_submodel = FALSE
)

set_fit(
  model = "rule_fit",
  eng = "pre",
  mode = "classification",
  value = list(
    interface = "data.frame",
    protect = c("x", "y", "weights"),
    func = c(pkg = "parsnip", fun = "pre_train"),
    defaults = list()
  )
)

set_pred(
  model = "rule_fit",
  eng = "pre",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = quote(object$fit), newdata = quote(new_data))
  )
)

set_pred(
  model = "rule_fit",
  eng = "pre",
  mode = "classification",
  type = "prob",
  value = list(
    pre = NULL,
    post = function(x, object) {
      as_tibble(x)
    },
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "prob"
      )
  )
)

set_pred(
  model = "rule_fit",
  eng = "pre",
  mode = "classification",
  type = "raw",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args = list(object = quote(object$fit),
                newdata = quote(new_data))
  )
)
