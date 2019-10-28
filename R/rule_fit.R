#' General Interface for Prediction Rule Ensemble
#'
#' `rule_fit()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{mtry}: The number of predictors that will be
#'   randomly sampled at each split when creating the tree models.
#'   \item \code{trees}: The number of trees contained in the ensemble.
#'   \item \code{min_n}: The minimum number of data points in a node
#'   that are required for the node to be split further.
#'   \item \code{tree_depth}: The maximum depth of the tree (i.e. number of
#'  splits).
#'   \item \code{learn_rate}: The rate at which the boosting algorithm adapts
#'   from iteration-to-iteration.
#'   \item \code{loss_reduction}: The reduction in the loss function required
#'   to split further.
#'   \item \code{sample_size}: The amount of data exposed to the fitting routine.
#'   \item \code{penalty}: The amount of L1 regularization.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using the  `set_engine()` function. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions.  If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param mtry An number for the number (or proportion) of predictors that will
#'  be randomly sampled at each split when creating the tree models (`xgboost`
#'  only).
#' @param trees An integer for the number of trees contained in
#'  the ensemble.
#' @param min_n An integer for the minimum number of data points
#'  in a node that are required for the node to be split further.
#' @param tree_depth An integer for the maximum deopth of the tree (i.e. number
#'  of splits) (`xgboost` only).
#' @param learn_rate A number for the rate at which the boosting algorithm adapts
#'   from iteration-to-iteration (`xgboost` only).
#' @param loss_reduction A number for the reduction in the loss function required
#'   to split further  (`xgboost` only).
#' @param sample_size An number for the number (or proportion) of data that is
#'  exposed to the fitting routine. For `xgboost`, the sampling is done at at
#'  each iteration while `C5.0` samples once during traning.
#' @param penalty The amount of L1 regularization used in the rule selection model.
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `rule_fit()`, the
#'  possible modes are "regression" and "classification".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"xrf"` (the default), `"pre"`
#' }
#'
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call.  For this type of model, the template of the
#'  fit calls are:
#'
#' \pkg{xrf} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::rule_fit(mode = "classification"), "xrf")}
#'
#' \pkg{xrf} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::rule_fit(mode = "regression"), "xrf")}
#'
#' \pkg{C5.0} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::rule_fit(mode = "classification"), "C5.0")}
#'
#' \pkg{spark} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::rule_fit(mode = "classification"), "spark")}
#'
#' \pkg{spark} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::rule_fit(mode = "regression"), "spark")}
#'
#' @note For models created using the spark engine, there are
#'  several differences to consider. First, only the formula
#'  interface to via `fit()` is available; using `fit_xy()` will
#'  generate an error. Second, the predictions will always be in a
#'  spark table format. The names will be the same as documented but
#'  without the dots. Third, there is no equivalent to factor
#'  columns in spark tables so class predictions are returned as
#'  character columns. Fourth, to retain the model object for a new
#'  R session (via `save()`), the `model$fit` element of the `parsnip`
#'  object should be serialized via `ml_save(object$fit)` and
#'  separately saved to disk. In a new session, the object can be
#'  reloaded and reattached to the `parsnip` object.
#'
#' @importFrom purrr map_lgl
#' @seealso [varying()], [fit()], [set_engine()]
#' @examples
#' rule_fit(mode = "classification", trees = 20)
#' # Parameters can be represented by a placeholder:
#' rule_fit(mode = "regression", mtry = varying())
#' @export

rule_fit <-
  function(mode = "unknown",
           mtry = NULL, trees = NULL, min_n = NULL,
           tree_depth = NULL, learn_rate = NULL,
           loss_reduction = NULL,
           sample_size = NULL) {
    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n),
      tree_depth = enquo(tree_depth),
      learn_rate = enquo(learn_rate),
      loss_reduction = enquo(loss_reduction),
      sample_size = enquo(sample_size)
    )

    new_model_spec(
      "rule_fit",
      args,
      eng_args = NULL,
      mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.rule_fit <- function(x, ...) {
  cat("Boosted Tree Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @inheritParams rule_fit
#' @param object A boosted tree model specification.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- rule_fit(mtry = 10, min_n = 3)
#' model
#' update(model, mtry = 1)
#' update(model, mtry = 1, fresh = TRUE)
#' @method update rule_fit
#' @rdname rule_fit
#' @export
update.rule_fit <-
  function(object,
           mtry = NULL, trees = NULL, min_n = NULL,
           tree_depth = NULL, learn_rate = NULL,
           loss_reduction = NULL, sample_size = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)

    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n),
      tree_depth = enquo(tree_depth),
      learn_rate = enquo(learn_rate),
      loss_reduction = enquo(loss_reduction),
      sample_size = enquo(sample_size)
    )

    # TODO make these blocks into a function and document well
    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    new_model_spec(
      "rule_fit",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.rule_fit <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'xrf'` for translation.")
    engine <- "xrf"
  }
  x <- translate.default(x, engine, ...)

  if (engine == "spark") {
    if (x$mode == "unknown") {
      stop(
        "For spark boosted trees models, the mode cannot be 'unknown' ",
        "if the specification is to be translated.",
        call. = FALSE
      )
    } else {
      x$method$fit$args$type <- x$mode
    }
  }
  x
}

# ------------------------------------------------------------------------------

check_args.rule_fit <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$trees) && args$trees < 0)
    stop("`trees` should be >= 1", call. = FALSE)
  if (is.numeric(args$sample_size) && (args$sample_size < 0 | args$sample_size > 1))
    stop("`sample_size` should be within [0,1]", call. = FALSE)
  if (is.numeric(args$tree_depth) && args$tree_depth < 0)
    stop("`tree_depth` should be >= 1", call. = FALSE)
  if (is.numeric(args$min_n) && args$min_n < 0)
    stop("`min_n` should be >= 1", call. = FALSE)

  invisible(object)
}


# multi_predict._pre


# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

# expose all the parameters in a common interface, detect family from data

xrf_formula <- function(formula, data,
                        max_depth = 3,
                        nrounds = 100,
                        eta = NULL,
                        colsample_bytree = NULL,
                        min_child_weight = NULL,
                        gamma = NULL,
                        subsample = NULL,
                        ...) {
  # lambda is passed to predict so that the full path
  # is availible here
  xgb_args <-
    list(
      max_depth = max_depth,
      nrounds = nrounds,
      eta = eta,
      colsample_bytree = colsample_bytree,
      min_child_weight = min_child_weight,
      gamma = gamma,
      subsample = subsample
    )
  xgb_args <- xgb_args[purrr:::map_lgl(xgb_args, ~ !is.null(.x))]
  dots <- enquos(...)
  cl <-
    rlang::call2(
      .fn = "xrf",
      .ns = "xrf",
      object = rlang::expr(formula),
      data = rlang::expr(data),
      xgb_control = xgb_args,
      !!!dots
    )
  if (!any(names(dots) == "family")) {
    cl$family <- get_fam(formula, data, xrf = TRUE)
  }
  rlang::eval_tidy(cl)
}

predict_xrf <- function(object, new_data, ...) {

}


pre_formula <- function(formula, data,
                        maxdepth = NULL,
                        ntrees = NULL,
                        learnrate = NULL,
                        mtry = NULL,
                        sampfrac = NULL,
                        ...) {

  pre_args <-
    list(
      maxdepth = maxdepth,
      ntrees = ntrees,
      learnrate = learnrate,
      mtry = mtry,
      sampfrac = sampfrac
    )
  pre_args <- pre_args[purrr:::map_lgl(pre_args, ~ !is.null(.x))]
  dots <- enquos(...)
  cl <-
    rlang::call2(
      .fn = "pre",
      .ns = "pre",
      formula = rlang::expr(formula),
      data = rlang::expr(data),
      !!!pre_args,
      !!!dots
    )
  if (!any(names(dots) == "family")) {
    cl$family <- get_fam(formula, data, xrf = FALSE)
  }
  rlang::eval_tidy(cl)
}

get_fam <- function(f, dat, xrf = TRUE) {
  y_name <- all.vars(f[[2]])
  if (length(y_name) > 1) {
    if (xrf) {
      rlang::abort("Multivariate models not supported by `xrf`.")
    } else {
      return("mgaussian")
    }
  }
  y <- dat[[y_name]]
  if (is.factor(y)) {
    lvl <- levels(y)
    if (length(lvl) > 2) {
      res <- "multinomial"
    } else {
      res <- "binomial"
    }
  } else {
    res <- "gaussian"
  }
  res
}


