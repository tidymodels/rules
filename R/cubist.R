#' General Interface for Cubist Rule-Based Regression Models
#'
#' [cubist_rules()] is a way to generate a _specification_ of a model
#'  before fitting. The main arguments for the model are:
#' \itemize{
#'   \item \code{committees}: The number of sequential models included in the
#'   ensemble (similar to the number of trees in boosting).
#'   \item \code{neighbors}: The number of neighbors in the post-model
#'   instance-based adjustment.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using [parsnip::set_engine()]. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param committees A non-negative integer (no greater than 100 for the number
#'  of members of the ensemble.
#' @param max_rules The largest number of rules.
#' @param neighbors An integer between zero and nine for the number of training
#' set instances that are used to adjust the model-based prediction.
#' @details
#' Cubist is a rule-based ensemble regression model. A basic model tree
#'  (Quinlan, 1992) is created that has a separate linear regression model
#'  corresponding for each terminal node. The paths along the model tree is
#'  flattened into rules these rules are simplified and pruned. The parameter
#'  `min_n` is the primary method for controlling the size of each tree while
#'  `max_rules` controls the number of rules.
#'
#' Cubist ensembles are created using _committees_, which are similar to
#'  boosting. After the first model in the committee is created, the second
#'  model uses a modified version of the outcome data based on whether the
#'  previous model under- or over-predicted the outcome. For iteration _m_, the
#'  new outcome `y*` is computed using
#'
#' \figure{comittees.png}
#'
#' If a sample is under-predicted on the previous iteration, the outcome is
#'  adjusted so that the next time it is more likely to be over-predicted to
#'  compensate. This adjustment continues for each ensemble iteration. See
#'  Kuhn and Johnson (2013) for details.
#'
#' After the model is created, there is also an option for a post-hoc
#'  adjustment that uses the training set (Quinlan, 1993). When a new sample is
#'  predicted by the model, it can be modified by its nearest neighbors in the
#'  original training set. For _K_ neighbors, the model based predicted value is
#'  adjusted by the neighbor using:
#'
#' \figure{adjust.png}
#'
#' where `t` is the training set prediction and `w` is a weight that is inverse
#'  to the distance to the neighbor.
#'
#' Note that `cubist_rules()` does not require that categorical predictors be
#'  converted to numeric indicator values. Note that using [parsnip::fit()] will
#'  _always_ create dummy variables so, if there is interest in keeping the
#'  categorical predictors in their original format, [parsnip::fit_xy()] would
#'  be a better choice. When using the `tune` package, using a recipe for
#'  pre-processing enables more control over how such predictors are encoded
#'  since recipes do not automatically create dummy variables.
#'
#' The only available engine is `"Cubist"`.
#'
#' @return An updated `parsnip` model specification.
#' @seealso [parsnip::fit()], [parsnip::fit_xy()], [Cubist::cubist()],
#' [Cubist::cubistControl()]
#' @references Quinlan R (1992). "Learning with Continuous Classes." Proceedings
#' of the 5th Australian Joint Conference On Artificial Intelligence, pp.
#' 343-348.
#'
#' Quinlan R (1993)."Combining Instance-Based and Model-Based Learning."
#' Proceedings of the Tenth International Conference on Machine Learning, pp.
#' 236-243.
#'
#' Kuhn M and Johnson K (2013). _Applied Predictive Modeling_. Springer.
#' @examples
#' cubist_rules()
#' # Parameters can be represented by a placeholder:
#' cubist_rules(committees = 7)
#'
#' # ------------------------------------------------------------------------------
#'
#' data(car_prices, package = "modeldata")
#' car_rules <-
#'   cubist_rules(committees = 1) %>%
#'   fit(log10(Price) ~ ., data = car_prices)
#'
#' car_rules
#'
#' summary(car_rules$fit)
#' @export
cubist_rules <-
  function(mode = "regression",
           committees = NULL,
           neighbors = NULL,
           max_rules = NULL) {

    args <- list(
      committees = enquo(committees),
      neighbors = enquo(neighbors),
      max_rules = enquo(max_rules)
    )

    new_model_spec(
      "cubist_rules",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = "Cubist"
    )
  }

#' @export
print.cubist_rules <- function(x, ...) {
  cat("Cubist Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}


# ------------------------------------------------------------------------------

#' @param object A Cubist model specification.
#' @examples
#'
#' # ------------------------------------------------------------------------------
#'
#' model <- cubist_rules(committees = 10, neighbors = 2)
#' model
#' update(model, committees = 1)
#' update(model, committees = 1, fresh = TRUE)
#' @method update cubist_rules
#' @rdname cubist_rules
#' @inheritParams update.C5_rules
#' @export
update.cubist_rules <-
  function(object,
           parameters = NULL,
           committees = NULL, neighbors = NULL, max_rules = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }
    args <- list(
      committees = enquo(committees),
      neighbors = enquo(neighbors),
      max_rules = enquo(max_rules)
    )

    args <- update_main_parameters(args, parameters)

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
      "cubist_rules",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

# make work in different places

check_args.cubist_rules <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$committees)) {
    if (length(args$committees) > 1) {
      rlang::abort("Only a single committee member is used.")
    }
    msg <- "The number of committees should be >= 1 and <= 100. Truncating the value."
    if (args$committees > 100) {
      object$args$committees <-
        rlang::new_quosure(100L, env = rlang::empty_env())
      rlang::warn(msg)
    }
    if (args$committees < 1) {
      object$args$committees <-
        rlang::new_quosure(1L, env = rlang::empty_env())
      rlang::warn(msg)
    }

  }
  if (is.numeric(args$neighbors)) {
    if (length(args$neighbors) > 1) {
      rlang::abort("Only a single neighbors value is used.")
    }
    msg <- "The number of neighbors should be >= 0 and <= 9. Truncating the value."
    if (args$neighbors > 9) {
      object$args$neighbors <-
        rlang::new_quosure(9L, env = rlang::empty_env())
      rlang::warn(msg)
    }
    if (args$neighbors < 0) {
      object$args$neighbors <-
        rlang::new_quosure(0L, env = rlang::empty_env())
      rlang::warn(msg)
    }

  }
  invisible(object)
}

# ------------------------------------------------------------------------------

cubist_args <- function(x) {
  ctrl_args <- c('unbiased', 'rules', 'extrapolation', 'sample', 'seed', 'label')

  # translate name
  names(x) <- ifelse(names(x) == "max_rules", "rules", names(x))
  nms <- names(x)

  is_ctrl <- nms %in% ctrl_args
  is_main_arg <- !is_ctrl & nms != "control"
  main_args <- x[is_main_arg]
  nms <- nms[!is_main_arg]

  if (any(nms == "control")) {
    ctrl_arg_nms <- nms[nms != "control"]
    # Add any other options to ctrl
    for (i in ctrl_arg_nms) {
      x$control[[i]] <- x[[i]]
    }
    args <- c(main_args, x["control"])
  } else {
    if (any(is_ctrl)) {
      ctrl_call <- rlang::call2("cubistControl", .ns = "Cubist", !!!x[nms])
      args <- c(main_args, list(control = ctrl_call))
    } else {
      args <- main_args
    }
  }

  rlang::call2("cubist", .ns = "Cubist", !!!args)
}

#' @export
#' @keywords internal
#' @rdname rules-internal
cubist_fit <- function(x, y, committees = 1, neighbors = 0, max_rules = NA, ...) {
  args <- list(
    x = expr(x),
    y = expr(y),
    committees = committees
    # neighbors not needed until predict time but is saved below
  )
  if (!is.na(max_rules)) {
    args$rules <- max_rules
  }
  dots <- rlang::dots_list(...)
  if (length(dots) > 0) {
    args <- c(args, dots)
  }

  cl <- cubist_args(args)

  res <- rlang::eval_tidy(cl)

 # Fix used args
 used <- res$coefficients %>% dplyr::select(-`(Intercept)`, -committee, -rule)
 used <- purrr::map_lgl(used, ~ any(!is.na(.x)))
 res$vars$used <- names(used)[used]

 res$.neighbors <- rlang::eval_tidy(neighbors)
 res
}

#' @export
#' @keywords internal
#' @rdname rules-internal
get_neighbors <- function(x) {
  k <- rlang::eval_tidy(x$neighbors)
  if (is.null(k) || k < 0) {
    k <- 0
  }
  if (k > 9) {
    k <- 9
  }
  k
}


## TODO the wrapper breaks when there are dummy variables since `fit()` makes
## dummy variables. Maybe `predict.cubist_rules()` should directly invoke
## `predict.cubist()`

cubist_pred_wrap <- function(neighbors = 0, object, new_data, ...) {
  if (length(neighbors) > 1) {
    rlang::abort("`cubist_pred_wrap` takes a single neighbor.")
  }
  object$spec$args$neighbors <- neighbors
  res <- predict(object, new_data)
  tibble::tibble(neighbors = neighbors, .pred = res$.pred)
}

cubist_pred <- function(object, new_data, neighbors = NULL, ...) {
  if (is.null(neighbors)) {
    if (any(names(object) == ".neighbors")) {
      neighbors <- object$.neighbors
    } else {
      neighbors <- 0L
    }
  } else {
    neighbors <- rlang::eval_tidy(neighbors)
  }

  new_data <- as.data.frame(new_data)

  purrr::map_dfr(neighbors,
                 cubist_pred_wrap,
                 object = object,
                 new_data = new_data,
                 ...)
}

#' Parameter functions for Cubist models
#'
#' Committee-based models enact a boosting-like procedure to produce ensembles.
#' `committees` parameter is for the number of models in the ensembles while
#' `max_rules` can be used to limit the number of possible rules.
#'
#' @param range A two-element vector holding the _defaults_ for the smallest and
#' largest possible values, respectively.
#'
#' @param trans A `trans` object from the `scales` package, such as
#' `scales::log10_trans()` or `scales::reciprocal_trans()`. If not provided,
#' the default is used which matches the units used in `range`. If no
#' transformation, `NULL`.
#'
#' @return A function with classes "quant_param" and "param"
#' @examples
#' committees()
#' committees(4:5)
#'
#' max_rules()
#' @export
committees <- function(range = c(1L, 100L), trans = NULL)  {
  range[range < 1] <- 1L
  range[range > 100] <- 100L

  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(committees = "# Committees"),
    finalize = NULL
  )
}

#' @export
#' @rdname committees
max_rules <- function(range = c(1L, 500L), trans = NULL)  {
  range[range < 1] <- 1L

  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(max_rules = "Max. Rules"),
    finalize = NULL
  )
}


#' @export
#' @keywords internal
#' @rdname rules-internal
tunable.cubist_rules <- function(x, ...) {
  tibble::tibble(
    name = c('committees', 'neighbors', 'max_rules'),
    call_info = list(
      list(pkg = "rules", fun = "committees", range = c(1L, 100L)),
      list(pkg = "dials", fun = "neighbors",  range = c(0L,   9L)),
      list(pkg = "rules", fun = "max_rules")
    ),
    source = "model_spec",
    component = class(x)[class(x) != "model_spec"][1],
    component_id =  "main"
  )
}

#' `multi_predict()` methods for rule-based models
#' @rdname multi_predict
#' @export
#' @param neighbors An numeric vector of neighbors values between zero and nine.
multi_predict._cubist <-
  function(object, new_data, type = NULL, neighbors = NULL, ...) {
    if (any(names(enquos(...)) == "newdata")) {
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
    }
    if (is.null(neighbors)) {
      n <- 1
    } else {
      n <- length(neighbors)
    }

    new_data <- parsnip::prepare_data(object, new_data)
    # preprocess data
    if (!is.null(object$spec$method$pred$numeric$pre)) {
      new_data <- object$spec$method$pred$numeric$pre(new_data, object)
    }

    res <- cubist_pred(object, new_data, neighbors = neighbors, ...)
    if (n > 1) {
      res$.row_number <- rep(1:nrow(new_data), n)
      res <-
        res %>%
        dplyr::group_by(.row_number) %>%
        tidyr::nest() %>%
        dplyr::ungroup() %>%
        dplyr::select(-.row_number) %>%
        setNames(".pred")
    }
    res
  }
