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
#' @param neighbors An integer between zero and nine for the number of training
#' set instances that are used to adjust the model-based prediction.
#' @details
#' The only availible engine is `"Cubist"`.
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. For this type of
#'  model, the template of the fit calls are:
#'
#' \pkg{Cubist}
#'
#'
#' @seealso [parsnip::fit()], [parsnip::set_engine()]
#' @examples
#' cubist_rules()
#' # Parameters can be represented by a placeholder:
#' cubist_rules(committees = 7)
#' @export
#' @importFrom purrr map_lgl
cubist_rules <-
  function(mode = "regression",
           committees = NULL,
           neighbors = NULL) {

    args <- list(
      committees = enquo(committees),
      neighbors = enquo(neighbors)
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
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}


# ------------------------------------------------------------------------------

#' @param object A Cubist model specification.
#' @examples
#' model <- cubist_rules(committees = 10, neighbors = 2)
#' model
#' update(model, committees = 1)
#' update(model, committees = 1, fresh = TRUE)
#' @method update cubist_rules
#' @rdname cubist_rules
#' @export
update.cubist_rules <-
  function(object,
           parameters = NULL,
           committees = NULL, neighbors = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }
    args <- list(
      committees = enquo(committees),
      neighbors = enquo(neighbors)
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

#' @export
#' @keywords internal
cubist_fit <- function(x, y, committees = 1, neighbors = 0, ...) {

  args <- list(
    x = expr(x),
    y = expr(y),
    committees = expr(committees)
    # neighbors not needed until predict time but are saved below
  )
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    args <- c(args, dots)
  } else {
    args <-
      c(args, list(control = expr(Cubist::cubistControl(seed = sample.int(10 ^ 5, 1)))))
  }
 cl <- rlang::call2(.fn = "cubist", .ns = "Cubist", !!!args)
 res <- rlang::eval_tidy(cl)
 res$.neighbors <- rlang::eval_tidy(neighbors)
 res
}

cubist_pred_wrap <- function(neighbors = 0, object, new_data, ...) {
  if (length(neighbors) > 1) {
    rlang::abort("`cubist_pred_wrap` takes a single neighbor.")
  }
  neighbors[neighbors < 0] <- 0L
  neighbors[neighbors > 9] <- 9L

  args <- list(
    object = expr(object$fit),
    newdata = expr(new_data),
    neighbors = neighbors
  )
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    args <- c(args, dots)
  }
  cl <- rlang::call2(.fn = "predict", !!!args)
  res <- rlang::eval_tidy(cl)
  tibble::tibble(neighbors = neighbors, .pred = res)
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

  purrr:::map_dfr(neighbors,
                  cubist_pred_wrap,
                  object = object,
                  new_data = new_data,
                  ...)
}


#' @export
committees <- function(range = c(1L, 100L), trans = NULL)  {
  range[range < 1] <- 1L
  range[range > 100] <- 100L

  dials:::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(committees = "# Committees"),
    finalize = NULL
  )
}


#' [multi_predict()] methods for rule-based models
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
    res <- cubist_pred(object, new_data, neighbors = neighbors, ...)
    if (n > 1) {
      res$.rows <- rep(1:nrow(new_data), n)
      res <-
        res %>%
        dplyr::group_by(.rows) %>%
        tidyr::nest() %>%
        dplyr::ungroup() %>%
        dplyr::select(-.rows) %>%
        setNames(".pred")
    }
    res
  }
