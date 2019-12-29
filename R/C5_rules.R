#' General Interface for C5.0 Rule-Based Classification Models
#'
#' [C5_rules()] is a way to generate a _specification_ of a model
#'  before fitting. The main arguments for the model are:
#' \itemize{
#'   \item \code{trees}: The number of sequential models included in the
#'   ensemble (rules are derived from an initial set of boosted trees).
#'   \item \code{min_n}: The minimum number of data points in a node that are
#'   required for the node to be split further.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using [parsnip::set_engine()]. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param trees A non-negative integer (no greater than 100 for the number
#'  of members of the ensemble.
#' @param min_n An integer greater than one zero and nine for the minimum number
#'  of data points in a node that are required for the node to be split further.
#' @details
#' The only availible engine is `"C5.0"`.
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. For this type of
#'  model, the template of the fit calls are:
#'
#' \pkg{C5.0}
#'
#'
#' @seealso [parsnip::fit()], [parsnip::set_engine()]
#' @examples
#' C5_rules()
#' # Parameters can be represented by a placeholder:
#' C5_rules(trees = 7)
#' @export
#' @importFrom purrr map_lgl
C5_rules <-
  function(mode = "classification",
           trees = NULL,
           min_n = NULL) {

    args <- list(
      trees = enquo(trees),
      min_n = enquo(min_n)
    )

    new_model_spec(
      "C5_rules",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = "C5.0"
    )
  }

#' @export
print.C5_rules <- function(x, ...) {
  cat("C5.0 Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}


# ------------------------------------------------------------------------------

#' @param object A `C5_rules` model specification.
#' @param parameters	A 1-row tibble or named list with _main_ parameters to update.
#' If the individual arguments are used, these will supersede the values in
#' parameters. Also, using engine arguments in this object will result in an
#' error.
#' @param fresh	A logical for whether the arguments should be modified in-place
#' or replaced wholesale.
#' @param ...	Not used for `update()`.
#' @examples
#' model <- C5_rules(trees = 10, min_n = 2)
#' model
#' update(model, trees = 1)
#' update(model, trees = 1, fresh = TRUE)
#' @method update C5_rules
#' @rdname C5_rules
#' @export
update.C5_rules <-
  function(object,
           parameters = NULL,
           trees = NULL, min_n = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }
    args <- list(
      trees = enquo(trees),
      min_n = enquo(min_n)
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
      "C5_rules",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

# make work in different places

check_args.C5_rules <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$trees)) {
    if (length(args$trees) > 1) {
      rlang::abort("Only a single value of `trees` is used.")
    }
    msg <- "The number of trees should be >= 1 and <= 100. Truncating the value."
    if (args$trees > 100) {
      object$args$trees <-
        rlang::new_quosure(100L, env = rlang::empty_env())
      rlang::warn(msg)
    }
    if (args$trees < 1) {
      object$args$trees <-
        rlang::new_quosure(1L, env = rlang::empty_env())
      rlang::warn(msg)
    }

  }
  if (is.numeric(args$min_n)) {
    if (length(args$min_n) > 1) {
      rlang::abort("Only a single `min_n`` value is used.")
    }
  }
  invisible(object)
}

# ------------------------------------------------------------------------------

#' Internal function wrappers
#'
#' These are not supported when called by the user.
#' @export
#' @keywords internal
#' @rdname rules-internal
c5_fit <- function(x, y, trials = 1, minCases = 2, cost =  NULL, ...) {

  args <- list(
    x = expr(x),
    y = expr(y),
    trials = expr(trials)
  )
  dots <- rlang::enquos(...)
  # trim rules and costs from dots
  if (length(dots) > 0) {
    if (any(names(dots) == "control")) {
      dots$control$minCases <- minCases
      if (!any(names(dots$control) == "seed")) {
        dots$control$seed <- sample.int(10 ^ 5, 1)
      }
    }
    args <- c(args, dots)
  } else {
    args <-
      c(args,
        list(control = expr(C50::C5.0Control(minCases = minCases, seed = sample.int(10 ^ 5, 1))))
      )
  }
  args$rules <- TRUE

  if (!is.null(cost)) {
    if (length(levels(y) != 2)) {
      rlang::abort("Cost-sensitive models only implemented for 2 classes.")
    }
    costs <- matrix(c(0, 1, cost, 0), nrow = 2, byrow = TRUE)
    args$costs <- costs
  }


  cl <- rlang::call2(.fn = "C5.0", .ns = "C50", !!!args)
  res <- rlang::eval_tidy(cl)
  res
}

c5_pred_wrap <- function(trials = 1, object, new_data, type = "class", ...) {
  if (length(trials) > 1) {
    rlang::abort("`c5_pred_wrap` takes a single value of `trials`")
  }
  trials[trials <   1] <-   1L
  trials[trials > 100] <- 100L

  args <- list(
    object = expr(object$fit),
    newdata = expr(new_data),
    trials = trials,
    type = type
  )
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    args <- c(args, dots)
  }
  cl <- rlang::call2(.fn = "predict", !!!args)
  res <- rlang::eval_tidy(cl)
  if (type == "class") {
    res <- tibble::tibble(trials = trials, .pred_class = res)
  } else {
    res <- tibble::as_tibble(res)
    names(res) <- paste0(".pred_", names(res))
    res <- dplyr::bind_cols(tibble::tibble(trials = rep(trials, nrow(res))), res)
  }
  res
}

#' @export
#' @keywords internal
#' @rdname rules-internal
c5_pred <- function(object, new_data, trials = object$fit$trials["Actual"], ...) {

  res <- purrr::map_dfr(trials,
                        c5_pred_wrap,
                        object = object,
                        new_data = new_data,
                        ...)
  if (length(trials) == 1) {
    res <- res %>% dplyr::select(-trials)
  }
  res
}

#' @rdname multi_predict
#' @export
#' @param trees An numeric vector of `trees` between one and 100.
#' @param object An object of class `model_fit`
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values
#'  are class" and "prob".
#' @param ... Not currently used.
multi_predict._C5.0 <-
  function(object, new_data, type = NULL, trees = NULL, ...) {
    if (any(names(enquos(...)) == "newdata")) {
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
    }
    trees_used <- object$fit$trials["Actual"]
    trees <- ifelse(trees > trees_used, trees_used, trees)
    if (is.null(type)) {
      type <- "class"
    }

    res <- c5_pred(object, new_data, trials = trees, type = type, ...)

    res$.rows <- rep(1:nrow(new_data), length(trees))
    res <-
      res %>%
      dplyr::group_by(.rows) %>%
      tidyr::nest() %>%
      dplyr::ungroup() %>%
      dplyr::select(-.rows) %>%
      setNames(".pred")
    res
  }

# ------------------------------------------------------------------------------

prob_matrix_to_tibble <- function(x, object) {
  tibble::as_tibble(x)
}