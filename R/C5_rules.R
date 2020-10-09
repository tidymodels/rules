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
#' @details C5.0 is a classification model that is an extension of the C4.5
#'  model of Quinlan (1993). It has tree- and rule-based versions that also
#'  include boosting capabilities. `C5_rules()` enables the version of the model
#'  that uses a series of rules (see the examples below). To make a set of
#'  rules, an initial C5.0 tree is created and flattened into rules. The rules
#'  are pruned, simplified, and ordered. Rule sets are created within each
#'  iteration of boosting.
#'
#' The two main tuning parameters are the number of trees in the boosting
#'  ensemble (`trees`) and the number of samples required to continue splitting
#'  when creating a tree (`min_n`). There are no arguments to control the total
#'  number of rules in the ensemble.
#'
#' Note that `C5_rules()` does not require that categorical predictors be
#'  converted to numeric indicator values. Note that using [parsnip::fit()] will
#'  _always_ create dummy variables so, if there is interest in keeping the
#'  categorical predictors in their original format, [parsnip::fit_xy()] would
#'  be a better choice. When using the `tune` package, using a recipe for
#'  pre-processing enables more control over how such predictors are encoded
#'  since recipes do not automatically create dummy variables.
#'
#' Note that C5.0 has a tool for _early stopping_ during boosting where less
#'  iterations of boosting are performed than the number requested. `C5_rules()`
#'  turns this feature off (although it can be re-enabled using
#'  [C50::C5.0Control()]).
#'
#' @return An updated `parsnip` model specification.
#' @seealso [parsnip::fit()], [parsnip::fit_xy()], [C50::C5.0()],
#' [C50::C5.0Control()]
#' @references Quinlan R (1993). _C4.5: Programs for Machine Learning_. Morgan
#' Kaufmann Publishers.
#' @examples
#' C5_rules()
#' # Parameters can be represented by a placeholder:
#' C5_rules(trees = 7)
#'
#' # ------------------------------------------------------------------------------
#'
#' data(ad_data, package = "modeldata")
#'
#' set.seed(282782)
#' class_rules <-
#'   C5_rules(trees = 1, min_n  = 10) %>%
#'   fit(Class ~ ., data = ad_data)
#'
#' summary(class_rules$fit)
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
#'
#' # ------------------------------------------------------------------------------
#'
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
      if (!any(names(dots$control) == "earlyStopping")) {
        dots$control$earlyStopping <- FALSE
      }
    }
    args <- c(args, dots)
  } else {
    args <-
      c(args,
        list(control =
               expr(C50::C5.0Control(minCases = minCases, seed = sample.int(10 ^ 5, 1), earlyStopping = FALSE)))
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
  # Give an extra class to have a separate multi_predict() method
  class(res) <- c("c5_rules", class(res))
  res
}

c5_pred_wrap <- function(trials = 1, object, new_data, type = "class", ...) {
  if (length(trials) > 1) {
    rlang::abort("`c5_pred_wrap` takes a single value of `trials`")
  }
  trials[trials <   1] <-   1L
  trials[trials > 100] <- 100L

  new_data <- as.data.frame(new_data)

  object$spec$method$fit$args$trials <- trials

  args <- list(
    object = expr(object),
    new_data = expr(new_data),
    type = type
  )
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    args <- c(args, dots)
  }
  cl <- rlang::call2(.fn = "predict", !!!args)
  tbl_trial <- tibble::tibble(trees  = rep(trials, nrow(new_data)))
  res <- dplyr::bind_cols(tbl_trial, rlang::eval_tidy(cl))
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
#' @return A tibble with one row for each row of `new_data`. Multiple
#' predictions are contained in a list column called `.pred`. That column has
#' the standard `parsnip` prediction column names as well as the column with
#' the tuning parameter values.
#' @details
#' For C5.0 rule-based models, the model fit may contain less boosting
#' iterations than the number requested. Printing the object will show how many
#' were used due to early stopping. This can be change using an option in
#' [C50::C5.0Control()]. Beware that the number of iterations requested
multi_predict._c5_rules <-
  function(object, new_data, type = NULL, trees = NULL, ...) {
    if (any(names(enquos(...)) == "newdata")) {
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
    }
    trees_used <- object$fit$trials["Actual"]
    trees <- ifelse(trees > trees_used, trees_used, trees)
    trees <- sort(unique(trees))

    if (is.null(type)) {
      type <- "class"
    }

    new_data <- parsnip::prepare_data(object, new_data)
    # preprocess data
    if (!is.null(object$spec$method$pred$numeric$pre)) {
      new_data <- object$spec$method$pred$numeric$pre(new_data, object)
    }

    res <- c5_pred(object, new_data, trials = trees, type = type, ...)

    res$.row_number <- rep(1:nrow(new_data), length(trees))
    res <-
      res %>%
      dplyr::group_by(.row_number) %>%
      tidyr::nest() %>%
      dplyr::ungroup() %>%
      dplyr::select(-.row_number) %>%
      setNames(".pred")
    res
  }

# ------------------------------------------------------------------------------

prob_matrix_to_tibble <- function(x, object) {
  tibble::as_tibble(x)
}
