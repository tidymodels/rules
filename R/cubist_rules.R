cubist_args <- function(x) {
  ctrl_args <- c("unbiased", "rules", "extrapolation", "sample", "seed", "label")

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
    x = rlang::expr(x),
    y = rlang::expr(y),
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

  purrr::map_dfr(
    neighbors,
    cubist_pred_wrap,
    object = object,
    new_data = new_data,
    ...
  )
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
committees <- function(range = c(1L, 100L), trans = NULL) {
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
max_rules <- function(range = c(1L, 500L), trans = NULL) {
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
    name = c("committees", "neighbors", "max_rules"),
    call_info = list(
      list(pkg = "rules", fun = "committees", range = c(1L, 100L)),
      list(pkg = "dials", fun = "neighbors", range = c(0L, 9L)),
      list(pkg = "rules", fun = "max_rules")
    ),
    source = "model_spec",
    component = class(x)[class(x) != "model_spec"][1],
    component_id = "main"
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
