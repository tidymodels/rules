#' @export
#' @keywords internal
#' @rdname rules-internal
xrf_fit <-
  function(
    formula,
    data,
    max_depth = 6,
    nrounds = 15,
    eta = 0.3,
    colsample_bynode = NULL,
    colsample_bytree = NULL,
    min_child_weight = 1,
    gamma = 0,
    subsample = 1,
    validation = 0,
    early_stop = NULL,
    counts = TRUE,
    event_level = c("first", "second"),
    lambda = 0.1,
    ...
  ) {
    converted <-
      parsnip::.convert_form_to_xy_fit(
        formula = formula,
        data = data
      )

    prefit <-
      parsnip::xgb_train(
        converted$x,
        converted$y,
        max_depth = max_depth,
        nrounds = nrounds,
        eta = eta,
        colsample_bynode = colsample_bynode,
        colsample_bytree = colsample_bytree,
        min_child_weight = min_child_weight,
        gamma = gamma,
        subsample = subsample,
        validation = validation,
        early_stop = early_stop,
        objective = NULL,
        counts = counts,
        event_level = event_level
      )

    args <-
      list(
        object = formula,
        data = rlang::expr(data),
        prefit_xgb = prefit
      )

    dots <- rlang::enquos(...)

    # ignore parsnip-protected argument
    dots[["xgb_control"]] <- NULL

    if (!any(names(dots) == "family")) {
      info <- get_family(formula, data)
      args$family <- info$fam
      if (info$fam == "multinomial") {
        # have to mock an xgb_control object for xrf for now
        args$xgb_control <- list()
        args$xgb_control$num_class <- info$classes
        args$xgb_control$nrounds <- 10
      }
    }
    if (length(dots) > 0) {
      args <- c(args, dots)
    }

    cl <- rlang::call2(.fn = "xrf", .ns = "xrf", !!!args)
    res <- rlang::eval_tidy(cl)

    res$lambda <- lambda
    res$family <- args$family
    res$levels <- get_levels(formula, data)
    res
  }

process_mtry <- function(colsample_bytree, counts, n_predictors, is_missing) {
  if (!is.logical(counts)) {
    rlang::abort("'counts' should be a logical value.")
  }

  ineq <- if (counts) {
    "greater"
  } else {
    "less"
  }
  interp <- if (counts) {
    "count"
  } else {
    "proportion"
  }
  opp <- if (!counts) {
    "count"
  } else {
    "proportion"
  }

  if ((colsample_bytree < 1 & counts) | (colsample_bytree > 1 & !counts)) {
    rlang::abort(
      paste0(
        "The supplied argument `mtry = ",
        colsample_bytree,
        "` must be ",
        ineq,
        " than or equal to 1. \n\n`mtry` is currently being interpreted ",
        "as a ",
        interp,
        " rather than a ",
        opp,
        ". Supply `counts = ",
        !counts,
        "` to `set_engine()` to supply this argument as a ",
        opp,
        " rather than ",
        # TODO: add a section to the linked parsnip docs on mtry vs mtry_prop
        "a ",
        interp,
        ". \n\nSee `?details_rule_fit_xrf` for more details."
      ),
      call = NULL
    )
  }

  if (rlang::is_call(colsample_bytree)) {
    if (rlang::call_name(colsample_bytree) == "tune") {
      rlang::abort(
        paste0(
          "The supplied `mtry` parameter is a call to `tune`. Did you forget ",
          "to optimize hyperparameters with a tuning function like `tune::tune_grid`?"
        ),
        call = NULL
      )
    }
  }

  if (counts && !is_missing) {
    colsample_bytree <- colsample_bytree / n_predictors
  }

  colsample_bytree
}

get_family <- function(formula, data) {
  m <- model.frame(formula, head(data))
  y <- model.response(m)
  if (is.numeric(y)) {
    if (is.integer(y)) {
      res <- "poisson"
    } else {
      res <- "gaussian"
    }
    lvl <- NA
  } else {
    if (is.character(y)) {
      y <- factor(y)
    }
    lvl <- levels(y)
    if (length(lvl) > 2) {
      res <- "multinomial"
    } else {
      res <- "binomial"
    }
  }
  list(fam = res, classes = length(lvl))
}

get_glmnet_type <- function(x, type) {
  fam <- x$fit$family
  if (fam %in% c("binomial", "multinomial")) {
    if (rlang::is_missing(type)) {
      type <- "response"
    } else {
      if (type == "prob") {
        type <- "response"
      }
    }
  } else {
    type <- "response"
  }
  type
}


get_levels <- function(formula, data) {
  m <- model.frame(formula, head(data))
  y <- model.response(m)
  res <- levels(y)
  if (length(res) == 0) {
    res <- character(0)
  }
  res
}

#' @export
#' @keywords internal
#' @rdname rules-internal
xrf_pred <- function(object, new_data, lambda = object$fit$lambda, type, ...) {
  lambda <- sort(lambda)

  res <- predict(object$fit, new_data, lambda = lambda, type = "response")
  family <- rlang::as_name(object$fit$family)
  if (type != "prob") {
    res <- organize_xrf_multi_pred(res, object, lambda, family)
  } else {
    res <- organize_xrf_multi_prob(res, object, lambda, family)
  }
  res
}

#' @rdname multi_predict
#' @export
#' @param penalty Non-negative penalty values.
#' @param ... Not currently used.
multi_predict._xrf <-
  function(object, new_data, type = NULL, penalty = NULL, ...) {
    if (any(names(enquos(...)) == "newdata")) {
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
    }
    if (is.null(penalty)) {
      penalty <- object$fit$lambda
    }

    if (is.null(type)) {
      fam <- object$fit$family
      if (fam %in% c("binomial", "multinomial")) {
        type <- "class"
      } else {
        type <- "numeric"
      }
    }

    new_data <- parsnip::prepare_data(object, new_data)
    # preprocess data
    if (!is.null(object$spec$method$pred$numeric$pre)) {
      new_data <- object$spec$method$pred$numeric$pre(new_data, object)
    }

    res <- xrf_pred(object, new_data, lambda = penalty, type = type, ...)
    res
  }

# ------------------------------------------------------------------------------

organize_xrf_pred <- function(x, object) {
  res <- dplyr::pull(x, .pred)
  res <- unname(res)
}

organize_xrf_class_pred <- function(x, object) {
  x <- tidyr::unnest(x, cols = c(.pred))
  lams <- unique(x$penalty)
  if (length(lams) > 1) {
    x$penalty <- NULL
  }
  x
}

organize_xrf_class_prob <- function(x, object) {
  if (!inherits(x, "array")) {
    x <- x[, 1]
    x <- tibble(v1 = 1 - x, v2 = x)
  } else {
    x <- x[,, 1]
    x <- as_tibble(x)
  }
  colnames(x) <- object$lvl
  x
}

organize_xrf_multi_pred <- function(x, object, penalty, fam) {
  if (fam %in% c("gaussian", "poisson")) {
    if (ncol(x) == 1) {
      res <- tibble(penalty = rep(penalty, nrow(x)), .pred = unname(x[, 1]))
    } else {
      res <-
        as_tibble(x) |>
        dplyr::mutate(.row_number = seq_len(nrow(x))) |>
        tidyr::pivot_longer(cols = c(-.row_number), values_to = ".pred") |>
        dplyr::mutate(penalty = rep(penalty, nrow(x))) |>
        dplyr::select(-name) |>
        dplyr::group_by(.row_number) |>
        tidyr::nest() |>
        dplyr::ungroup() |>
        dplyr::select(-.row_number) |>
        setNames(".pred")
    }
  } else {
    if (fam == "binomial") {
      res <-
        as_tibble(x) |>
        dplyr::mutate(.row_number = seq_len(nrow(x))) |>
        tidyr::pivot_longer(
          cols = c(-.row_number),
          values_to = ".pred_class"
        ) |>
        dplyr::select(-name) |>
        dplyr::mutate(
          .pred_class = ifelse(.pred_class >= .5, object$lvl[2], object$lvl[1]),
          .pred_class = factor(.pred_class, levels = object$lvl)
        )

      if (length(penalty) == 1) {
        # predict
        res <- dplyr::pull(res, .pred_class)
      } else {
        # multipredict
        res <-
          res |>
          dplyr::mutate(penalty = rep(penalty, nrow(x))) |>
          dplyr::group_by(.row_number) |>
          tidyr::nest() |>
          dplyr::ungroup() |>
          dplyr::select(-.row_number) |>
          setNames(".pred")
      }
    } else {
      # fam = "multinomial"
      res <-
        apply(x, 3, function(x) apply(x, 1, which.max)) |>
        as_tibble() |>
        dplyr::mutate(.row_number = seq_len(nrow(x))) |>
        tidyr::pivot_longer(
          cols = c(-.row_number),
          values_to = ".pred_class"
        ) |>
        dplyr::select(-name) |>
        dplyr::mutate(
          .pred_class = object$lvl[.pred_class],
          .pred_class = factor(.pred_class, levels = object$lvl)
        )
      if (length(penalty) == 1) {
        # predict
        res <- dplyr::pull(res, .pred_class)
      } else {
        # multi-predict
        res <-
          res |>
          dplyr::mutate(penalty = rep(penalty, nrow(x))) |>
          dplyr::group_by(.row_number) |>
          tidyr::nest() |>
          dplyr::ungroup() |>
          dplyr::select(-.row_number) |>
          setNames(".pred")
      }
    }
  }
  res
}

organize_xrf_multi_prob <- function(x, object, penalty, fam) {
  if (fam == "binomial") {
    res <-
      as_tibble(x) |>
      dplyr::mutate(.row_number = seq_len(nrow(x))) |>
      tidyr::pivot_longer(cols = c(-.row_number), values_to = ".pred_2") |>
      dplyr::mutate(penalty = rep(penalty, nrow(x))) |>
      dplyr::select(-name) |>
      dplyr::mutate(.pred_1 = 1 - .pred_2) |>
      dplyr::select(.row_number, penalty, .pred_1, .pred_2)

    if (length(penalty) == 1) {
      # predict
      res <-
        res |>
        setNames(c(".row_number", "penalty", object$lvl)) |>
        dplyr::select(-.row_number, -penalty)
    } else {
      # multi_predict
      res <-
        res |>
        setNames(c(".row_number", "penalty", paste0(".pred_", object$lvl))) |>
        dplyr::group_by(.row_number) |>
        tidyr::nest() |>
        dplyr::ungroup() |>
        dplyr::select(-.row_number) |>
        setNames(".pred")
    }
  } else {
    # fam = "multinomial"
    res <-
      apply(x, 3, as_tibble) |>
      dplyr::bind_rows() |>
      setNames(object$lvl)

    # good format for predict()
    if (length(penalty) > 1) {
      # multi_predict
      res <-
        res |>
        dplyr::mutate(.row_number = rep(seq_len(nrow(x)), length(penalty))) |>
        dplyr::mutate(penalty = rep(penalty, each = nrow(x))) |>
        dplyr::group_by(.row_number) |>
        tidyr::nest() |>
        dplyr::ungroup() |>
        dplyr::select(-.row_number) |>
        setNames(".pred")
    }
  }
  res
}

#' @export
#' @keywords internal
#' @rdname rules-internal
tunable.rule_fit <- function(x, ...) {
  tibble(
    name = c(
      "mtry",
      "trees",
      "min_n",
      "tree_depth",
      "learn_rate",
      "loss_reduction",
      "sample_size",
      "penalty"
    ),
    call_info = list(
      list(pkg = "dials", fun = "mtry_prop"),
      list(pkg = "dials", fun = "trees", range = c(5L, 100L)),
      list(pkg = "dials", fun = "min_n"),
      list(pkg = "dials", fun = "tree_depth", range = c(1L, 10L)),
      list(pkg = "dials", fun = "learn_rate", range = c(-10, 0)),
      list(pkg = "dials", fun = "loss_reduction"),
      list(pkg = "dials", fun = "sample_prop", range = c(0.50, 0.95)),
      list(pkg = "dials", fun = "penalty")
    ),
    source = "model_spec",
    component = class(x)[class(x) != "model_spec"][1],
    component_id = "main"
  )
}
