#' Internal function wrappers
#'
#' These are not supported when called by the user.
#' @export
#' @keywords internal
#' @rdname rules-internal
c5_fit <- function(x, y, trials = 1, minCases = 2, cost = NULL, ...) {
  args <- list(
    x = rlang::expr(x),
    y = rlang::expr(y),
    trials = rlang::expr(trials)
  )
  dots <- rlang::enquos(...)
  # trim rules and costs from dots
  if (length(dots) > 0) {
    if (any(names(dots) == "control")) {
      dots$control$minCases <- minCases
      if (!any(names(dots$control) == "seed")) {
        dots$control$seed <- sample.int(10^5, 1)
      }
      if (!any(names(dots$control) == "earlyStopping")) {
        dots$control$earlyStopping <- FALSE
      }
    }
    args <- c(args, dots)
  } else {
    args <-
      c(
        args,
        list(
          control =
            rlang::expr(C50::C5.0Control(minCases = minCases, seed = sample.int(10^5, 1), earlyStopping = FALSE))
        )
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

  new_data <- as.data.frame(new_data)

  object$spec$method$fit$args$trials <- trials

  args <- list(
    object = rlang::expr(object),
    new_data = rlang::expr(new_data),
    type = type
  )
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    args <- c(args, dots)
  }
  cl <- rlang::call2(.fn = "predict", !!!args)
  tbl_trial <- tibble::tibble(trees = rep(trials, nrow(new_data)))
  res <- dplyr::bind_cols(tbl_trial, rlang::eval_tidy(cl))
  res
}

#' @export
#' @keywords internal
#' @rdname rules-internal
c5_pred <- function(object, new_data, trials = object$fit$trials["Actual"], ...) {
  res <- purrr::map_dfr(
    trials,
    c5_pred_wrap,
    object = object,
    new_data = new_data,
    ...
  )
  if (length(trials) == 1) {
    res <- res %>% dplyr::select(-trials)
  }
  res
}

# ------------------------------------------------------------------------------

prob_matrix_to_tibble <- function(x, object) {
  tibble::as_tibble(x)
}

# ------------------------------------------------------------------------------


#' @export
#' @keywords internal
#' @rdname rules-internal
tunable.C5_rules <- function(x, ...) {
  res <- NextMethod()

  if (x$engine == "C5.0") {
    res <- add_engine_parameters(res, c5_rules_engine_args)

    res$call_info[res$name == "trees"] <-
      list(list(pkg = "dials", fun = "trees", range = c(1, 100)))
    res$call_info[res$name == "min_n"] <-
      list(list(pkg = "dials", fun = "min_n", range = c(2L, 40L)))
  }

  res
}

add_engine_parameters <- function(pset, engines) {
  is_engine_param <- pset$name %in% engines$name
  if (any(is_engine_param)) {
    engine_names <- pset$name[is_engine_param]
    pset <- pset[!is_engine_param,]
    pset <-
      dplyr::bind_rows(pset, engines %>% dplyr::filter(name %in% engines$name))
  }
  pset
}

c5_rules_engine_args <-
  tibble::tibble(
    name = c(
      "CF",
      "noGlobalPruning",
      "winnow",
      "fuzzyThreshold",
      "bands"
    ),
    call_info = list(
      list(pkg = "dials", fun = "confidence_factor"),
      list(pkg = "dials", fun = "no_global_pruning"),
      list(pkg = "dials", fun = "predictor_winnowing"),
      list(pkg = "dials", fun = "fuzzy_thresholding"),
      list(pkg = "dials", fun = "rule_bands")
    ),
    source = "model_spec",
    component = "C5_rules",
    component_id = "engine"
  )
