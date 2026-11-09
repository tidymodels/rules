#' Internal function wrappers
#'
#' These are not supported when called by the user.
#' @export
#' @keywords internal
#' @rdname rules-internal
c5_fit <- function(x, y, trials = 1, minCases = 2, cost = NULL, ...) {
  rlang::check_installed("C50")
  args <- list(
    x = rlang::expr(x),
    y = rlang::expr(y),
    trials = rlang::expr(trials)
  )
  dots <- rlang::enquos(...)
  # trim rules and costs from dots
  ctrl_names <- setdiff(names(formals(C50::C5.0Control)), "minCases")
  is_ctrl <- names(dots) %in% ctrl_names
  ctrl_dots <- as.list(dots[is_ctrl])
  dots <- as.list(dots[!is_ctrl])
  has_control <- any(names(dots) == "control")

  if (has_control) {
    control <- rlang::eval_tidy(dots$control)
    dots$control <- NULL
    if (length(ctrl_dots) > 0) {
      control <- merge_c5_control(control, ctrl_dots)
    }
    control$minCases <- minCases
    if (!any(names(control) == "seed")) {
      control$seed <- sample.int(10^5, 1)
    }
    if (!any(names(control) == "earlyStopping")) {
      control$earlyStopping <- FALSE
    }
  } else {
    if (!any(names(ctrl_dots) == "seed")) {
      ctrl_dots$seed <- sample.int(10^5, 1)
    }
    if (!any(names(ctrl_dots) == "earlyStopping")) {
      ctrl_dots$earlyStopping <- FALSE
    }
    control <- rlang::call2(
      "C5.0Control",
      .ns = "C50",
      minCases = minCases,
      !!!ctrl_dots
    )
  }

  args <- c(args, list(control = control), dots)
  args$rules <- TRUE

  if (!is.null(cost)) {
    if (length(levels(y)) != 2) {
      cli::cli_abort("Cost-sensitive models only implemented for 2 classes.")
    }
    costs <- matrix(
      c(0, 1, cost, 0),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(levels(y), levels(y))
    )
    args$costs <- costs
  }

  cl <- rlang::call2(.fn = "C5.0", .ns = "C50", !!!args)
  res <- rlang::eval_tidy(cl)
  res
}

merge_c5_control <- function(control, ctrl_dots, call = rlang::caller_env()) {
  ctrl_vals <- purrr::map(ctrl_dots, rlang::eval_tidy)
  ctrl_defaults <- formals(C50::C5.0Control)
  # The default seed is a random draw, so a user-set value can't be detected.
  check_nms <- setdiff(names(ctrl_vals), "seed")
  is_dupe <- purrr::map_lgl(
    check_nms,
    \(nm) !identical(control[[nm]], ctrl_defaults[[nm]])
  )
  dupes <- check_nms[is_dupe]
  if (length(dupes) > 0) {
    cli::cli_warn(
      "{.arg {dupes}} {?was/were} specified in both {.arg control} and as
       engine argument{?s}. Using the engine argument value{?s}.",
      call = call
    )
  }
  rlang::exec(C50::C5.0Control, !!!utils::modifyList(control, ctrl_vals))
}

c5_pred_wrap <- function(trials = 1, object, new_data, type = "class", ...) {
  if (length(trials) > 1) {
    cli::cli_abort("`c5_pred_wrap` takes a single value of {.arg trials}")
  }
  trials[trials < 1] <- 1L
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
  tbl_trial <- tibble(trees = rep(trials, nrow(new_data)))
  res <- dplyr::bind_cols(tbl_trial, rlang::eval_tidy(cl))
  res
}

#' @export
#' @keywords internal
#' @rdname rules-internal
c5_pred <- function(
  object,
  new_data,
  trials = object$fit$trials["Actual"],
  ...
) {
  res <- purrr::map_dfr(
    trials,
    c5_pred_wrap,
    object = object,
    new_data = new_data,
    ...
  )
  if (length(trials) == 1) {
    res <- res |> dplyr::select(-trials)
  }
  res
}

# ------------------------------------------------------------------------------

prob_matrix_to_tibble <- function(x, object) {
  as_tibble(x)
}
