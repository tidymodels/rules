#' @export
#' @rdname tidy.cubist
#' @param penalty A single numeric value for the `lambda` penalty value.
#' @param unit What data should be returned? For `unit = 'rules'`, each row
#' corresponds to a rule. For `unit = 'columns'`, each row is a predictor
#' column. The latter can be helpful when determining variable importance.
tidy.xrf <- function(x, penalty = NULL, unit = c("rules", "columns"), ...) {
  unit <- match.arg(unit)

  msg <- "Please choose a single numeric value of 'penalty'."
  if (is.null(penalty)) {
    rlang::abort(msg)
  } else {
    if (!is.numeric(penalty) | length(penalty) != 1) {
      rlang::abort(msg)
    }
  }

  lvls <- x$levels
  cat_terms <- expand_xlev(x$glm$xlev)
  coef_table <- xrf_coefs(x, penalty = penalty)
  if (unit == "rules") {
    res <-
      dplyr::left_join(coef_table, cat_terms, by = "term") %>%
      dplyr::mutate(
        rule_comp = xrf_term(column, term, less_than, split_value, level),
        rule_id = ifelse(is.na(rule_id), term, rule_id)
      )
    if (length(lvls) > 2) {
      res <- res %>% dplyr::group_by(rule_id, class)
    } else {
      res <- res %>% dplyr::group_by(rule_id)
    }

    res <- res %>%
      dplyr::summarize(
        rule = paste("(", sort(unique(rule_comp)), ")", collapse = " & "),
        estimate = min(value),
        .groups = "keep"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(rule = ifelse(rule == "( (Intercept) )", "( TRUE )", rule))
  } else {
    res <-
      dplyr::left_join(coef_table, cat_terms, by = "term") %>%
      dplyr::mutate(term = ifelse(is.na(column), term, column)) %>%
      dplyr::rename(estimate = value) %>%
      dplyr::select(dplyr::any_of(c("rule_id", "term", "class", "estimate")))
  }
  res
}

xrf_coefs <- function(x, penalty = NULL) {
  if (is.null(penalty)) {
    penalty <- x$lambda
  }
  if (length(penalty) != 1) {
    rlang::abort("`penalty` should be a single numeric measure.")
  }

  lvls <- x$levels
  rule_info <- x$rules
  feature_coef <- stats::coef(x$glm$model, s = penalty)
  if (is.list(feature_coef)) {
    feature_coef <- purrr::map(feature_coef, ~ as.matrix(.x))
    feature_coef <-
      purrr::map(
        feature_coef,
        ~ as_tibble(.x, .name_repair = "minimal", rownames = "rule_id")
      )
    feature_coef <-
      purrr::map2(
        feature_coef,
        lvls,
        ~ rlang::set_names(.x, c("rule_id", .y))
      )
    tmp <- feature_coef[[1]]
    for (cls in 2:length(feature_coef)) {
      tmp <- dplyr::full_join(tmp, feature_coef[[cls]], by = "rule_id")
    }
    feature_coef <- tmp
  } else {
    feature_coef <- as.matrix(feature_coef)
    feature_coef <-
      as_tibble(
        feature_coef,
        .name_repair = "minimal",
        rownames = "rule_id"
      )
    feature_coef <- rlang::set_names(feature_coef, c("rule_id", "value"))
  }

  feature_coef <- dplyr::full_join(rule_info, feature_coef, by = "rule_id")
  if (length(lvls) > 2) {
    feature_coef <-
      tidyr::pivot_longer(
        feature_coef,
        cols = c(dplyr::one_of(lvls)),
        names_to = "class",
        values_to = "value"
      )
  }

  feature_coef %>%
    # Fix cases where features as added as rules
    dplyr::mutate(
      feature = ifelse(is.na(split_id), rule_id, feature)
    ) %>%
    dplyr::filter(value != 0) %>%
    dplyr::rename(term = feature, split_value = split)
}



lvl_to_tibble <- function(x, var_name) {
  tibble(
    term = paste0(var_name, x),
    column = var_name,
    level = x
  )
}

expand_xlev <- function(x) {
  nms <- names(x)
  if (length(nms) > 0) {
    res <- purrr::map2_dfr(x, nms, lvl_to_tibble)
  } else {
    res <-
      tibble(
        term = NA_character_,
        column = NA_character_,
        level = NA_character_
      )
  }
  res
}

xrf_term <- function(col, trm, lt, val, lvl) {
  ifelse(
    is.na(col),
    xrf_num_term(trm, val, lt),
    xrf_cat_term(col, lvl, lt)
  )
}

xrf_cat_term <- function(col, lvl, lt) {
  # simple dummy variables not in a rule should
  # be included
  lt[is.na(lt)] <- FALSE
  lvl <- paste0("'", lvl, "'")
  ifelse(
    lt,
    paste(col, "!=", lvl),
    paste(col, "==", lvl)
  )
}

xrf_num_term <- function(col, val, lt) {
  res <-
    ifelse(
      lt,
      paste(col, "< ", val),
      paste(col, ">=", val)
    )
  # no rule terms
  res <-
    ifelse(
      is.na(lt),
      col,
      res
    )
  res
}
