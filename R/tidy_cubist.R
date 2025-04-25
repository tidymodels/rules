#' Turn C5.0 and rule-based models into tidy tibbles
#'
#' @param x A `Cubist`, `C5.0`, or `xrf` object.
#' @param committees The number of committees to tidy (defaults to the entire
#' ensemble).
#' @param trees The number of boosting iterations to tidy (defaults to the entire
#' ensemble).
#' @param penalty A single numeric value for the `lambda` penalty value.
#' @param unit What data should be returned? For `unit = 'rules'`, each row
#' corresponds to a rule. For `unit = 'columns'`, each row is a predictor
#' column. The latter can be helpful when determining variable importance.
#' @param ... Not currently used.
#' @includeRmd man/rmd/tidy-example.Rmd details
#' @export
tidy.cubist <- function(x, committees = x$committee, ...) {
  txt <- x$model
  txt_rows <- stringr::str_split(txt, pattern = "\n") |> unlist()

  if (!is.null(committees)) {
    committees <- min(committees, x$committee)
  }

  # These are the markers for where committees start
  comm_inds <- stringr::str_which(txt_rows, "^rules=")

  # Truncate them when needed
  total_comm <- length(comm_inds)
  num_comm <- min(committees, total_comm)
  comm_inds <- comm_inds[1:(committees + 1)] # Need 1 extra to find boundary of committee info

  # container for results for each committee
  comms <- list(length = num_comm)

  # Within each committee, elements are `type` (for each condition) and `coef`
  # (for model eq). A rule starts with a `conds` element and that tells us
  # how many lines make up the rule elements. The rule elements start right after
  # the `rules` line. Immediately after these is a single line with the information
  # on the regression equation.

  for (i in 1:num_comm) {
    loc <- comm_inds[i]
    # Get the locations of the model file that encompasses the committee's rows
    if (i < total_comm) {
      uppr <- comm_inds[i + 1] - 1
    } else {
      uppr <- length(txt_rows)
    }
    num_rules <- get_num_rules(txt_rows[loc])
    comm_data <-
      tibble(
        rule_num = 1:num_rules,
        rule = NA,
        estimate = NA
      )
    # Where are the lines that show the `conds` lines
    attr_inds <- find_cond_info(txt_rows, loc, uppr)
    cond_att <- purrr::map_dfr(attr_inds, parse_cond, txt = txt_rows)
    comm_data <-
      dplyr::bind_cols(comm_data, cond_att) |>
      dplyr::mutate(num_conditions = conds) |>
      dplyr::rename(
        coverage = cover,
        min = loval,
        max = hival,
        error = esterr
      ) |>
      tidyr::nest(
        statistic = c(num_conditions, coverage, mean, min, max, error)
      )

    # Loop over all of the rules and get their rule conditions
    for (j in seq_along(attr_inds)) {
      att_loc <- attr_inds[j] + 1:comm_data$conds[j]
      atts <- purrr::map_chr(txt_rows[att_loc], make_conds)
      # case with rule with no conditions
      if (all(nchar(atts) == 0)) {
        atts <- "<no conditions>"
      } else {
        atts <- stringr::str_c(atts, collapse = " & ")
      }
      comm_data$rule[j] <- atts
    }

    # Get regression equations
    eq_ind <- attr_inds + comm_data$conds + 1
    comm_data$estimate <- purrr::map(txt_rows[eq_ind], get_reg_data)
    comm_data$committee <- i
    comms[[i]] <- comm_data
  }
  res <-
    dplyr::bind_rows(comms) |>
    dplyr::select(committee, rule_num, rule, estimate, statistic)
  res
}

# ------------------------------------------------------------------------------

find_cond_info <- function(txt, strt = 0, stp = 0) {
  txt <- txt[(strt + 1):(stp - 1)]
  stringr::str_which(txt, "^conds=") + strt
}

parse_cond <- function(ind, txt) {
  entires <- stringr::str_split(txt[ind], " ") |> unlist()
  tmp <- purrr::map(
    entires,
    \(.x) stringr::str_split(.x, pattern = "=") |> unlist()
  )
  nms <- purrr::map_chr(tmp, purrr::pluck, 1)
  info <- purrr::map(tmp, stringr::str_remove_all, pattern = "\"")
  info_name <- purrr::map_chr(info, \(.x) .x[1])
  info_value <- purrr::map_chr(info, \(.x) .x[2])
  info_value <- purrr::map2(info_name, info_value, convert_info)
  names(info_value) <- info_name
  as_tibble(info_value)
}

convert_info <- function(nm, val) {
  if (nm != "class") {
    val <- as.numeric(val)
  }
  val
}


get_num_rules <- function(txt) {
  res <- stringr::str_split(txt, " ") |> unlist()
  res_ind <- stringr::str_which(res, "^rules=")
  res <- res[res_ind]
  res <- stringr::str_remove(res, "^rules=")
  res <- stringr::str_remove_all(res, '\"')
  as.integer(res)
}


# ------------------------------------------------------------------------------

get_reg_data <- function(txt, results = "expression") {
  entires <- stringr::str_split(txt, " ") |> unlist()
  n <- length(entires)
  vals <- purrr::map_chr(entires, reg_terms)
  res <- vals[1]
  vals <- vals[-1]
  if (length(vals) > 0) {
    n_elem <- length(vals)
    if (n_elem %% 2 != 0) {
      rlang::abort("number of remaining terms not even", call. = FALSE)
    }
    n_terms <- n_elem / 2
    split_terms <- split(vals, rep(1:n_terms, each = 2))
    res <- splits_to_coefs(split_terms, res)
  } else {
    res <- tibble(term = "(Intercept)", estimate = as.numeric(res))
  }
  res
}

splits_to_coefs <- function(x, int) {
  num_check <- purrr::map_int(x, length)
  if (!all(num_check == 2)) {
    rlang::abort("Problem with getting coefficients")
  }
  coef_val <- purrr::map_dbl(x, \(.x) as.numeric(.x[2]))
  res <- tibble(term = purrr::map_chr(x, \(.x) .x[1]), estimate = coef_val)
  res <- dplyr::bind_rows(
    tibble(term = "(Intercept)", estimate = as.numeric(int)),
    res
  )
  res
}

splits_to_eqn <- function(x, int) {
  terms <- purrr::map_chr(x, paste_slopes)
  terms <- stringr::str_c(terms, collapse = " + ")
  stringr::str_c(int, " + ", terms, collapse = "")
}

reg_terms <- function(txt) {
  if (stringr::str_detect(txt, "^coeff")) {
    val <- stringr::str_remove(txt[1], "coeff=\"")
    val <- stringr::str_remove(val, "\"")
  } else {
    val <- stringr::str_remove(txt[1], "att=\"")
    val <- stringr::str_remove(val, "\"")
  }
  val
}

paste_slopes <- function(txt) {
  stringr::str_c("(", txt[2], "*", txt[1], ")", sep = " ")
}

# ------------------------------------------------------------------------------

make_conds <- function(txt) {
  # When there are no rule conditions, return nothing
  if (grepl("^coef", txt) | grepl("^conds=\"0\"", txt)) {
    return("")
  }
  res <- purrr::map_chr(txt, single_cond)
  res <- stringr::str_c(res, collapse = " & ")
  res <- stringr::str_replace_all(res, "\"", "'")
  res
}

single_cond <- function(txt) {
  if (stringr::str_detect(txt, "type=\"2")) {
    res <- cb_cond_2(txt)
  } else {
    res <- cb_cond_3(txt)
  }
  res
}

cb_cond_2 <- function(txt) {
  entires <- stringr::str_split(txt, " ") |> unlist()
  rms <- "(att=\")|(cut=\")|(result=\")"
  entires <- purrr::map_chr(entires, stringr::str_remove_all, rms)
  entires <- purrr::map_chr(entires, stringr::str_remove_all, "\"")
  stringr::str_c("(", entires[2], entires[4], entires[3], ")", sep = " ")
}

cb_cond_3 <- function(txt) {
  entires <- stringr::str_split(txt, " ") |> unlist()
  var_name <- entires[2]
  var_name <- stringr::str_remove(var_name, "att=\"")
  var_name <- stringr::str_remove(var_name, "\"")
  elts <- entires[3]
  elts <- stringr::str_remove(elts, "elts=")

  if (!stringr::str_detect(elts, ",")) {
    elts <- stringr::str_remove(elts, "val=")
    res <- stringr::str_c("(", var_name, "==", elts, ")", sep = " ")
  } else {
    res <- stringr::str_c("(", var_name, " %in% c(", elts, ") )", sep = " ")
  }
  res
}
