
#' @rdname tidy.cubist
#' @export
tidy.C5.0 <- function(x, trials = x$trials["Actual"], ...) {
  if (!x$rbm) {
    rlang::abort("`tidy()` method only implemented for C5.0 rules (not trees).")
  }
  txt <- x$rules
  txt_rows <- stringr::str_split(txt, pattern = "\n") %>% unlist()

  if (!is.null(trials)) {
    trials <- min(trials, x$trials["Actual"])
  }

  # These are the markers for where trees start
  trial_inds <- stringr::str_which(txt_rows, "^rules=")

  # Truncate them when needed
  total_trial <- length(trial_inds)
  num_trial <- min(trials, total_trial)

  if (trials < total_trial) {
    trial_inds <- trial_inds[1:(trials + 1)]
  }
  trial_res <- list(length = num_trial)

  for (i in 1:num_trial) {
    loc <- trial_inds[i]
    # Get the locations of the model file that encompasses the rows of this trial
    if (i < total_trial) {
      uppr <- trial_inds[i + 1] - 1
    } else {
      uppr <- length(txt_rows)
    }
    num_rules <- get_num_rules(txt_rows[loc])
    trial_data <-
      tibble(
        rule_num = 1:num_rules,
        rule = NA
      )
    # Where are the lines that show the `conds` lines
    attr_inds <- find_cond_info(txt_rows, loc, uppr)
    cond_att <- purrr::map_dfr(attr_inds, parse_cond, txt = txt_rows)
    trial_data <-
      dplyr::bind_cols(trial_data, cond_att) %>%
      dplyr::mutate(num_conditions = conds) %>%
      dplyr::rename(coverage = cover) %>%
      dplyr::select(-ok) %>%
      tidyr::nest(statistic = c(num_conditions, coverage, lift, class))

    # Loop over all of the rules and get their rule conditions
    for (j in seq_along(attr_inds)) {
      att_loc <- attr_inds[j] + 1:trial_data$conds[j]
      atts <- purrr::map_chr(txt_rows[att_loc], make_conds)
      # case with rule with no conditions
      if (all(nchar(atts) == 0)) {
        atts <- "<no conditions>"
      } else {
        atts <- stringr::str_c(atts, collapse = " & ")
      }
      trial_data$rule[j] <- atts
    }

    trial_data$trial <- i
    trial_res[[i]] <- trial_data
  }

  res <-
    dplyr::bind_rows(trial_res) %>%
    dplyr::select(trial, rule_num, rule, statistic)
  res
}
