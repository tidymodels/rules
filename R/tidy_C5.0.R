#' @rdname tidy.cubist
#' @export
tidy.C5.0 <- function(x, trees = x$trials["Actual"], ...) {
  if (x$rbm) {
    res <- parse_rule_file(x, trees)
  } else {
    res <- parse_tree_file(x, trees)
  }
  res
}

# ------------------------------------------------------------------------------
# parsing rule models

parse_rule_file <- function(x, trials = x$trials["Actual"], ...) {
  txt <- x$rules
  txt_rows <- stringr::str_split(txt, pattern = "\n") |> unlist()

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
      dplyr::bind_cols(trial_data, cond_att) |>
      dplyr::mutate(num_conditions = conds) |>
      dplyr::rename(coverage = cover) |>
      dplyr::select(-ok) |>
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
    dplyr::bind_rows(trial_res) |>
    dplyr::select(trial, rule_num, rule, statistic)
  res
}

# ------------------------------------------------------------------------------
# parsing tree models

parse_tree_file <- function(x, trials = x$trials["Actual"]) {
  tree_raw <- x$tree |>
    stringr::str_split("\n") |>
    purrr::pluck(1)

  levels <- get_variable_levels(x)

  n_trees <- stringr::str_extract(tree_raw[2], "[0-9]+")
  n_trees <- as.integer(n_trees)

  tree <- tree_raw[seq(3, length(tree_raw) - 1)]
  tree <- stringr::str_subset(tree, "freq=")
  tree <- lapply(strsplit(tree, " "), make_list)

  res <- parse_tree(tree, n_trees, levels, lvls = x$levels)
  res[res$trial <= trials, ]
}

# is this working across all boosting iterations?
parse_tree <- function(input, n_trees, levels, lvls) {
  trees <- list()

  index <- 1

  for (i in seq_len(n_trees)) {
    rules <- get_rule_index(index, input, levels = levels)

    tree_tbl <- dplyr::tibble(
      trial = i,
      node = seq_along(rules),
      rule = purrr::map_chr(rules, parse_rule),
      statistic = purrr::map(rules, get_freqs, tree = input, lvls = lvls)
    )

    trees[i] <- list(tree_tbl)

    index <- max(unlist(rules)) + 1
  }

  dplyr::bind_rows(trees)
}


get_rule_index <- function(index, tree, history = c(), levels) {
  res <- list()
  history <- c(history, index)
  curr <- tree[[index]]

  if (curr$type == 0) {
    # A situation where there are no splits, such as the first two here:
    #
    # species = Chinstrap: Dream (68)
    # species = Gentoo: Biscoe (119)
    # species = Adelie:
    #   :...sex = male:

    return(list(history))
  } else if (curr$type == 1) {
    # Case with binary split on a categorical predictor where there are
    # only two possible levels

    new_rules <- list()
    elts <- levels[[curr$att]]
    for (i in seq_along(elts)) {
      value <- paste0("\"", elts[i], "\"")
      rule_name <- paste("(", curr$att, "==", value, ")")
      rule_index <- stats::setNames(
        max(c(index, unlist(new_rules))) + 1,
        rule_name
      )

      new_rule <- get_rule_index(
        index = rule_index,
        tree = tree,
        history = history,
        levels = levels
      )
      new_rules <- c(new_rules, new_rule)
    }
    res <- c(res, new_rules)
  } else if (curr$type == 2) {
    # A binary split on a numeric predictor

    rule_le_name <- paste("(", curr$att, "<=", curr$cut, ")")
    rule_le_index <- stats::setNames(index + 1, rule_le_name)

    rule_le_ <- get_rule_index(rule_le_index, tree, history, levels)
    rule_gt_name <- paste("(", curr$att, "> ", curr$cut, ")")
    rule_gt_index <- stats::setNames(max(unlist(rule_le_)) + 1, rule_gt_name)

    rule_gt <- get_rule_index(rule_gt_index, tree, history, levels)

    res <- c(res, rule_le_, rule_gt)
  } else if (curr$type == 3) {
    # A split with 3+ branches on a categorical predictor

    new_rules <- list()
    elts <- curr$elt
    for (i in seq_along(elts)) {
      value <- paste0("c(", elts[i], ")")
      rule_name <- paste("(", curr$att, "%in%", value, ")")
      rule_index <- stats::setNames(
        max(c(index, unlist(new_rules))) + 1,
        rule_name
      )

      new_rule <- get_rule_index(
        index = rule_index,
        tree = tree,
        history = history,
        levels = levels
      )
      new_rules <- c(new_rules, new_rule)
    }

    res <- c(res, new_rules)
  } else {
    msg <- paste("Unknown split type", curr$type)
    cli::cli_abort(msg)
  }

  res
}

parse_rule <- function(x) {
  x <- names(x)
  x <- x[-1]
  x <- paste(x, collapse = " & ")
  x
}

get_freqs <- function(rule, tree, lvls) {
  last <- utils::tail(rule, 1)

  # These can be fraction based on how C5.0 deals with missing data
  freqs <- tree[[last]]$freq
  freqs <- stringr::str_remove_all(freqs, "\"")
  freqs <- stringr::str_split(freqs, ",")[[1]]
  freqs <- as.numeric(freqs)

  if (length(freqs) != length(lvls)) {
    msg <- paste0(
      "The number of counts (",
      length(freqs),
      ") is not the same as ",
      "the number of levels (",
      length(lvls),
      ")."
    )
    cli::cli_abort(msg)
  }
  tibble::tibble(value = lvls, count = freqs)
}


make_list <- function(x) {
  bbb <- strsplit(x, "=")

  res <- lapply(bbb, function(x) x[2])
  names(res) <- vapply(bbb, function(x) x[1], FUN.VALUE = character(1))

  res$type <- stringr::str_remove_all(res$type, "\"")
  res$type <- as.integer(res$type)

  if (!is.null(res$att)) {
    res$att <- stringr::str_remove(res$att, "^\"")
    res$att <- stringr::str_remove(res$att, "\"$")
  }
  if (!is.null(res$cut)) {
    res$cut <- stringr::str_remove(res$cut, "^\"")
    res$cut <- stringr::str_remove(res$cut, "\"$")
  }
  if (!is.null(res$elts)) {
    res$elt <- unlist(unname(res[names(res) == "elts"]))
    res[names(res) == "elts"] <- NULL
  }
  res
}

get_variable_levels <- function(model) {
  res <- stringr::str_split(model$names, "\n")[[1]]
  res <- res[-c(1:4)]
  res <- head(res, -1)
  res <- stringr::str_replace_all(res, "\\\\_", "_")
  res <- stringr::str_remove(res, ".$")
  res <- stringr::str_subset(res, "continuous$", negate = TRUE)
  res <- stringr::str_split(res, ": ")
  res <- setNames(
    lapply(res, function(x) stringr::str_split(x[2], ",")[[1]]),
    vapply(res, function(x) x[1], FUN.VALUE = character(1))
  )
  res
}
