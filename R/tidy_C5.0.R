
#' @rdname tidy.cubist
#' @param trees The number of boosting iterations to tidy (defaults to the entire
#' ensemble).
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
      tibble::tibble(
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


# ------------------------------------------------------------------------------
# parsing tree models

parse_tree_file <- function(x, trials = x$trials["Actual"]) {
  tree_raw <- x$tree %>%
    stringr::str_split("\n") %>%
    purrr::pluck(1)

  tree <- tree_raw[seq(3, length(tree_raw) - 1)]
  tree <- stringr::str_subset(tree, "freq=")

  res <- parse_tree(tree, lvls = x$levels)
  res[res$tree <= trials, ]
}

parse_tree <- function(tree, lvls) {
  trees <- list()

  index <- 1
  repeat {
    rules <- get_rule_index(index, tree)

    tree_tbl <- dplyr::tibble(
      node = seq_along(rules),
      rule = purrr::map_chr(rules, parse_rule),
      freqs = lapply(rules, get_freqs, tree = tree, lvls = lvls)
    )

    trees <- c(trees, list(tree_tbl))

    if (max(unlist(rules)) == length(tree)) break
    index <- max(unlist(rules)) + 1
  }

  purrr::map_dfr(trees, identity, .id = "tree")
}

get_rule_index <- function(index, tree, history = c()) {
  res <- list()
  history <- c(history, index)
  curr <- tree[index]

  if (stringr::str_detect(curr, "cut=")) {
    att <- stringr::str_remove(stringr::str_remove(curr, ".*att=\""), "\".*")
    cut <- stringr::str_remove(stringr::str_remove(curr, ".*cut=\""), "\".*")
    rule1_name <- paste(att, "<=", cut)
    rule1_index <- stats::setNames(index + 1, rule1_name)

    rule1 <- get_rule_index(rule1_index, tree, history)

    rule2_name <- paste(att, ">", cut)
    rule2_index <- stats::setNames(max(unlist(rule1)) + 1, rule2_name)

    rule2 <- get_rule_index(rule2_index, tree, history)
    res <- c(res, rule1, rule2)
  } else if (stringr::str_detect(curr, "elts=")) {
    att <- stringr::str_remove(stringr::str_remove(curr, ".*att=\""), "\".*")
    new_rules <- list()
    elts <- strsplit(curr, " elts=")[[1]][-1]
    for (i in seq_along(elts)) {
      value <- paste0("c(", elts[i], ")")
      rule_name <- paste(att, "%in%", value)
      rule_index <- stats::setNames(max(c(index, unlist(new_rules))) + 1, rule_name)

      new_rule <- get_rule_index(
        index = rule_index,
        tree = tree,
        history = history
      )
      new_rules <- c(new_rules, new_rule)
    }
    res <- c(res, new_rules)
  } else {
    return(list(history))
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

  freqs <- stringr::str_remove(tree[last], ".*freq=")
  freqs <- stringr::str_extract_all(freqs, "[0-9]+")[[1]]
  freqs <- as.integer(freqs)
  names(freqs) <- lvls

  freqs
}

