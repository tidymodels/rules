library(dplyr)

test_that("formula method", {
  skip_on_cran()
  skip_if_not_installed("C50")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit_exp <-
    C50::C5.0(
      x = ad_data$ad_mod[, names(ad_data$ad_mod) != "Class"],
      y = ad_data$ad_mod$Class,
      trials = 10,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_data$ad_pred)
  c5_prob_exp <- predict(c5_fit_exp, ad_data$ad_pred, type = "prob")

  expect_no_error(
    c5_mod <-
      C5_rules(trees = 10) |>
      set_engine("C5.0", seed = 2)
  )

  expect_no_error(
    c5_fit <- fit(c5_mod, Class ~ ., data = ad_data$ad_mod)
  )
  c5_pred <- predict(c5_fit, ad_data$ad_pred)
  c5_prob <- predict(c5_fit, ad_data$ad_pred, type = "prob")

  expect_equal(c5_fit_exp$boostResults, c5_fit$fit$boostResults)
  expect_equal(names(c5_pred), ".pred_class")
  expect_true(tibble::is_tibble(c5_pred))
  expect_equal(c5_pred$.pred_class, c5_pred_exp)
})

test_that("formula method - case weights", {
  skip_on_cran()
  skip_if_not_installed("C50")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  wts <- importance_weights(1:nrow(ad_data$ad_mod))

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit_exp <-
    C50::C5.0(
      x = ad_data$ad_mod[, names(ad_data$ad_mod) != "Class"],
      y = ad_data$ad_mod$Class,
      trials = 10,
      rules = TRUE,
      weights = as.double(wts),
      control = C50::C5.0Control(seed = 2)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_data$ad_pred)
  c5_prob_exp <- predict(c5_fit_exp, ad_data$ad_pred, type = "prob")

  expect_no_error(
    c5_mod <-
      C5_rules(trees = 10) |>
      set_engine("C5.0", seed = 2)
  )

  expect_no_error(
    c5_fit <- fit(c5_mod, Class ~ ., data = ad_data$ad_mod, case_weights = wts)
  )
  c5_pred <- predict(c5_fit, ad_data$ad_pred)
  c5_prob <- predict(c5_fit, ad_data$ad_pred, type = "prob")

  expect_equal(c5_fit_exp$boostResults, c5_fit$fit$boostResults)
  expect_equal(names(c5_pred), ".pred_class")
  expect_true(tibble::is_tibble(c5_pred))
  expect_equal(c5_pred$.pred_class, c5_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("formula method - control", {
  skip_on_cran()
  skip_if_not_installed("C50")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit_exp <-
    C50::C5.0(
      x = ad_data$ad_mod[, names(ad_data$ad_mod) != "Class"],
      y = ad_data$ad_mod$Class,
      trials = 2,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2, subset = FALSE)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_data$ad_pred)

  expect_no_error(
    c5_mod <-
      C5_rules(trees = 2) |>
      set_engine("C5.0", control = ctrl)
  )

  expect_no_error(
    c5_fit <- fit(c5_mod, Class ~ ., data = ad_data$ad_mod)
  )
  c5_pred <- predict(c5_fit, ad_data$ad_pred)
  c5_prob <- predict(c5_fit, ad_data$ad_pred, type = "prob")

  expect_equal(c5_fit_exp$boostResults, c5_fit$fit$boostResults)
  expect_equal(names(c5_pred), ".pred_class")
  expect_true(tibble::is_tibble(c5_pred))
  expect_equal(c5_pred$.pred_class, c5_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("non-formula method", {
  skip_on_cran()
  skip_if_not_installed("C50")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit_exp <-
    C50::C5.0(
      x = ad_data$ad_mod[, names(ad_data$ad_mod) != "Class"],
      y = ad_data$ad_mod$Class,
      trials = 10,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_data$ad_pred)

  expect_no_error(
    c5_mod <-
      C5_rules(trees = 10) |>
      set_engine("C5.0", seed = 2)
  )

  expect_no_error(
    c5_fit <- fit_xy(
      c5_mod,
      x = ad_data$ad_mod[, names(ad_data$ad_mod) != "Class"],
      y = ad_data$ad_mod$Class
    )
  )
  c5_pred <- predict(c5_fit, ad_data$ad_pred)
  c5_prob <- predict(c5_fit, ad_data$ad_pred, type = "prob")

  expect_equal(c5_fit_exp$boostResults, c5_fit$fit$boostResults)
  expect_equal(names(c5_pred), ".pred_class")
  expect_true(tibble::is_tibble(c5_pred))
  expect_equal(c5_pred$.pred_class, c5_pred_exp)
})

test_that("non-formula method - case weights", {
  skip_on_cran()
  skip_if_not_installed("C50")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  wts <- importance_weights(1:nrow(ad_data$ad_mod))

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit_exp <-
    C50::C5.0(
      x = ad_data$ad_mod[, names(ad_data$ad_mod) != "Class"],
      y = ad_data$ad_mod$Class,
      trials = 10,
      rules = TRUE,
      weights = as.double(wts),
      control = C50::C5.0Control(seed = 2)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_data$ad_pred)

  expect_no_error(
    c5_mod <-
      C5_rules(trees = 10) |>
      set_engine("C5.0", seed = 2)
  )

  expect_no_error(
    c5_fit <- fit_xy(
      c5_mod,
      x = ad_data$ad_mod[, names(ad_data$ad_mod) != "Class"],
      y = ad_data$ad_mod$Class,
      case_weights = wts
    )
  )
  c5_pred <- predict(c5_fit, ad_data$ad_pred)
  c5_prob <- predict(c5_fit, ad_data$ad_pred, type = "prob")

  expect_equal(c5_fit_exp$boostResults, c5_fit$fit$boostResults)
  expect_equal(names(c5_pred), ".pred_class")
  expect_true(tibble::is_tibble(c5_pred))
  expect_equal(c5_pred$.pred_class, c5_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("non-formula method - control", {
  skip_on_cran()
  skip_if_not_installed("C50")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit_exp <-
    C50::C5.0(
      x = as.data.frame(ad_data$ad_mod[, -1]),
      y = ad_data$ad_mod$Class,
      trials = 2,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2, subset = FALSE)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_data$ad_pred)

  expect_no_error(
    c5_mod <-
      C5_rules(trees = 2) |>
      set_engine("C5.0", control = ctrl)
  )

  expect_no_error(
    c5_fit <- fit_xy(
      c5_mod,
      x = as.data.frame(ad_data$ad_mod[, -1]),
      y = ad_data$ad_mod$Class
    )
  )
  c5_pred <- predict(c5_fit, ad_data$ad_pred)
  c5_prob <- predict(c5_fit, ad_data$ad_pred, type = "prob")

  expect_equal(c5_fit_exp$boostResults, c5_fit$fit$boostResults)
  expect_equal(names(c5_pred), ".pred_class")
  expect_true(tibble::is_tibble(c5_pred))
  expect_equal(c5_pred$.pred_class, c5_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("printing", {
  skip_on_cran()
  skip_if_not_installed("C50")

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  expect_output(print(C5_rules(trees = 1)))
})

# ------------------------------------------------------------------------------

test_that("updates", {
  spec_1 <- C5_rules(trees = 1)
  spec_1_a <- C5_rules(trees = 1, min_n = 100)
  spec_10 <- C5_rules(trees = 10)
  spec_10_a <- C5_rules(trees = 10, min_n = 100)

  expect_equal(update(spec_1, tibble(trees = 10))$args$trees, 10)
  expect_equal(update(spec_1_a, tibble(trees = 10))$args$trees, 10)

  expect_equal(update(spec_1, trees = 10), spec_10)
  expect_equal(update(spec_1_a, trees = 10), spec_10_a)
})


# ------------------------------------------------------------------------------

test_that("mulit-predict", {
  skip_on_cran()
  skip_if_not_installed("C50")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit <-
    C5_rules(trees = 10) |>
    set_engine("C5.0", seed = 2) |>
    fit_xy(x = ad_data$ad_mod_x[-(1:5), -1], y = ad_data$ad_mod$Class[-(1:5)])

  c5_multi_pred <-
    multi_predict(c5_fit, ad_data$ad_mod_x[1:5, -1], trees = 1:3) |>
    mutate(.row_number = row_number()) |>
    tidyr::unnest(cols = c(.pred))
  c5_multi_prob <-
    multi_predict(
      c5_fit,
      ad_data$ad_mod_x[1:5, -1],
      type = "prob",
      trees = 1:3
    ) |>
    mutate(.row_number = row_number()) |>
    tidyr::unnest(cols = c(.pred))

  expect_equal(
    predict(c5_fit$fit, ad_data$ad_mod_x[1:5, -1], trials = 2, type = "class"),
    c5_multi_pred$.pred_class[c5_multi_pred$trees == 2],
    ignore_attr = TRUE
  )
  expect_equal(
    predict(c5_fit$fit, ad_data$ad_mod_x[1:5, -1], trials = 2, type = "prob")[,
      1
    ],
    c5_multi_prob$.pred_Impaired[c5_multi_prob$trees == 2],
    ignore_attr = TRUE
  )
})


test_that("tunable", {
  C5_rules_C5.0 <-
    C5_rules(trees = tune(), min_n = tune()) |>
    set_engine("C5.0") |>
    tunable()

  expect_equal(
    C5_rules_C5.0$call_info[C5_rules_C5.0$name == "trees"][[1]]$range,
    c(1L, 100L)
  )

  expect_equal(
    C5_rules_C5.0$call_info[C5_rules_C5.0$name == "min_n"][[1]]$range,
    c(2L, 40L)
  )

  C5_rules_engine_args <-
    C5_rules() |>
    set_engine("C5.0", fuzzyThreshold = tune()) |>
    tunable()

  expect_equal(
    C5_rules_engine_args$call_info[
      C5_rules_engine_args$name == "fuzzyThreshold"
    ][[1]]$fun,
    "fuzzy_thresholding"
  )
})

test_that("mode specific package dependencies", {
  expect_identical(
    get_from_env(paste0("C5_rules", "_pkgs")) |>
      dplyr::filter(engine == "C5.0", mode == "classification") |>
      dplyr::pull(pkg),
    list(c("C50", "rules"))
  )

  expect_identical(
    get_from_env(paste0("C5_rules", "_pkgs")) |>
      dplyr::filter(engine == "C5.0", mode == "regression") |>
      dplyr::pull(pkg),
    list()
  )
})

# ------------------------------------------------------------------------------

test_that("tidy method", {
  skip_on_cran()
  skip_if_not_installed("C50")
  skip_if_not_installed("modeldata")
  library(C50)

  data(penguins, package = "modeldata")
  penguins <- penguins[complete.cases(penguins), ]

  # ------------------------------------------------------------------------------
  # rule based model

  rules_1 <- C5.0(sex ~ ., data = penguins, rules = TRUE)
  rules_2 <- C5.0(sex ~ ., data = penguins, rules = TRUE, trials = 5)

  tidy_1 <- tidy(rules_1)
  expect_equal(nrow(tidy_1), rules_1$size)
  expect_equal(max(tidy_1$trial), 1L)
  # Spot check a few lines
  expect_equal(
    tidy_1$rule[4],
    "( bill_length_mm < 39.599998 ) & ( bill_depth_mm < 17.9 )"
  )
  expect_equal(tidy_1$statistic[[2]]$lift, 1.94859)

  tidy_2 <- tidy(rules_2)
  expect_equal(nrow(tidy_2), sum(rules_2$size))
  expect_equal(max(tidy_2$trial), 5L)
  # Spot check a few more lines
  expect_equal(
    tidy_2$rule[37], # trial 4, rule 6
    "( island  %in% c( 'Biscoe','Dream' ) ) & ( bill_length_mm < 50.5 ) & ( bill_depth_mm > 20.5 )"
  )
  expect_equal(tidy_2$statistic[[50]]$lift, 1.67623) # trial 5, rule 4

  expect_equal(tidy_1, tidy_2[1:nrow(tidy_1), ])

  # ------------------------------------------------------------------------------
  # tree-based models

  trees_1 <- C5.0(sex ~ ., data = penguins)
  trees_2 <- C5.0(
    sex ~ .,
    data = penguins,
    trials = 2,
    control = C5.0Control(earlyStopping = FALSE)
  )

  tidy_1 <- tidy(trees_1)
  tidy_2 <- tidy(trees_2)

  print_1 <- capture.output(summary(trees_1))
  print_2 <- capture.output(summary(trees_2))

  term_nodes_1 <- sum(grepl("[0-9]\\)$", print_1))
  term_nodes_2 <- sum(grepl("[0-9]\\)$", print_2))

  expect_equal(nrow(tidy_1), term_nodes_1)
  expect_equal(nrow(tidy_2), term_nodes_2)
})

test_that('check_args() works', {
  skip_on_cran()
  skip_if_not_installed("C50")
  skip_if_not_installed("parsnip", "1.2.1.9001")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  expect_snapshot(
    error = TRUE,
    {
      spec <- C5_rules(trees = c(1, 2, 3)) |>
        set_engine("C5.0") |>
        set_mode("classification")
      fit(spec, Class ~ ., data = ad_data$ad_mod)
    }
  )

  expect_snapshot(
    {
      spec <- C5_rules(trees = 0) |>
        set_engine("C5.0") |>
        set_mode("classification")
      res <- fit(spec, Class ~ ., data = ad_data$ad_mod)
    }
  )

  expect_snapshot(
    {
      spec <- C5_rules(trees = 1000) |>
        set_engine("C5.0") |>
        set_mode("classification")
      res <- fit(spec, Class ~ ., data = ad_data$ad_mod)
    }
  )

  expect_snapshot(
    error = TRUE,
    {
      spec <- C5_rules(min_n = c(1, 2, 3)) |>
        set_engine("C5.0") |>
        set_mode("classification")
      fit(spec, Class ~ ., data = ad_data$ad_mod)
    }
  )
})
