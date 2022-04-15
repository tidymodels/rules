test_that("formula method", {
  skip_on_cran()
  skip_if_not_installed("C50")

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit_exp <-
    C50::C5.0(
      x = ad_mod[, names(ad_mod) != "Class"],
      y = ad_mod$Class,
      trials = 10,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_pred)
  c5_prob_exp <- predict(c5_fit_exp, ad_pred, type = "prob")

  expect_error(
    c5_mod <-
      C5_rules(trees = 10) %>%
      set_engine("C5.0", seed = 2),
    NA
  )

  expect_error(
    c5_fit <- fit(c5_mod, Class ~ ., data = ad_mod),
    NA
  )
  c5_pred <- predict(c5_fit, ad_pred)
  c5_prob <- predict(c5_fit, ad_pred, type = "prob")

  expect_equal(c5_fit_exp$boostResults, c5_fit$fit$boostResults)
  expect_equal(names(c5_pred), ".pred_class")
  expect_true(tibble::is_tibble(c5_pred))
  expect_equal(c5_pred$.pred_class, c5_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("formula method - control", {
  skip_on_cran()
  skip_if_not_installed("C50")

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit_exp <-
    C50::C5.0(
      x = ad_mod[, names(ad_mod) != "Class"],
      y = ad_mod$Class,
      trials = 2,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2, subset = FALSE)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_pred)

  expect_error(
    c5_mod <-
      C5_rules(trees = 2) %>%
      set_engine("C5.0", control = ctrl),
    NA
  )

  expect_error(
    c5_fit <- fit(c5_mod, Class ~ ., data = ad_mod),
    NA
  )
  c5_pred <- predict(c5_fit, ad_pred)
  c5_prob <- predict(c5_fit, ad_pred, type = "prob")

  expect_equal(c5_fit_exp$boostResults, c5_fit$fit$boostResults)
  expect_equal(names(c5_pred), ".pred_class")
  expect_true(tibble::is_tibble(c5_pred))
  expect_equal(c5_pred$.pred_class, c5_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("non-formula method", {
  skip_on_cran()
  skip_if_not_installed("C50")

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit_exp <-
    C50::C5.0(
      x = ad_mod[, names(ad_mod) != "Class"],
      y = ad_mod$Class,
      trials = 10,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_pred)

  expect_error(
    c5_mod <-
      C5_rules(trees = 10) %>%
      set_engine("C5.0", seed = 2),
    NA
  )

  expect_error(
    c5_fit <- fit_xy(c5_mod, x = ad_mod[, names(ad_mod) != "Class"], y = ad_mod$Class),
    NA
  )
  c5_pred <- predict(c5_fit, ad_pred)
  c5_prob <- predict(c5_fit, ad_pred, type = "prob")

  expect_equal(c5_fit_exp$boostResults, c5_fit$fit$boostResults)
  expect_equal(names(c5_pred), ".pred_class")
  expect_true(tibble::is_tibble(c5_pred))
  expect_equal(c5_pred$.pred_class, c5_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("non-formula method - control", {
  skip_on_cran()
  skip_if_not_installed("C50")

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit_exp <-
    C50::C5.0(
      x = as.data.frame(ad_mod[, -1]),
      y = ad_mod$Class,
      trials = 2,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2, subset = FALSE)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_pred)

  expect_error(
    c5_mod <-
      C5_rules(trees = 2) %>%
      set_engine("C5.0", control = ctrl),
    NA
  )

  expect_error(
    c5_fit <- fit_xy(c5_mod, x = as.data.frame(ad_mod[, -1]), y = ad_mod$Class),
    NA
  )
  c5_pred <- predict(c5_fit, ad_pred)
  c5_prob <- predict(c5_fit, ad_pred, type = "prob")

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
  spec_1    <- C5_rules(trees = 1)
  spec_1_a  <- C5_rules(trees = 1, min_n = 100)
  spec_10   <- C5_rules(trees = 10)
  spec_10_a <- C5_rules(trees = 10, min_n = 100)

  expect_equal(update(spec_1,   tibble::tibble(trees = 10))$args$trees, 10)
  expect_equal(update(spec_1_a, tibble::tibble(trees = 10))$args$trees, 10)

  expect_equal(update(spec_1,   trees = 10), spec_10)
  expect_equal(update(spec_1_a, trees = 10), spec_10_a)
})


# ------------------------------------------------------------------------------

test_that("mulit-predict", {
  skip_on_cran()
  skip_if_not_installed("C50")

  ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

  c5_fit <-
    C5_rules(trees = 10) %>%
    set_engine("C5.0", seed = 2) %>%
    fit_xy(x = ad_mod_x[-(1:5), -1], y = ad_mod$Class[-(1:5)])

  c5_multi_pred <-
    multi_predict(c5_fit, ad_mod_x[1:5, -1], trees = 1:3) %>%
    mutate(.row_number = row_number()) %>%
    tidyr::unnest(cols = c(.pred))
  c5_multi_prob <-
    multi_predict(c5_fit, ad_mod_x[1:5, -1], type = "prob", trees = 1:3) %>%
    mutate(.row_number = row_number()) %>%
    tidyr::unnest(cols = c(.pred))

  expect_equivalent(
    predict(c5_fit$fit, ad_mod_x[1:5, -1], trees = 2, type = "class"),
    c5_multi_pred$.pred_class[c5_multi_pred$trees == 2]
  )
  expect_equivalent(
    predict(c5_fit$fit, ad_mod_x[1:5, -1], trees = 2, type = "prob")[, 1],
    c5_multi_prob$.pred_Impaired[c5_multi_prob$trees == 2]
  )
})


test_that("tunable", {
  C5_rules_C5.0 <-
    C5_rules(trees = tune(), min_n = tune()) %>%
    set_engine("C5.0") %>%
    tunable()

  expect_equal(
    C5_rules_C5.0$call_info[C5_rules_C5.0$name == "trees"][[1]]$range,
    c(1L, 100L)
  )

  C5_rules_engine_args <-
    C5_rules() %>%
    set_engine("C5.0", fuzzyThreshold = tune()) %>%
    tunable()

  expect_equal(
    C5_rules_engine_args$call_info[C5_rules_engine_args$name == "fuzzyThreshold"][[1]]$fun,
    "fuzzy_thresholding"
  )
})

test_that("mode specific package dependencies", {
  expect_identical(
    get_from_env(paste0("C5_rules", "_pkgs")) %>%
      dplyr::filter(engine == "C5.0", mode == "classification") %>%
      dplyr::pull(pkg),
    list(c("C50", "rules"))
  )

  expect_identical(
    get_from_env(paste0("C5_rules", "_pkgs")) %>%
      dplyr::filter(engine == "C5.0", mode == "regression") %>%
      dplyr::pull(pkg),
    list()
  )
})
