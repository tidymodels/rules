context("C5 fits")

source(file.path(test_path(), "test-helpers.R"))

ctrl <- C50::C5.0Control(subset = FALSE, seed = 2)

# ------------------------------------------------------------------------------

test_that('formula method', {

  c5_fit_exp <-
    C50::C5.0(
      x = ad_mod_x,
      y = ad_mod$Class,
      trials = 10,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_pred_x)
  c5_prob_exp <- predict(c5_fit_exp, ad_pred_x, type = "prob")

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

test_that('formula method - control', {

  c5_fit_exp <-
    C50::C5.0(
      x = ad_mod_x,
      y = ad_mod$Class,
      trials = 2,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2, subset = FALSE)
    )
  c5_pred_exp <- predict(c5_fit_exp, ad_pred_x)

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

test_that('non-formula method', {

  c5_fit_exp <-
    C50::C5.0(
      x = as.data.frame(ad_mod[, -1]),
      y = ad_mod$Class,
      trials = 10,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2)
    )
  c5_pred_exp <- predict(c5_fit_exp, as.data.frame(ad_pred))

  expect_error(
    c5_mod <-
      C5_rules(trees = 10) %>%
      set_engine("C5.0", seed = 2),
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

test_that('non-formula method - control', {

  c5_fit_exp <-
    C50::C5.0(
      x = as.data.frame(ad_mod[, -1]),
      y = ad_mod$Class,
      trials = 2,
      rules = TRUE,
      control = C50::C5.0Control(seed = 2, subset = FALSE)
    )
  c5_pred_exp <- predict(c5_fit_exp, as.data.frame(ad_pred))

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
