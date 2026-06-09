test_that("formula method - qrf binary classification", {
  skip_on_cran()
  skip_if_not_installed("qrf")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  set.seed(4526)
  rf_fit_exp <- qrf::qrf(
    Class ~ .,
    data = ad_data$ad_mod,
    iterations = 3,
    penalty = 1
  )
  rf_pred_exp <- predict(rf_fit_exp, ad_data$ad_pred, type = "class")
  rf_prob_exp <- predict(rf_fit_exp, ad_data$ad_pred, type = "prob")

  expect_no_error(
    rf_mod <-
      rule_fit(trees = 3, penalty = 1) |>
      set_engine("qrf") |>
      set_mode("classification")
  )

  set.seed(4526)
  expect_no_error(
    rf_fit <- fit(rf_mod, Class ~ ., data = ad_data$ad_mod)
  )
  rf_pred <- predict(rf_fit, ad_data$ad_pred)
  rf_prob <- predict(rf_fit, ad_data$ad_pred, type = "prob")

  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred_class, rf_pred_exp$.pred_class)

  expect_equal(names(rf_prob), paste0(".pred_", ad_data$lvls))
  expect_true(tibble::is_tibble(rf_prob))
  expect_equal(rf_prob[[1]], rf_prob_exp[[1]])
})

# ------------------------------------------------------------------------------

test_that("non-formula method - qrf binary classification", {
  skip_on_cran()
  skip_if_not_installed("qrf")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  set.seed(4526)
  rf_fit_exp <- qrf::qrf(
    x = ad_data$ad_mod[, -1],
    y = ad_data$ad_mod$Class,
    iterations = 3,
    penalty = 1
  )
  rf_pred_exp <- predict(rf_fit_exp, ad_data$ad_pred, type = "class")
  rf_prob_exp <- predict(rf_fit_exp, ad_data$ad_pred, type = "prob")

  expect_no_error(
    rf_mod <-
      rule_fit(trees = 3, penalty = 1) |>
      set_engine("qrf") |>
      set_mode("classification")
  )

  set.seed(4526)
  expect_no_error(
    rf_fit <- fit_xy(
      rf_mod,
      x = ad_data$ad_mod[, -1],
      y = ad_data$ad_mod$Class
    )
  )
  rf_pred <- predict(rf_fit, ad_data$ad_pred)
  rf_prob <- predict(rf_fit, ad_data$ad_pred, type = "prob")

  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred_class, rf_pred_exp$.pred_class)

  expect_equal(names(rf_prob), paste0(".pred_", ad_data$lvls))
  expect_true(tibble::is_tibble(rf_prob))
  expect_equal(rf_prob[[1]], rf_prob_exp[[1]])
})

# ------------------------------------------------------------------------------

test_that("formula method - qrf multiclass classification", {
  skip_on_cran()
  skip_if_not_installed("qrf")
  skip_if_not_installed("modeldata")

  hpc_data <- make_hpc_data()

  set.seed(4526)
  rf_fit_exp <- qrf::qrf(
    class ~ .,
    data = hpc_data$hpc_mod,
    iterations = 3,
    penalty = 1
  )
  rf_pred_exp <- predict(rf_fit_exp, hpc_data$hpc_pred, type = "class")
  rf_prob_exp <- predict(rf_fit_exp, hpc_data$hpc_pred, type = "prob")

  expect_no_error(
    rf_mod <-
      rule_fit(trees = 3, penalty = 1) |>
      set_engine("qrf") |>
      set_mode("classification")
  )

  set.seed(4526)
  expect_no_error(
    rf_fit <- fit(rf_mod, class ~ ., data = hpc_data$hpc_mod)
  )
  rf_pred <- predict(rf_fit, hpc_data$hpc_pred)
  rf_prob <- predict(rf_fit, hpc_data$hpc_pred, type = "prob")

  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred_class, rf_pred_exp$.pred_class)

  expect_equal(names(rf_prob), paste0(".pred_", hpc_data$lvls))
  expect_true(tibble::is_tibble(rf_prob))
  expect_equal(rf_prob[[1]], rf_prob_exp[[1]])
})

# ------------------------------------------------------------------------------

test_that("engine args - qrf classification", {
  skip_on_cran()
  skip_if_not_installed("qrf")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  expect_no_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 5, sample_size = 0.8, penalty = 1) |>
      set_engine("qrf") |>
      set_mode("classification")
  )

  set.seed(4526)

  expect_no_error(
    rf_fit <- fit(rf_mod, Class ~ ., data = ad_data$ad_mod)
  )

  rf_pred <- predict(rf_fit, ad_data$ad_pred)
  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
})

# ------------------------------------------------------------------------------

test_that("case weights - qrf classification", {
  skip_on_cran()
  skip_if_not_installed("qrf")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  set.seed(1)
  wts <- runif(nrow(ad_data$ad_mod), min = 0.5, max = 2)
  wts_hw <- hardhat::importance_weights(wts)

  set.seed(4526)
  rf_fit_exp <- qrf::qrf(
    x = ad_data$ad_mod[, -1],
    y = ad_data$ad_mod$Class,
    case_weights = wts,
    iterations = 3,
    penalty = 1
  )
  rf_pred_exp <- predict(rf_fit_exp, ad_data$ad_pred, type = "class")

  rf_mod <-
    rule_fit(trees = 3, penalty = 1) |>
    set_engine("qrf") |>
    set_mode("classification")

  set.seed(4526)
  expect_no_error(
    rf_fit <- fit(
      rf_mod,
      Class ~ .,
      data = ad_data$ad_mod,
      case_weights = wts_hw
    )
  )
  rf_pred <- predict(rf_fit, ad_data$ad_pred)

  expect_equal(rf_pred$.pred_class, rf_pred_exp$.pred_class)
})
