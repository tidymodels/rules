test_that("formula method - qrf regression", {
  skip_on_cran()
  skip_if_not_installed("qrf")
  skip_if_not_installed("modeldata")

  chi_data <- make_chi_data()

  set.seed(4526)
  rf_fit_exp <- qrf::qrf(
    ridership ~ .,
    data = chi_data$chi_mod,
    iterations = 3,
    penalty = 1
  )
  rf_pred_exp <- predict(rf_fit_exp, chi_data$chi_pred)

  expect_no_error(
    rf_mod <-
      rule_fit(trees = 3, penalty = 1) |>
      set_engine("qrf") |>
      set_mode("regression")
  )

  set.seed(4526)

  expect_no_error(
    rf_fit <- fit(rf_mod, ridership ~ ., data = chi_data$chi_mod)
  )
  rf_pred <- predict(rf_fit, chi_data$chi_pred)

  expect_equal(names(rf_pred), ".pred")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred, rf_pred_exp$.pred)
})

# ------------------------------------------------------------------------------

test_that("non-formula method - qrf regression", {
  skip_on_cran()
  skip_if_not_installed("qrf")
  skip_if_not_installed("modeldata")

  chi_data <- make_chi_data()

  set.seed(4526)
  rf_fit_exp <- qrf::qrf(
    x = chi_data$chi_mod[, -1],
    y = chi_data$chi_mod$ridership,
    iterations = 3,
    penalty = 1
  )
  rf_pred_exp <- predict(rf_fit_exp, chi_data$chi_pred)

  expect_no_error(
    rf_mod <-
      rule_fit(trees = 3, penalty = 1) |>
      set_engine("qrf") |>
      set_mode("regression")
  )

  set.seed(4526)
  expect_no_error(
    rf_fit <- fit_xy(
      rf_mod,
      x = chi_data$chi_mod[, -1],
      y = chi_data$chi_mod$ridership
    )
  )
  rf_pred <- predict(rf_fit, chi_data$chi_pred)

  expect_equal(names(rf_pred), ".pred")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred, rf_pred_exp$.pred)
})

# ------------------------------------------------------------------------------

test_that("case weights - qrf regression", {
  skip_on_cran()
  skip_if_not_installed("qrf")
  skip_if_not_installed("modeldata")

  chi_data <- make_chi_data()

  set.seed(1)
  wts <- runif(nrow(chi_data$chi_mod), min = 0.5, max = 2)
  wts_hw <- hardhat::importance_weights(wts)

  set.seed(4526)
  rf_fit_exp <- qrf::qrf(
    x = chi_data$chi_mod[, -1],
    y = chi_data$chi_mod$ridership,
    case_weights = wts,
    iterations = 3,
    penalty = 1
  )
  rf_pred_exp <- predict(rf_fit_exp, chi_data$chi_pred)

  rf_mod <-
    rule_fit(trees = 3, penalty = 1) |>
    set_engine("qrf") |>
    set_mode("regression")

  set.seed(4526)
  expect_no_error(
    rf_fit <- fit(
      rf_mod,
      ridership ~ .,
      data = chi_data$chi_mod,
      case_weights = wts_hw
    )
  )
  rf_pred <- predict(rf_fit, chi_data$chi_pred)

  expect_equal(rf_pred$.pred, rf_pred_exp$.pred)
})

# ------------------------------------------------------------------------------

test_that("mode specific package dependencies - qrf", {
  expect_identical(
    get_from_env(paste0("rule_fit", "_pkgs")) |>
      dplyr::filter(engine == "qrf", mode == "regression") |>
      dplyr::pull(pkg),
    list(c("qrf", "rules"))
  )

  expect_identical(
    get_from_env(paste0("rule_fit", "_pkgs")) |>
      dplyr::filter(engine == "qrf", mode == "classification") |>
      dplyr::pull(pkg),
    list(c("qrf", "rules"))
  )
})
