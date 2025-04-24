test_that("formula method", {
  skip_on_cran()
  skip_if_not_installed("xrf")
  skip_if_not_installed("modeldata")

  chi_data <- make_chi_data()

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      ridership ~ .,
      data = chi_data$chi_mod,
      family = "gaussian",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1),
      verbose = 0
    )
  rf_pred_exp <- predict(rf_fit_exp, chi_data$chi_pred, lambda = 1)[, 1]

  expect_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 3, penalty = 1) |>
      set_engine("xrf") |>
      set_mode("regression"),
    NA
  )

  set.seed(4526)
  expect_error(
    rf_fit <- fit(rf_mod, ridership ~ ., data = chi_data$chi_mod),
    NA
  )
  rf_pred <- predict(rf_fit, chi_data$chi_pred)

  expect_equal(
    unname(rf_fit_exp$xgb$evaluation_log),
    unname(rf_fit_exp$xgb$evaluation_log)
  )
  expect_equal(names(rf_pred), ".pred")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred, unname(rf_pred_exp))

  expect_error(
    rf_m_pred <- multi_predict(
      rf_fit,
      chi_data$chi_pred,
      penalty = chi_data$vals
    ),
    NA
  )
  rf_m_pred <-
    rf_m_pred |>
    mutate(.row_number = 1:nrow(rf_m_pred)) |>
    tidyr::unnest(cols = c(.pred)) |>
    arrange(penalty, .row_number)

  for (i in chi_data$vals) {
    exp_pred <- predict(rf_fit_exp, chi_data$chi_pred, lambda = i)[, 1]
    obs_pred <- rf_m_pred |> dplyr::filter(penalty == i) |> pull(.pred)
    expect_equal(unname(exp_pred), obs_pred)
  }
})

# ------------------------------------------------------------------------------

test_that("non-formula method", {
  skip_on_cran()
  skip_if_not_installed("xrf")
  skip_if_not_installed("modeldata")

  chi_data <- make_chi_data()

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      ridership ~ .,
      data = chi_data$chi_mod,
      family = "gaussian",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1),
      verbose = 0
    )
  rf_pred_exp <- predict(rf_fit_exp, chi_data$chi_pred, lambda = 1)[, 1]

  expect_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 3, penalty = 1) |>
      set_engine("xrf") |>
      set_mode("regression"),
    NA
  )

  expect_error(
    rf_fit <- fit_xy(
      rf_mod,
      x = chi_data$chi_mod[, -1],
      y = chi_data$chi_mod$ridership
    ),
    NA
  )
  rf_pred <- predict(rf_fit, chi_data$chi_pred)

  expect_equal(
    unname(rf_fit_exp$xgb$evaluation_log),
    unname(rf_fit$fit$xgb$evaluation_log)
  )
  expect_equal(rf_fit_exp$glm$model$nzero, rf_fit$fit$glm$model$nzero)
  expect_equal(names(rf_pred), ".pred")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred, unname(rf_pred_exp))

  expect_error(
    rf_m_pred <- multi_predict(
      rf_fit,
      chi_data$chi_pred,
      penalty = chi_data$vals
    ),
    NA
  )
  rf_m_pred <-
    rf_m_pred |>
    mutate(.row_number = 1:nrow(rf_m_pred)) |>
    tidyr::unnest(cols = c(.pred)) |>
    arrange(penalty, .row_number)

  for (i in chi_data$vals) {
    exp_pred <- predict(rf_fit_exp, chi_data$chi_pred, lambda = i)[, 1]
    obs_pred <- rf_m_pred |> dplyr::filter(penalty == i) |> pull(.pred)
    expect_equal(unname(exp_pred), obs_pred)
  }
})

# ------------------------------------------------------------------------------

test_that("tidy method - regression", {
  skip_on_cran()
  skip_if_not_installed("xrf")
  skip_if_not_installed("modeldata")

  ames_data <- make_ames_data()

  library(xrf)

  xrf_reg_mod <-
    rule_fit(trees = 3, penalty = .001) |>
    set_engine("xrf") |>
    set_mode("regression")

  set.seed(1)
  xrf_reg_fit <-
    xrf_reg_mod |>
    fit(
      Sale_Price ~
        Neighborhood + Longitude + Latitude + Gr_Liv_Area + Central_Air,
      data = ames_data$ames
    )

  xrf_rule_res <- tidy(xrf_reg_fit, penalty = .001)
  raw_coef <- coef(xrf_reg_fit$fit, lambda = 0.001)
  raw_coef <- raw_coef[raw_coef[, 1] != 0, ]
  expect_true(nrow(raw_coef) == nrow(xrf_rule_res))
  expect_true(all(raw_coef$term %in% xrf_rule_res$rule_id))

  xrf_col_res <- tidy(xrf_reg_fit, unit = "column", penalty = .001)
  expect_equal(
    sort(unique(xrf_col_res$term)),
    c(
      "(Intercept)",
      "Central_Air",
      "Gr_Liv_Area",
      "Latitude",
      "Longitude",
      "Neighborhood"
    )
  )
  expect_equal(
    sort(unique(raw_coef$term)),
    sort(unique(xrf_col_res$rule_id))
  )
})

test_that("early stopping works in xrf_fit", {
  skip_on_cran()
  skip_if_not_installed("xrf")

  rf_mod_1 <-
    rule_fit(trees = 5) |>
    set_engine("xrf") |>
    set_mode("regression")

  rf_mod_2 <-
    rule_fit(trees = 5, stop_iter = 3) |>
    set_engine("xrf") |>
    set_mode("regression")

  rf_mod_3 <-
    rule_fit(trees = 5, stop_iter = 5) |>
    set_engine("xrf") |>
    set_mode("regression")

  expect_error_free(
    rf_fit_1 <- fit(rf_mod_1, mpg ~ ., data = mtcars)
  )

  expect_error_free(
    rf_fit_2 <- fit(rf_mod_2, mpg ~ ., data = mtcars)
  )

  expect_snapshot(
    suppressMessages(
      rf_fit_3 <- fit(rf_mod_3, mpg ~ ., data = mtcars)
    )
  )

  expect_true(is.null(rf_fit_1$fit$xgb$best_iteration))
  expect_true(!is.null(rf_fit_2$fit$xgb$best_iteration))
  expect_true(!is.null(rf_fit_3$fit$xgb$best_iteration))
})

test_that("xrf_fit is sensitive to glm_control", {
  skip_on_cran()
  skip_if_not_installed("xrf")

  rf_mod <-
    rule_fit(trees = 3) |>
    set_engine(
      "xrf",
      glm_control = list(type.measure = "deviance", nfolds = 8)
    ) |>
    set_mode("regression")

  expect_error_free(
    rf_fit_1 <- fit(rf_mod, mpg ~ ., data = mtcars)
  )

  rf_fit_1_call_args <- rlang::call_args(rf_fit_1$fit$glm$model$call)

  expect_equal(rf_fit_1_call_args$nfolds, 8)
  expect_equal(rf_fit_1_call_args$type.measure, "deviance")
})

test_that("xrf_fit guards xgb_control", {
  skip_on_cran()
  skip_if_not_installed("xrf")

  rf_mod <-
    rule_fit(trees = 3) |>
    set_engine("xrf", xgb_control = list(nrounds = 3)) |>
    set_mode("regression")

  expect_snapshot(
    suppressMessages(
      fit(rf_mod, mpg ~ ., data = mtcars)
    )
  )
})
