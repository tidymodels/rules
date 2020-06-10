context("rule_fit numeric outcomes")

source(file.path(test_path(), "test-helpers.R"))

vals <- c(0.01, .1, 1)

# ------------------------------------------------------------------------------

test_that('formula method', {
  skip_on_cran()

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      ridership ~ .,
      data = chi_mod,
      family = "gaussian",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1),
      verbose = 0
    )
  rf_pred_exp <- predict(rf_fit_exp, chi_pred, lambda = 1)[,1]

  expect_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 3, penalty = 1) %>%
      set_engine("xrf") %>%
      set_mode("regression"),
    NA
  )

  set.seed(4526)
  expect_error(
    rf_fit <- fit(rf_mod, ridership ~ ., data = chi_mod),
    NA
  )
  rf_pred <- predict(rf_fit, chi_pred)

  expect_equal(rf_fit_exp$xgb$evaluation_log, rf_fit$fit$xgb$evaluation_log)
  expect_equal(names(rf_pred), ".pred")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred, unname(rf_pred_exp))

  expect_error(
    rf_m_pred <- multi_predict(rf_fit, chi_pred, penalty = vals),
    NA
  )
  rf_m_pred <-
    rf_m_pred %>%
    mutate(.row_number = 1:nrow(rf_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in vals) {
    exp_pred <- predict(rf_fit_exp, chi_pred, lambda = i)[,1]
    obs_pred <- rf_m_pred %>% dplyr::filter(penalty == i) %>% pull(.pred)
    expect_equal(unname(exp_pred), obs_pred)
  }
})

# ------------------------------------------------------------------------------

test_that('non-formula method', {
  skip_on_cran()

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      ridership ~ .,
      data = chi_mod,
      family = "gaussian",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1),
      verbose = 0
    )
  rf_pred_exp <- predict(rf_fit_exp, chi_pred, lambda = 1)[,1]

  expect_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 3, penalty = 1) %>%
      set_engine("xrf") %>%
      set_mode("regression"),
    NA
  )

  expect_error(
    rf_fit <- fit_xy(rf_mod, x = chi_mod[, -1], y = chi_mod$ridership),
    NA
  )
  rf_pred <- predict(rf_fit, chi_pred)

  expect_equal(rf_fit_exp$xgb$evaluation_log, rf_fit$fit$xgb$evaluation_log)
  expect_equal(rf_fit_exp$glm$model$nzero, rf_fit$fit$glm$model$nzero)
  expect_equal(names(rf_pred), ".pred")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred, unname(rf_pred_exp))

  expect_error(
    rf_m_pred <- multi_predict(rf_fit, chi_pred, penalty = vals),
    NA
  )
  rf_m_pred <-
    rf_m_pred %>%
    mutate(.row_number = 1:nrow(rf_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in vals) {
    exp_pred <- predict(rf_fit_exp, chi_pred, lambda = i)[,1]
    obs_pred <- rf_m_pred %>% dplyr::filter(penalty == i) %>% pull(.pred)
    expect_equal(unname(exp_pred), obs_pred)
  }
})

