library(dplyr)

# ------------------------------------------------------------------------------

test_that("formula method", {
  skip_on_cran()
  skip_if_not_installed("xrf")

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
      rule_fit(trees = 3, min_n = 3, penalty = 1) %>%
      set_engine("xrf") %>%
      set_mode("regression"),
    NA
  )

  set.seed(4526)
  expect_error(
    rf_fit <- fit(rf_mod, ridership ~ ., data = chi_data$chi_mod),
    NA
  )
  rf_pred <- predict(rf_fit, chi_data$chi_pred)

  expect_equal(rf_fit_exp$xgb$evaluation_log, rf_fit$fit$xgb$evaluation_log)
  expect_equal(names(rf_pred), ".pred")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred, unname(rf_pred_exp))

  expect_error(
    rf_m_pred <- multi_predict(rf_fit, chi_data$chi_pred, penalty = chi_data$vals),
    NA
  )
  rf_m_pred <-
    rf_m_pred %>%
    mutate(.row_number = 1:nrow(rf_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in chi_data$vals) {
    exp_pred <- predict(rf_fit_exp, chi_data$chi_pred, lambda = i)[, 1]
    obs_pred <- rf_m_pred %>% dplyr::filter(penalty == i) %>% pull(.pred)
    expect_equal(unname(exp_pred), obs_pred)
  }
})

# ------------------------------------------------------------------------------

test_that("non-formula method", {
  skip_on_cran()
  skip_if_not_installed("xrf")

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
      rule_fit(trees = 3, min_n = 3, penalty = 1) %>%
      set_engine("xrf") %>%
      set_mode("regression"),
    NA
  )

  expect_error(
    rf_fit <- fit_xy(rf_mod, x = chi_data$chi_mod[, -1], y = chi_data$chi_mod$ridership),
    NA
  )
  rf_pred <- predict(rf_fit, chi_data$chi_pred)

  expect_equal(rf_fit_exp$xgb$evaluation_log, rf_fit$fit$xgb$evaluation_log)
  expect_equal(rf_fit_exp$glm$model$nzero, rf_fit$fit$glm$model$nzero)
  expect_equal(names(rf_pred), ".pred")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred, unname(rf_pred_exp))

  expect_error(
    rf_m_pred <- multi_predict(rf_fit, chi_data$chi_pred, penalty = chi_data$vals),
    NA
  )
  rf_m_pred <-
    rf_m_pred %>%
    mutate(.row_number = 1:nrow(rf_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in chi_data$vals) {
    exp_pred <- predict(rf_fit_exp, chi_data$chi_pred, lambda = i)[, 1]
    obs_pred <- rf_m_pred %>% dplyr::filter(penalty == i) %>% pull(.pred)
    expect_equal(unname(exp_pred), obs_pred)
  }
})

# ------------------------------------------------------------------------------

test_that("tidy method - regression", {
  skip_on_cran()
  skip_if_not_installed("xrf")

  ames_data <- make_ames_data()

  library(xrf)

  xrf_reg_mod <-
    rule_fit(trees = 3, penalty = .001) %>%
    set_engine("xrf") %>%
    set_mode("regression")

  set.seed(1)
  xrf_reg_fit <-
    xrf_reg_mod %>%
    fit(
      Sale_Price ~ Neighborhood + Longitude + Latitude +
        Gr_Liv_Area + Central_Air,
      data = ames_data$ames
    )

  xrf_rule_res <- tidy(xrf_reg_fit)
  raw_coef <- coef(xrf_reg_fit$fit, lambda = 0.001)
  raw_coef <- raw_coef[raw_coef[, 1] != 0, ]
  expect_true(nrow(raw_coef) == nrow(xrf_rule_res))
  expect_true(all(raw_coef$term %in% xrf_rule_res$rule_id))


  xrf_col_res <- tidy(xrf_reg_fit, unit = "column")
  expect_equal(
    sort(unique(xrf_col_res$term)),
    c(
      "(Intercept)", "Central_Air", "Gr_Liv_Area", "Latitude", "Longitude",
      "Neighborhood"
    )
  )
  expect_equal(
    sort(unique(raw_coef$term)),
    sort(unique(xrf_col_res$rule_id))
  )
})

test_that("rule_fit handles mtry vs mtry_prop gracefully", {
  skip_on_cran()
  skip_if_not_installed("xrf")

  ames_data <- make_ames_data()

  # supply no mtry
  expect_error_free({
    pars_fit_1 <-
      rule_fit(trees = 5) %>%
      set_engine("xrf") %>%
      set_mode("regression") %>%
      fit(
        Sale_Price ~ Neighborhood + Longitude + Latitude +
          Gr_Liv_Area + Central_Air,
        data = ames_data$ames
      )
  })

  expect_equal(
    extract_fit_engine(pars_fit_1)$xgb$params$colsample_bytree,
    1
  )

  # supply mtry = 1 (edge cases)
  expect_error_free({
    pars_fit_2 <-
      rule_fit(mtry = 1, trees = 5) %>%
      set_engine("xrf", counts = TRUE) %>%
      set_mode("regression") %>%
      fit(
        Sale_Price ~ Neighborhood + Longitude + Latitude +
          Gr_Liv_Area + Central_Air,
        data = ames_data$ames
      )
  })

  expect_equal(
    extract_fit_engine(pars_fit_2)$xgb$params$colsample_bytree,
    1 / 5
  )

  expect_error_free({
    pars_fit_3 <-
      rule_fit(mtry = 1, trees = 5) %>%
      set_engine("xrf", counts = FALSE) %>%
      set_mode("regression") %>%
      fit(
        Sale_Price ~ Neighborhood + Longitude + Latitude +
          Gr_Liv_Area + Central_Air,
        data = ames_data$ames
      )
  })

  expect_equal(
    extract_fit_engine(pars_fit_3)$xgb$params$colsample_bytree,
    1
  )

  # supply a count (with default counts = TRUE)
  expect_error_free({
    pars_fit_4 <-
      rule_fit(mtry = 3, trees = 5) %>%
      set_engine("xrf") %>%
      set_mode("regression") %>%
      fit(
        Sale_Price ~ Neighborhood + Longitude + Latitude +
            Gr_Liv_Area + Central_Air,
        data = ames_data$ames
      )
  })

  expect_equal(
    extract_fit_engine(pars_fit_4)$xgb$params$colsample_bytree,
    3 / 5
  )

  # supply a proportion when count expected
  expect_snapshot_error({
    pars_fit_5 <-
      rule_fit(mtry = .5, trees = 5) %>%
      set_engine("xrf") %>%
      set_mode("regression") %>%
      fit(
        Sale_Price ~ Neighborhood + Longitude + Latitude +
          Gr_Liv_Area + Central_Air,
        data = ames_data$ames
      )
  })

  # supply a count when proportion expected
  expect_snapshot_error({
    pars_fit_6 <-
      rule_fit(mtry = 3, trees = 5) %>%
      set_engine("xrf", counts = FALSE) %>%
      set_mode("regression") %>%
      fit(
        Sale_Price ~ Neighborhood + Longitude + Latitude +
          Gr_Liv_Area + Central_Air,
        data = ames_data$ames
      )
  })

  expect_warning({
    pars_fit_7 <-
      rule_fit(trees = 5) %>%
      set_engine("xrf", colsample_bytree = .5) %>%
      set_mode("regression") %>%
      fit(
        Sale_Price ~ Neighborhood + Longitude + Latitude +
          Gr_Liv_Area + Central_Air,
        data = ames_data$ames
      )},
    "manually modified and were removed: colsample_bytree."
  )

  expect_equal(
    extract_fit_engine(pars_fit_7)$xgb$params$colsample_bytree,
    1
  )

  # supply both feature fraction and mtry
  expect_snapshot({
    pars_fit_8 <-
      rule_fit(mtry = .5, trees = 5) %>%
      set_engine("xrf", colsample_bytree = .5) %>%
      set_mode("regression") %>%
      fit(
        Sale_Price ~ Neighborhood + Longitude + Latitude +
          Gr_Liv_Area + Central_Air,
        data = ames_data$ames
      )},
    error = TRUE
  )

  expect_warning({
    pars_fit_9 <-
      rule_fit(mtry = 2, trees = 5) %>%
      set_engine("xrf", colsample_bytree = .5) %>%
      set_mode("regression") %>%
      fit(
        Sale_Price ~ Neighborhood + Longitude + Latitude +
          Gr_Liv_Area + Central_Air,
        data = ames_data$ames
      )},
    "manually modified and were removed: colsample_bytree."
  )

  expect_equal(
    extract_fit_engine(pars_fit_9)$xgb$params$colsample_bytree,
    2 / 5
  )
})
