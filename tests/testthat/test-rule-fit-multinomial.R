test_that("formula method", {
  skip_on_cran()
  skip_if_not_installed("xrf")
  skip_if_not_installed("modeldata")

  hpc_data <- make_hpc_data()

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      class ~ .,
      data = hpc_data$hpc_mod,
      family = "multinomial",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1, num_class = 4),
      verbose = 0
    )
  rf_prob_exp <- predict(rf_fit_exp, hpc_data$hpc_pred, lambda = 1)[, , 1]
  rf_pred_exp <- factor(hpc_data$lvls[apply(rf_prob_exp, 1, which.max)], levels = hpc_data$lvls)
  rf_pred_exp <- unname(rf_pred_exp)

  expect_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 3, penalty = 1) %>%
      set_engine("xrf") %>%
      set_mode("classification"),
    NA
  )

  set.seed(4526)
  expect_error(
    rf_fit <- fit(rf_mod, class ~ ., data = hpc_data$hpc_mod),
    NA
  )
  rf_pred <- predict(rf_fit, hpc_data$hpc_pred)
  rf_prob <- predict(rf_fit, hpc_data$hpc_pred, type = "prob")

  expect_equal(unname(rf_fit_exp$xgb$evaluation_log), unname(rf_fit$fit$xgb$evaluation_log))

  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred_class, unname(rf_pred_exp))

  expect_equal(names(rf_prob), paste0(".pred_", hpc_data$lvls))
  expect_true(tibble::is_tibble(rf_prob))
  for (i in 1:ncol(rf_prob)) {
    expect_equal(rf_prob[[i]], unname(rf_prob_exp[, i]))
  }

  expect_error(
    rf_m_pred <- multi_predict(rf_fit, hpc_data$hpc_pred, penalty = hpc_data$vals),
    NA
  )
  expect_error(
    rf_m_prob <- multi_predict(rf_fit, hpc_data$hpc_pred, penalty = hpc_data$vals, type = "prob"),
    NA
  )

  rf_m_pred <-
    rf_m_pred %>%
    mutate(.row_number = 1:nrow(rf_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in hpc_data$vals) {
    exp_prob <- predict(rf_fit_exp, hpc_data$hpc_pred, lambda = i)[, , 1]
    exp_pred <- factor(hpc_data$lvls[apply(exp_prob, 1, which.max)], levels = hpc_data$lvls)
    exp_pred <- unname(exp_pred)

    obs_pred <- rf_m_pred %>% dplyr::filter(penalty == i) %>% pull(.pred_class)
    expect_equal(unname(exp_pred), obs_pred)
  }

  rf_m_prob <-
    rf_m_prob %>%
    mutate(.row_number = 1:nrow(rf_m_prob)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in hpc_data$vals) {
    exp_pred <- predict(rf_fit_exp, hpc_data$hpc_pred, lambda = i, type = "response")[, , 1]
    obs_pred <- rf_m_prob %>% dplyr::filter(penalty == i)
    for (i in 1:ncol(rf_prob)) {
      expect_equal(obs_pred[[i]], unname(exp_pred[, i]))
    }
  }
})

# ------------------------------------------------------------------------------

test_that("non-formula method", {
  skip_on_cran()
  skip_if_not_installed("xrf")
  skip_if_not_installed("modeldata")

  hpc_data <- make_hpc_data()

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      class ~ .,
      data = hpc_data$hpc_mod,
      family = "multinomial",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1, num_class = 4),
      verbose = 0
    )
  rf_prob_exp <- predict(rf_fit_exp, hpc_data$hpc_pred, lambda = 1)[, , 1]
  rf_pred_exp <- factor(hpc_data$lvls[apply(rf_prob_exp, 1, which.max)], levels = hpc_data$lvls)
  rf_pred_exp <- unname(rf_pred_exp)

  expect_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 3, penalty = 1) %>%
      set_engine("xrf") %>%
      set_mode("classification"),
    NA
  )

  expect_error(
    rf_fit <- fit_xy(rf_mod, x = hpc_data$hpc_mod[, -1], y = hpc_data$hpc_mod$class),
    NA
  )
  rf_pred <- predict(rf_fit, hpc_data$hpc_pred)
  rf_prob <- predict(rf_fit, hpc_data$hpc_pred, type = "prob")

  expect_equal(unname(rf_fit_exp$xgb$evaluation_log), unname(rf_fit$fit$xgb$evaluation_log))

  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred_class, unname(rf_pred_exp))

  expect_equal(names(rf_prob), paste0(".pred_", hpc_data$lvls))
  expect_true(tibble::is_tibble(rf_prob))
  for (i in 1:ncol(rf_prob)) {
    expect_equal(rf_prob[[i]], unname(rf_prob_exp[, i]))
  }

  expect_error(
    rf_m_pred <- multi_predict(rf_fit, hpc_data$hpc_pred, penalty = hpc_data$vals),
    NA
  )
  expect_error(
    rf_m_prob <- multi_predict(rf_fit, hpc_data$hpc_pred, penalty = hpc_data$vals, type = "prob"),
    NA
  )

  rf_m_pred <-
    rf_m_pred %>%
    mutate(.row_number = 1:nrow(rf_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in hpc_data$vals) {
    exp_prob <- predict(rf_fit_exp, hpc_data$hpc_pred, lambda = i)[, , 1]
    exp_pred <- factor(hpc_data$lvls[apply(exp_prob, 1, which.max)], levels = hpc_data$lvls)
    exp_pred <- unname(exp_pred)

    obs_pred <- rf_m_pred %>% dplyr::filter(penalty == i) %>% pull(.pred_class)
    expect_equal(unname(exp_pred), obs_pred)
  }

  rf_m_prob <-
    rf_m_prob %>%
    mutate(.row_number = 1:nrow(rf_m_prob)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in hpc_data$vals) {
    exp_pred <- predict(rf_fit_exp, hpc_data$hpc_pred, lambda = i, type = "response")[, , 1]
    obs_pred <- rf_m_prob %>% dplyr::filter(penalty == i)
    for (i in 1:ncol(rf_prob)) {
      expect_equal(obs_pred[[i]], unname(exp_pred[, i]))
    }
  }
})

# ------------------------------------------------------------------------------

test_that("tidy method - multi-class", {
  skip_on_cran()
  skip_if_not_installed("xrf")
  skip_if_not_installed("modeldata")

  hpc_data <- make_hpc_data()

  library(xrf)

  xrf_cls_mod <-
    rule_fit(trees = 3, penalty = .001) %>%
    set_engine("xrf") %>%
    set_mode("classification")

  set.seed(1)
  xrf_cls_fit <-
    xrf_cls_mod %>%
    fit(class ~ ., data = hpc_data$hpc_mod)

  xrf_rule_res <- tidy(xrf_cls_fit, penalty = .001)
  expect_true(length(unique(xrf_rule_res$class)) == 4)
  expect_true(sum(xrf_rule_res$rule == "( TRUE )") == 4)
  expect_true(sum(xrf_rule_res$rule_id == "(Intercept)") == 4)

  xrf_col_res <- tidy(xrf_cls_fit, unit = "column", penalty = .001)
  expect_true(length(unique(xrf_col_res$class)) == 4)
  expect_true(sum(xrf_col_res$term == "(Intercept)") == 4)
  expect_true(sum(xrf_col_res$rule_id == "(Intercept)") == 4)
  expect_equal(
    sort(unique(xrf_col_res$term)),
    c("(Intercept)", "compounds", "input_fields", "protocol")
  )
})
