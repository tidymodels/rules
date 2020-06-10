context("rule_fit binomial outcomes")

source(file.path(test_path(), "test-helpers.R"))

vals <- c(0.01, .1, 1)
lvls <- levels(ad_mod$Class)

# ------------------------------------------------------------------------------

test_that('formula method', {

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      Class ~ .,
      data = ad_mod,
      family = "binomial",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1),
      verbose = 0
    )
  rf_pred_exp <- predict(rf_fit_exp, ad_pred, lambda = 1)[,1]
  rf_pred_exp <- factor(ifelse(rf_pred_exp >= 0.5, lvls[2], lvls[1]), levels = lvls)
  rf_pred_exp <- unname(rf_pred_exp)
  rf_prob_exp <- predict(rf_fit_exp, ad_pred, lambda = 1, type = "response")[,1]

  expect_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 3, penalty = 1) %>%
      set_engine("xrf") %>%
      set_mode("classification"),
    NA
  )

  set.seed(4526)
  expect_error(
    rf_fit <- fit(rf_mod, Class ~ ., data = ad_mod),
    NA
  )
  rf_pred <- predict(rf_fit, ad_pred)
  rf_prob <- predict(rf_fit, ad_pred, type = "prob")

  expect_equal(rf_fit_exp$xgb$evaluation_log, rf_fit$fit$xgb$evaluation_log)

  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred_class, unname(rf_pred_exp))

  expect_equal(names(rf_prob), paste0(".pred_", lvls))
  expect_true(tibble::is_tibble(rf_prob))
  expect_equal(rf_prob$.pred_Control, unname(rf_prob_exp))

  expect_error(
    rf_m_pred <- multi_predict(rf_fit, ad_pred, penalty = vals),
    NA
  )
  expect_error(
    rf_m_prob <- multi_predict(rf_fit, ad_pred, penalty = vals, type = "prob"),
    NA
  )

  rf_m_pred <-
    rf_m_pred %>%
    mutate(.row_number = 1:nrow(rf_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in vals) {
    exp_pred <- predict(rf_fit_exp, ad_pred, lambda = i)[,1]
    exp_pred <- factor(ifelse(exp_pred >= 0.5, lvls[2], lvls[1]), levels = lvls)
    exp_pred <- unname(exp_pred)
    obs_pred <- rf_m_pred %>% dplyr::filter(penalty == i) %>% pull(.pred_class)
    expect_equal(unname(exp_pred), obs_pred)
  }

  rf_m_prob <-
    rf_m_prob %>%
    mutate(.row_number = 1:nrow(rf_m_prob)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in vals) {
    exp_pred <- predict(rf_fit_exp, ad_pred, lambda = i, type = "response")[,1]
    obs_pred <- rf_m_prob %>% dplyr::filter(penalty == i) %>% pull(.pred_Control)
    expect_equal(unname(exp_pred), obs_pred)
  }
})

# ------------------------------------------------------------------------------

test_that('non-formula method', {

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      Class ~ .,
      data = ad_mod,
      family = "binomial",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1),
      verbose = 0
    )
  rf_pred_exp <- predict(rf_fit_exp, ad_pred, lambda = 1)[,1]
  rf_pred_exp <- factor(ifelse(rf_pred_exp >= 0.5, lvls[2], lvls[1]), levels = lvls)
  rf_pred_exp <- unname(rf_pred_exp)
  rf_prob_exp <- predict(rf_fit_exp, ad_pred, lambda = 1, type = "response")[,1]

  expect_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 3, penalty = 1) %>%
      set_engine("xrf") %>%
      set_mode("classification"),
    NA
  )

  expect_error(
    rf_fit <- fit_xy(rf_mod, x = ad_mod[, -1], y = ad_mod$Class),
    NA
  )
  rf_pred <- predict(rf_fit, ad_pred)
  rf_prob <- predict(rf_fit, ad_pred, type = "prob")

  expect_equal(rf_fit_exp$xgb$evaluation_log, rf_fit$fit$xgb$evaluation_log)

  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred_class, unname(rf_pred_exp))

  expect_equal(names(rf_prob), paste0(".pred_", lvls))
  expect_true(tibble::is_tibble(rf_prob))
  expect_equal(rf_prob$.pred_Control, unname(rf_prob_exp))

  expect_error(
    rf_m_pred <- multi_predict(rf_fit, ad_pred, penalty = vals),
    NA
  )
  expect_error(
    rf_m_prob <- multi_predict(rf_fit, ad_pred, penalty = vals, type = "prob"),
    NA
  )

  rf_m_pred <-
    rf_m_pred %>%
    mutate(.row_number = 1:nrow(rf_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in vals) {
    exp_pred <- predict(rf_fit_exp, ad_pred, lambda = i)[,1]
    exp_pred <- factor(ifelse(exp_pred >= 0.5, lvls[2], lvls[1]), levels = lvls)
    exp_pred <- unname(exp_pred)
    obs_pred <- rf_m_pred %>% dplyr::filter(penalty == i) %>% pull(.pred_class)
    expect_equal(unname(exp_pred), obs_pred)
  }

  rf_m_prob <-
    rf_m_prob %>%
    mutate(.row_number = 1:nrow(rf_m_prob)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .row_number)

  for (i in vals) {
    exp_pred <- predict(rf_fit_exp, ad_pred, lambda = i, type = "response")[,1]
    obs_pred <- rf_m_prob %>% dplyr::filter(penalty == i) %>% pull(.pred_Control)
    expect_equal(unname(exp_pred), obs_pred)
  }

})
