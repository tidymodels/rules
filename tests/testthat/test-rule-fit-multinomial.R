context("rule_fit multinomial outcomes")

source(file.path(test_path(), "test-helpers.R"))

vals <- c(0.01, .1, 1)
lvls <- levels(hpc_mod$class)

# ------------------------------------------------------------------------------

test_that('formula method', {

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      class ~ .,
      data = hpc_mod,
      family = "multinomial",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1, num_class = 4),
      verbose = 0
    )
  rf_prob_exp <- predict(rf_fit_exp, hpc_pred, lambda = 1)[,,1]
  rf_pred_exp <- factor(lvls[apply(rf_prob_exp, 1, which.max)], levels = lvls)
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
    rf_fit <- fit(rf_mod, class ~ ., data = hpc_mod),
    NA
  )
  rf_pred <- predict(rf_fit, hpc_pred)
  rf_prob <- predict(rf_fit, hpc_pred, type = "prob")

  expect_equal(rf_fit_exp$xgb$evaluation_log, rf_fit$fit$xgb$evaluation_log)

  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred_class, unname(rf_pred_exp))

  expect_equal(names(rf_prob), paste0(".pred_", lvls))
  expect_true(tibble::is_tibble(rf_prob))
  for (i in 1:ncol(rf_prob)) {
    expect_equal(rf_prob[[i]], unname(rf_prob_exp[,i]))
  }

  expect_error(
    rf_m_pred <- multi_predict(rf_fit, hpc_pred, penalty = vals),
    NA
  )
  expect_error(
    rf_m_prob <- multi_predict(rf_fit, hpc_pred, penalty = vals, type = "prob"),
    NA
  )

  rf_m_pred <-
    rf_m_pred %>%
    mutate(.rows = 1:nrow(rf_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .rows)

  for (i in vals) {
    exp_prob <- predict(rf_fit_exp, hpc_pred, lambda = i)[,,1]
    exp_pred <- factor(lvls[apply(exp_prob, 1, which.max)], levels = lvls)
    exp_pred <- unname(exp_pred)

    obs_pred <- rf_m_pred %>% dplyr::filter(penalty == i) %>% pull(.pred_class)
    expect_equal(unname(exp_pred), obs_pred)
  }

  rf_m_prob <-
    rf_m_prob %>%
    mutate(.rows = 1:nrow(rf_m_prob)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .rows)

  for (i in vals) {
    exp_pred <- predict(rf_fit_exp, hpc_pred, lambda = i, type = "response")[,,1]
    obs_pred <- rf_m_prob %>% dplyr::filter(penalty == i)
    for (i in 1:ncol(rf_prob)) {
      expect_equal(obs_pred[[i]], unname(exp_pred[,i]))
    }
  }

})

# ------------------------------------------------------------------------------

test_that('non-formula method', {

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      class ~ .,
      data = hpc_mod,
      family = "multinomial",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1, num_class = 4),
      verbose = 0
    )
  rf_prob_exp <- predict(rf_fit_exp, hpc_pred, lambda = 1)[,,1]
  rf_pred_exp <- factor(lvls[apply(rf_prob_exp, 1, which.max)], levels = lvls)
  rf_pred_exp <- unname(rf_pred_exp)

  expect_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 3, penalty = 1) %>%
      set_engine("xrf") %>%
      set_mode("classification"),
    NA
  )

  expect_error(
    rf_fit <- fit_xy(rf_mod, x = hpc_mod[, -1], y = hpc_mod$class),
    NA
  )
  rf_pred <- predict(rf_fit, hpc_pred)
  rf_prob <- predict(rf_fit, hpc_pred, type = "prob")

  expect_equal(rf_fit_exp$xgb$evaluation_log, rf_fit$fit$xgb$evaluation_log)

  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred_class, unname(rf_pred_exp))

  expect_equal(names(rf_prob), paste0(".pred_", lvls))
  expect_true(tibble::is_tibble(rf_prob))
  for (i in 1:ncol(rf_prob)) {
    expect_equal(rf_prob[[i]], unname(rf_prob_exp[,i]))
  }

  expect_error(
    rf_m_pred <- multi_predict(rf_fit, hpc_pred, penalty = vals),
    NA
  )
  expect_error(
    rf_m_prob <- multi_predict(rf_fit, hpc_pred, penalty = vals, type = "prob"),
    NA
  )

  rf_m_pred <-
    rf_m_pred %>%
    mutate(.rows = 1:nrow(rf_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .rows)

  for (i in vals) {
    exp_prob <- predict(rf_fit_exp, hpc_pred, lambda = i)[,,1]
    exp_pred <- factor(lvls[apply(exp_prob, 1, which.max)], levels = lvls)
    exp_pred <- unname(exp_pred)

    obs_pred <- rf_m_pred %>% dplyr::filter(penalty == i) %>% pull(.pred_class)
    expect_equal(unname(exp_pred), obs_pred)
  }

  rf_m_prob <-
    rf_m_prob %>%
    mutate(.rows = 1:nrow(rf_m_prob)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(penalty, .rows)

  for (i in vals) {
    exp_pred <- predict(rf_fit_exp, hpc_pred, lambda = i, type = "response")[,,1]
    obs_pred <- rf_m_prob %>% dplyr::filter(penalty == i)
    for (i in 1:ncol(rf_prob)) {
      expect_equal(obs_pred[[i]], unname(exp_pred[,i]))
    }
  }

})
