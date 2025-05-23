test_that("formula method", {
  skip_on_cran()
  skip_if_not_installed("xrf")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      Class ~ .,
      data = ad_data$ad_mod,
      family = "binomial",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1),
      verbose = 0
    )
  rf_pred_exp <- predict(rf_fit_exp, ad_data$ad_pred, lambda = 1)[, 1]
  rf_pred_exp <- factor(
    ifelse(rf_pred_exp >= 0.5, ad_data$lvls[2], ad_data$lvls[1]),
    levels = ad_data$lvls
  )
  rf_pred_exp <- unname(rf_pred_exp)
  rf_prob_exp <- predict(
    rf_fit_exp,
    ad_data$ad_pred,
    lambda = 1,
    type = "response"
  )[, 1]

  expect_no_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 3, penalty = 1) |>
      set_engine("xrf") |>
      set_mode("classification")
  )

  set.seed(4526)
  expect_no_error(
    rf_fit <- fit(rf_mod, Class ~ ., data = ad_data$ad_mod)
  )
  rf_pred <- predict(rf_fit, ad_data$ad_pred)
  rf_prob <- predict(rf_fit, ad_data$ad_pred, type = "prob")

  expect_equal(
    unname(rf_fit_exp$xgb$evaluation_log),
    unname(rf_fit$fit$xgb$evaluation_log)
  )

  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred_class, unname(rf_pred_exp))

  expect_equal(names(rf_prob), paste0(".pred_", ad_data$lvls))
  expect_true(tibble::is_tibble(rf_prob))
  expect_equal(rf_prob$.pred_Control, unname(rf_prob_exp))

  expect_no_error(
    rf_m_pred <- multi_predict(rf_fit, ad_data$ad_pred, penalty = ad_data$vals)
  )
  expect_no_error(
    rf_m_prob <- multi_predict(
      rf_fit,
      ad_data$ad_pred,
      penalty = ad_data$vals,
      type = "prob"
    )
  )

  rf_m_pred <-
    rf_m_pred |>
    mutate(.row_number = 1:nrow(rf_m_pred)) |>
    tidyr::unnest(cols = c(.pred)) |>
    arrange(penalty, .row_number)

  for (i in ad_data$vals) {
    exp_pred <- predict(rf_fit_exp, ad_data$ad_pred, lambda = i)[, 1]
    exp_pred <- factor(
      ifelse(exp_pred >= 0.5, ad_data$lvls[2], ad_data$lvls[1]),
      levels = ad_data$lvls
    )
    exp_pred <- unname(exp_pred)
    obs_pred <- rf_m_pred |> dplyr::filter(penalty == i) |> pull(.pred_class)
    expect_equal(unname(exp_pred), obs_pred)
  }

  rf_m_prob <-
    rf_m_prob |>
    mutate(.row_number = 1:nrow(rf_m_prob)) |>
    tidyr::unnest(cols = c(.pred)) |>
    arrange(penalty, .row_number)

  for (i in ad_data$vals) {
    exp_pred <- predict(
      rf_fit_exp,
      ad_data$ad_pred,
      lambda = i,
      type = "response"
    )[, 1]
    obs_pred <- rf_m_prob |>
      dplyr::filter(penalty == i) |>
      pull(.pred_Control)
    expect_equal(unname(exp_pred), obs_pred)
  }
})

# ------------------------------------------------------------------------------

test_that("non-formula method", {
  skip_on_cran()
  skip_if_not_installed("xrf")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  set.seed(4526)
  rf_fit_exp <-
    xrf::xrf(
      Class ~ .,
      data = ad_data$ad_mod,
      family = "binomial",
      xgb_control = list(nrounds = 3, min_child_weight = 3, penalty = 1),
      verbose = 0
    )
  rf_pred_exp <- predict(rf_fit_exp, ad_data$ad_pred, lambda = 1)[, 1]
  rf_pred_exp <- factor(
    ifelse(rf_pred_exp >= 0.5, ad_data$lvls[2], ad_data$lvls[1]),
    levels = ad_data$lvls
  )
  rf_pred_exp <- unname(rf_pred_exp)
  rf_prob_exp <- predict(
    rf_fit_exp,
    ad_data$ad_pred,
    lambda = 1,
    type = "response"
  )[, 1]

  expect_no_error(
    rf_mod <-
      rule_fit(trees = 3, min_n = 3, penalty = 1) |>
      set_engine("xrf") |>
      set_mode("classification")
  )

  expect_no_error(
    rf_fit <- fit_xy(
      rf_mod,
      x = ad_data$ad_mod[, -1],
      y = ad_data$ad_mod$Class
    )
  )
  rf_pred <- predict(rf_fit, ad_data$ad_pred)
  rf_prob <- predict(rf_fit, ad_data$ad_pred, type = "prob")

  expect_equal(
    unname(rf_fit_exp$xgb$evaluation_log),
    unname(rf_fit$fit$xgb$evaluation_log)
  )

  expect_equal(names(rf_pred), ".pred_class")
  expect_true(tibble::is_tibble(rf_pred))
  expect_equal(rf_pred$.pred_class, unname(rf_pred_exp))

  expect_equal(names(rf_prob), paste0(".pred_", ad_data$lvls))
  expect_true(tibble::is_tibble(rf_prob))
  expect_equal(rf_prob$.pred_Control, unname(rf_prob_exp))

  expect_no_error(
    rf_m_pred <- multi_predict(rf_fit, ad_data$ad_pred, penalty = ad_data$vals)
  )
  expect_no_error(
    rf_m_prob <- multi_predict(
      rf_fit,
      ad_data$ad_pred,
      penalty = ad_data$vals,
      type = "prob"
    )
  )

  rf_m_pred <-
    rf_m_pred |>
    mutate(.row_number = 1:nrow(rf_m_pred)) |>
    tidyr::unnest(cols = c(.pred)) |>
    arrange(penalty, .row_number)

  for (i in ad_data$vals) {
    exp_pred <- predict(rf_fit_exp, ad_data$ad_pred, lambda = i)[, 1]
    exp_pred <- factor(
      ifelse(exp_pred >= 0.5, ad_data$lvls[2], ad_data$lvls[1]),
      levels = ad_data$lvls
    )
    exp_pred <- unname(exp_pred)
    obs_pred <- rf_m_pred |> dplyr::filter(penalty == i) |> pull(.pred_class)
    expect_equal(unname(exp_pred), obs_pred)
  }

  rf_m_prob <-
    rf_m_prob |>
    mutate(.row_number = 1:nrow(rf_m_prob)) |>
    tidyr::unnest(cols = c(.pred)) |>
    arrange(penalty, .row_number)

  for (i in ad_data$vals) {
    exp_pred <- predict(
      rf_fit_exp,
      ad_data$ad_pred,
      lambda = i,
      type = "response"
    )[, 1]
    obs_pred <- rf_m_prob |>
      dplyr::filter(penalty == i) |>
      pull(.pred_Control)
    expect_equal(unname(exp_pred), obs_pred)
  }
})

# ------------------------------------------------------------------------------

test_that("tidy method - two classes", {
  skip_on_cran()
  skip_if_not_installed("xrf")
  skip_if_not_installed("modeldata")

  ad_data <- make_ad_data()

  library(xrf)

  xrf_cls_mod <-
    rule_fit(trees = 3, penalty = .001) |>
    set_engine("xrf") |>
    set_mode("classification")

  set.seed(1)
  xrf_cls_fit <-
    xrf_cls_mod |>
    fit(Class ~ ., data = ad_data$ad_mod)
  xrf_rule_res <- tidy(xrf_cls_fit, penalty = .001)
  raw_coef <- coef(xrf_cls_fit$fit, lambda = 0.001)
  raw_coef <- raw_coef[raw_coef[, 1] != 0, ]
  expect_true(nrow(raw_coef) == nrow(xrf_rule_res))
  expect_true(all(raw_coef$term %in% xrf_rule_res$rule_id))

  xrf_col_res <- tidy(xrf_cls_fit, unit = "column", penalty = .001)
  expect_equal(
    sort(unique(xrf_col_res$term)),
    c("(Intercept)", "Genotype", "MMP10", "p_tau")
  )
  expect_equal(
    sort(unique(raw_coef$term)),
    sort(unique(xrf_col_res$rule_id))
  )

  expect_snapshot_error(tidy(xrf_cls_fit))
})


test_that("tunable", {
  skip_if_not_installed("xrf")
  rule_fit_xrf <-
    rule_fit(
      tree_depth = tune(),
      trees = tune(),
      learn_rate = tune(),
      mtry = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      penalty = tune()
    ) |>
    set_engine("xrf") |>
    set_mode("classification") |>
    tunable()

  expect_equal(
    rule_fit_xrf$call_info[rule_fit_xrf$name == "trees"][[1]]$range,
    c(5L, 100L)
  )

  expect_equal(
    rule_fit_xrf$call_info[rule_fit_xrf$name == "tree_depth"][[1]]$range,
    c(1L, 10L)
  )

  expect_equal(
    rule_fit_xrf$call_info[rule_fit_xrf$name == "learn_rate"][[1]]$range,
    c(-10, 0)
  )

  expect_equal(
    rule_fit_xrf$call_info[rule_fit_xrf$name == "sample_size"][[1]]$range,
    c(0.50, 0.95)
  )
})

test_that("mode specific package dependencies", {
  expect_identical(
    get_from_env(paste0("rule_fit", "_pkgs")) |>
      dplyr::filter(engine == "xrf", mode == "classification") |>
      dplyr::pull(pkg),
    list(c("xrf", "rules"))
  )

  expect_identical(
    get_from_env(paste0("rule_fit", "_pkgs")) |>
      dplyr::filter(engine == "xrf", mode == "regression") |>
      dplyr::pull(pkg),
    list(c("xrf", "rules"))
  )
})
