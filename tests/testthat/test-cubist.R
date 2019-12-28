context("cubist fits")

source(file.path(test_path(), "test-helpers.R"))

ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

# ------------------------------------------------------------------------------

test_that('formula method', {

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod_x,
      y = chi_mod$ridership,
      committees = 10,
      control = Cubist::cubistControl(seed = 2)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_pred_x)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 10) %>%
      set_engine("Cubist", seed = 2),
    NA
  )

  expect_error(
    cb_fit <- fit(cb_mod, ridership ~ ., data = chi_mod),
    NA
  )
  cb_pred <- predict(cb_fit, chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)

  # fails
  # cb_pred <- multi_predict(cb_fit, as.data.frame(chi_pred), neighbors = c(0, 1, 9))
})

# ------------------------------------------------------------------------------

test_that('formula method - limited rules', {

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod_x,
      y = chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(rules = 3, seed = 2)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_pred_x)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2, max_rules = 3) %>%
      set_engine("Cubist", seed = 2),
    NA
  )

  expect_error(
    cb_fit <- fit(cb_mod, ridership ~ ., data = chi_mod),
    NA
  )
  cb_pred <- predict(cb_fit, chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})

# ------------------------------------------------------------------------------

test_that('formula method - limited rules and control', {

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod_x,
      y = chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(rules = 3, seed = 2, unbiased = TRUE)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_pred_x)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2, max_rules = 3) %>%
      set_engine("Cubist", control = ctrl),
    NA
  )

  expect_error(
    cb_fit <- fit(cb_mod, ridership ~ ., data = chi_mod),
    NA
  )
  cb_pred <- predict(cb_fit, chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})


# ------------------------------------------------------------------------------

test_that('formula method - control', {

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod_x,
      y = chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(seed = 2, unbiased = TRUE)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_pred_x)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2) %>%
      set_engine("Cubist", control = ctrl),
    NA
  )

  expect_error(
    cb_fit <- fit(cb_mod, ridership ~ ., data = chi_mod),
    NA
  )
  cb_pred <- predict(cb_fit, chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})

# ------------------------------------------------------------------------------

test_that('non-formula method', {

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod[, -1],
      y = chi_mod$ridership,
      committees = 10,
      control = Cubist::cubistControl(seed = 2)
    )
  cb_pred_exp <- predict(cb_fit_exp, as.data.frame(chi_pred))

  expect_error(
    cb_mod <-
      cubist_rules(committees = 10) %>%
      set_engine("Cubist", seed = 2),
    NA
  )

  expect_error(
    cb_fit <- fit_xy(cb_mod, x = chi_mod[, -1], y = chi_mod$ridership),
    NA
  )
  cb_pred <- predict(cb_fit, chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)

  K <- c(0, 1, 9)

  expect_error(
    cb_m_pred <- multi_predict(cb_fit, as.data.frame(chi_pred), neighbors = K),
    NA
  )
  cb_m_pred <-
    cb_m_pred %>%
    mutate(.rows = 1:nrow(cb_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(neighbors, .rows)

  for (i in K) {
    exp_pred <- predict(cb_fit_exp, as.data.frame(chi_pred), neighbors = i)
    obs_pred <- cb_m_pred %>% dplyr::filter(neighbors == i) %>% pull(.pred)
    expect_equal(exp_pred, obs_pred)
  }
})

# ------------------------------------------------------------------------------

test_that('non-formula method - limited rules', {

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod[, -1],
      y = chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(rules = 3, seed = 2)
    )
  cb_pred_exp <- predict(cb_fit_exp, as.data.frame(chi_pred))

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2, max_rules = 3) %>%
      set_engine("Cubist", seed = 2),
    NA
  )

  expect_error(
    cb_fit <- fit_xy(cb_mod, x = chi_mod[, -1], y = chi_mod$ridership),
    NA
  )
  cb_pred <- predict(cb_fit, chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})

# ------------------------------------------------------------------------------

test_that('non-formula method - limited rules and control', {

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod[, -1],
      y = chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(rules = 3, seed = 2, unbiased = TRUE)
    )
  cb_pred_exp <- predict(cb_fit_exp, as.data.frame(chi_pred))

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2, max_rules = 3) %>%
      set_engine("Cubist", control = ctrl),
    NA
  )

  expect_error(
    cb_fit <- fit_xy(cb_mod, x = chi_mod[, -1], y = chi_mod$ridership),
    NA
  )
  cb_pred <- predict(cb_fit, chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})


# ------------------------------------------------------------------------------

test_that('non-formula method - control', {

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod[, -1],
      y = chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(seed = 2, unbiased = TRUE)
    )
  cb_pred_exp <- predict(cb_fit_exp, as.data.frame(chi_pred))

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2) %>%
      set_engine("Cubist", control = ctrl),
    NA
  )

  expect_error(
    cb_fit <- fit_xy(cb_mod, x = chi_mod[, -1], y = chi_mod$ridership),
    NA
  )
  cb_pred <- predict(cb_fit, chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})

# ------------------------------------------------------------------------------

test_that('cubist parameters', {
  expect_equal(max_rules(1:2)$range,  list(lower = 1L, upper = 2L))
  expect_equal(committees(1:2)$range, list(lower = 1L, upper = 2L))
})
