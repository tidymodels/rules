context("cubist fits")

source(file.path(test_path(), "test-helpers.R"))

ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

# ------------------------------------------------------------------------------

test_that('argument/call assembly', {
  ctrl_1 <- Cubist::cubistControl(unbiased = TRUE, seed = 2)
  ctrl_2 <- Cubist::cubistControl(unbiased = TRUE, seed = 2, rules = 13)

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), committees = 12)),
    rlang::call2("cubist", .ns = "Cubist", quote(x), quote(y), committees = 12)
  )

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), max_rules = 1)),
    rlang::call2("cubist", .ns = "Cubist", quote(x), quote(y),
                 control = quote(Cubist::cubistControl(rules = 1)))
  )

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), max_rules = NA)),
    rlang::call2("cubist", .ns = "Cubist", quote(x), quote(y),
                 control = quote(Cubist::cubistControl(rules = NA)))
  )

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), max_rules = NA, control = ctrl_1)),
    rlang::call2("cubist", .ns = "Cubist", quote(x), quote(y),
                 control = list(unbiased = TRUE, rules = NA, extrapolation = 1,
                                sample = 0, label = "outcome", seed = 2))
    )

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), control = ctrl_2)),
    rlang::call2("cubist", .ns = "Cubist", quote(x), quote(y),
                 control = list(unbiased = TRUE, rules = 13, extrapolation = 1,
                                sample = 0, label = "outcome", seed = 2))
  )

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), max_rules = 31, control = ctrl_2)),
    rlang::call2("cubist", .ns = "Cubist", quote(x), quote(y),
                 control = list(unbiased = TRUE, rules = 31, extrapolation = 1,
                                sample = 0, label = "outcome", seed = 2))
  )
})


# ------------------------------------------------------------------------------

test_that('formula method', {
  skip_on_cran()

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

  cb_pred <-
    multi_predict(cb_fit, chi_pred[1:2,], neighbors = c(0, 1, 9)) %>%
    mutate(.row = row_number()) %>%
    tidyr::unnest(cols = c(.pred))

  # Will be slightly different due to the value of `maxd`
  expect_equal(
    cb_pred$.pred[cb_pred$neighbors == 0],
    predict(cb_fit_exp, chi_mod_x[1:2,], neighbors = 0),
    tol = .1
  )
  expect_equal(
    cb_pred$.pred[cb_pred$neighbors == 1],
    predict(cb_fit_exp, chi_mod_x[1:2,], neighbors = 1),
    tol = .1
  )
  expect_equal(
    cb_pred$.pred[cb_pred$neighbors == 9],
    predict(cb_fit_exp, chi_mod_x[1:2,], neighbors = 9),
    tol = .1
  )
})

# ------------------------------------------------------------------------------

test_that('formula method - limited rules', {
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod[, -1],
      y = chi_mod$ridership,
      committees = 10,
      control = Cubist::cubistControl(seed = 2)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_pred)

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
    cb_m_pred <- multi_predict(cb_fit, chi_pred, neighbors = K),
    NA
  )
  cb_m_pred <-
    cb_m_pred %>%
    mutate(.row_number = 1:nrow(cb_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(neighbors, .row_number)

  for (i in K) {
    exp_pred <- predict(cb_fit_exp, chi_pred, neighbors = i)
    obs_pred <- cb_m_pred %>% dplyr::filter(neighbors == i) %>% pull(.pred)
    expect_equal(exp_pred, obs_pred)
  }
})

# ------------------------------------------------------------------------------

test_that('non-formula method - limited rules', {
  skip_on_cran()

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod[, -1],
      y = chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(rules = 3, seed = 2)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_pred)

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
  skip_on_cran()

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod[, -1],
      y = chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(rules = 3, seed = 2, unbiased = TRUE)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_pred)

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
  skip_on_cran()

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_mod[, -1],
      y = chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(seed = 2, unbiased = TRUE)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_pred)

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
