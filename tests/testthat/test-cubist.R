library(dplyr)

test_that("argument/call assembly", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  ctrl_1 <- Cubist::cubistControl(unbiased = TRUE, seed = 2)
  ctrl_2 <- Cubist::cubistControl(unbiased = TRUE, seed = 2, rules = 13)

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), committees = 12)),
    rlang::call2("cubist", .ns = "Cubist", quote(x), quote(y), committees = 12)
  )

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), max_rules = 1)),
    rlang::call2(
      "cubist",
      .ns = "Cubist",
      quote(x), quote(y),
      control = quote(Cubist::cubistControl(rules = 1))
    )
  )

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), max_rules = NA)),
    rlang::call2(
      "cubist",
      .ns = "Cubist",
      quote(x), quote(y),
      control = quote(Cubist::cubistControl(rules = NA))
    )
  )

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), max_rules = NA, control = ctrl_1)),
    rlang::call2(
      "cubist",
      .ns = "Cubist",
      quote(x), quote(y),
      control = list(
        unbiased = TRUE, rules = NA, extrapolation = 1,
        sample = 0, label = "outcome", seed = 2
      )
    )
  )

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), control = ctrl_2)),
    rlang::call2(
      "cubist",
      .ns = "Cubist",
      quote(x), quote(y),
      control = list(
        unbiased = TRUE, rules = 13, extrapolation = 1,
        sample = 0, label = "outcome", seed = 2
      )
    )
  )

  expect_equal(
    rules:::cubist_args(list(quote(x), quote(y), max_rules = 31, control = ctrl_2)),
    rlang::call2("cubist",
                 .ns = "Cubist", quote(x), quote(y),
                 control = list(
                   unbiased = TRUE, rules = 31, extrapolation = 1,
                   sample = 0, label = "outcome", seed = 2
                 )
    )
  )
})


# ------------------------------------------------------------------------------

test_that("formula method", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  chi_data <- make_chi_data()

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
      y = chi_data$chi_mod$ridership,
      committees = 10,
      control = Cubist::cubistControl(seed = 2)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_data$chi_pred)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 10) %>%
      set_engine("Cubist", seed = 2),
    NA
  )

  expect_error(
    cb_fit <- fit(cb_mod, ridership ~ ., data = chi_data$chi_mod),
    NA
  )
  cb_pred <- predict(cb_fit, chi_data$chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)

  cb_pred <-
    multi_predict(cb_fit, chi_data$chi_pred[1:2, ], neighbors = c(0, 1, 9)) %>%
    mutate(.row = row_number()) %>%
    tidyr::unnest(cols = c(.pred))

  # Will be slightly different due to the value of `maxd`
  expect_equal(
    cb_pred$.pred[cb_pred$neighbors == 0],
    predict(cb_fit_exp, chi_data$chi_mod[1:2, ], neighbors = 0),
    tolerance = .1
  )
  expect_equal(
    cb_pred$.pred[cb_pred$neighbors == 1],
    predict(cb_fit_exp, chi_data$chi_mod[1:2, ], neighbors = 1),
    tolerance = .1
  )
  expect_equal(
    cb_pred$.pred[cb_pred$neighbors == 9],
    predict(cb_fit_exp, chi_data$chi_mod[1:2, ], neighbors = 9),
    tolerance = .1
  )
})

test_that("formula method - case weights", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  chi_data <- make_chi_data()

  wts <- importance_weights(1:nrow(chi_data$chi_mod))

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
      y = chi_data$chi_mod$ridership,
      committees = 10,
      control = Cubist::cubistControl(seed = 2),
      weights = as.double(wts)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_data$chi_pred)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 10) %>%
      set_engine("Cubist", seed = 2),
    NA
  )

  expect_error(
    cb_fit <- fit(cb_mod, ridership ~ ., data = chi_data$chi_mod,
                  case_weights = wts),
    NA
  )
  cb_pred <- predict(cb_fit, chi_data$chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})


# ------------------------------------------------------------------------------

test_that("formula method - limited rules", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  chi_data <- make_chi_data()

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
      y = chi_data$chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(rules = 3, seed = 2)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_data$chi_pred)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2, max_rules = 3) %>%
      set_engine("Cubist", seed = 2),
    NA
  )

  expect_error(
    cb_fit <- fit(cb_mod, ridership ~ ., data = chi_data$chi_mod),
    NA
  )
  cb_pred <- predict(cb_fit, chi_data$chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("formula method - limited rules and control", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  chi_data <- make_chi_data()

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
      y = chi_data$chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(rules = 3, seed = 2, unbiased = TRUE)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_data$chi_pred)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2, max_rules = 3) %>%
      set_engine("Cubist", control = ctrl),
    NA
  )

  expect_error(
    cb_fit <- fit(cb_mod, ridership ~ ., data = chi_data$chi_mod),
    NA
  )
  cb_pred <- predict(cb_fit, chi_data$chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})


# ------------------------------------------------------------------------------

test_that("formula method - control", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  chi_data <- make_chi_data()

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
      y = chi_data$chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(seed = 2, unbiased = TRUE)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_data$chi_pred)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2, neighbors = 0) %>%
      set_engine("Cubist", control = ctrl),
    NA
  )

  expect_error(
    cb_fit <- fit(cb_mod, ridership ~ ., data = chi_data$chi_mod),
    NA
  )
  cb_pred <- predict(cb_fit, chi_data$chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("non-formula method", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  chi_data <- make_chi_data()

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
      y = chi_data$chi_mod$ridership,
      committees = 10,
      control = Cubist::cubistControl(seed = 2)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_data$chi_pred)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 10, neighbors = 0) %>%
      set_engine("Cubist", seed = 2),
    NA
  )

  expect_error(
    cb_fit <-
      fit_xy(
        cb_mod,
        x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
        y = chi_data$chi_mod$ridership
      ),
    NA
  )
  cb_pred <- predict(cb_fit, chi_data$chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)

  K <- c(0, 1, 9)

  expect_error(
    cb_m_pred <- multi_predict(cb_fit, chi_data$chi_pred, neighbors = K),
    NA
  )
  cb_m_pred <-
    cb_m_pred %>%
    mutate(.row_number = 1:nrow(cb_m_pred)) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    arrange(neighbors, .row_number)

  for (i in K) {
    exp_pred <- predict(cb_fit_exp, chi_data$chi_pred, neighbors = i)
    obs_pred <- cb_m_pred %>%
      dplyr::filter(neighbors == i) %>%
      pull(.pred)
    expect_equal(exp_pred, obs_pred)
  }
})

test_that("non-formula method - case weights", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  chi_data <- make_chi_data()

  wts <- importance_weights(1:nrow(chi_data$chi_mod))

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
      y = chi_data$chi_mod$ridership,
      committees = 10,
      control = Cubist::cubistControl(seed = 2),
      weights = as.double(wts)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_data$chi_pred)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 10, neighbors = 0) %>%
      set_engine("Cubist", seed = 2),
    NA
  )

  expect_error(
    cb_fit <-
      fit_xy(
        cb_mod,
        x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
        y = chi_data$chi_mod$ridership,
        case_weights = wts
      ),
    NA
  )
  cb_pred <- predict(cb_fit, chi_data$chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("non-formula method - limited rules", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  chi_data <- make_chi_data()

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
      y = chi_data$chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(rules = 3, seed = 2)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_data$chi_pred)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2, max_rules = 3, neighbors = 0) %>%
      set_engine("Cubist", seed = 2),
    NA
  )

  expect_error(
    cb_fit <- fit_xy(
      cb_mod,
      x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
      y = chi_data$chi_mod$ridership
    ),
    NA
  )
  cb_pred <- predict(cb_fit, chi_data$chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("non-formula method - limited rules and control", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  chi_data <- make_chi_data()

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_data$chi_mod[, names(chi_data$chi_mod) != "ridership"],
      y = chi_data$chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(rules = 3, seed = 2, unbiased = TRUE)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_data$chi_pred)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2, max_rules = 3, neighbors = 0) %>%
      set_engine("Cubist", control = ctrl),
    NA
  )

  expect_error(
    cb_fit <- fit_xy(cb_mod, x = chi_data$chi_mod[, -1], y = chi_data$chi_mod$ridership),
    NA
  )
  cb_pred <- predict(cb_fit, chi_data$chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})


# ------------------------------------------------------------------------------

test_that("non-formula method - control", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  chi_data <- make_chi_data()

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_fit_exp <-
    Cubist::cubist(
      x = chi_data$chi_mod[, -1],
      y = chi_data$chi_mod$ridership,
      committees = 2,
      control = Cubist::cubistControl(seed = 2, unbiased = TRUE)
    )
  cb_pred_exp <- predict(cb_fit_exp, chi_data$chi_pred)

  expect_error(
    cb_mod <-
      cubist_rules(committees = 2, neighbors = 0) %>%
      set_engine("Cubist", control = ctrl),
    NA
  )

  expect_error(
    cb_fit <- fit_xy(cb_mod,
                     x = chi_data$chi_mod[, -1],
                     y = chi_data$chi_mod$ridership),
    NA
  )
  cb_pred <- predict(cb_fit, chi_data$chi_pred)

  expect_equal(cb_fit_exp$coefficients, cb_fit$fit$coefficients)
  expect_equal(names(cb_pred), ".pred")
  expect_true(tibble::is_tibble(cb_pred))
  expect_equal(cb_pred$.pred, cb_pred_exp)
})

# ------------------------------------------------------------------------------

test_that("cubist parameters", {
  expect_equal(max_rules(1:2)$range, list(lower = 1L, upper = 2L))
  expect_equal(committees(1:2)$range, list(lower = 1L, upper = 2L))
})

# ------------------------------------------------------------------------------

test_that("tidy method for cubist - one committee", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  ames_data <- make_ames_data()

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_single <- cubist_rules(committees = 1) %>% set_engine("Cubist")

  set.seed(1)
  cb_single_fit <-
    cb_single %>%
    fit(Sale_Price ~ Neighborhood + Longitude + Latitude +
          Gr_Liv_Area + Central_Air, data = ames_data$ames)

  # check for consistent cubist model:
  expect_output(
    print(summary(cb_single_fit$fit)),
    "Rule 18: \\[269 cases, mean 5.517277, range 5.167317 to 5.877947, est err 0.061801\\]"
  )

  cb_single_fit_res <- tidy(cb_single_fit)
  expect_equal(max(cb_single_fit_res$rule_num), 18)
  expect_equal(max(cb_single_fit_res$committee), 1)

  expect_equal(cb_single_fit_res$statistic[[18]]$error, 0.061801)
  expect_equal(unname(cb_single_fit_res$estimate[[18]]$estimate[1]), -404.368656)
  expect_equal(unname(cb_single_fit_res$estimate[[18]]$estimate[2]), 9.68)
  expect_true(grepl("\\( Gr_Liv_Area", cb_single_fit_res$rule[18]))
  expect_true(grepl("\\( Neighborhood", cb_single_fit_res$rule[18]))
  expect_true(grepl("\\( Latitude", cb_single_fit_res$rule[18]))
  expect_true(!grepl("\\( Longitude", cb_single_fit_res$rule[18]))
  expect_true(!grepl("\\( Central_Air", cb_single_fit_res$rule[18]))

  # ------------------------------------------------------------------------------
  # limit number of commiittees


  cb_multi <- cubist_rules(committees = 5) %>% set_engine("Cubist")

  set.seed(1)
  cb_multi_fit <-
    cb_multi %>%
    fit(Sale_Price ~ Neighborhood + Longitude + Latitude +
          Gr_Liv_Area + Central_Air, data = ames)

  for (comm in 1:5) {
    cb_multi_fit_res <- tidy(cb_single_fit, committees = comm)
    expect_equal(cb_single_fit_res, cb_multi_fit_res)
  }
})

test_that("tidy method for cubist - one committee - only intercepts", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  ames_data <- make_ames_data()

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_single <- cubist_rules(committees = 1) %>% set_engine("Cubist")

  set.seed(1)
  cb_single_fit <-
    cb_single %>%
    fit(Sale_Price ~ Neighborhood + Central_Air + MS_SubClass, data = ames_data$ames)

  # check for consistent cubist model:
  expect_output(
    print(summary(cb_single_fit$fit)),
    "Rule 15: \\[216 cases, mean 5.536680, range 5.278754 to 5.877947, est err 0.084098\\]"
  )

  cb_single_fit_res <- tidy(cb_single_fit)
  terms <-
    cb_single_fit_res %>%
    dplyr::select(estimate) %>%
    tidyr::unnest(cols = c(estimate)) %>%
    pull(term) %>%
    unique()

  expect_true(all(terms == "(Intercept)"))
})


test_that("tidy method for cubist - many committees", {
  skip_on_cran()
  skip_if_not_installed("Cubist")

  ames_data <- make_ames_data()

  ctrl <- Cubist::cubistControl(unbiased = TRUE, seed = 2)

  cb_mult <- cubist_rules(committees = 10) %>% set_engine("Cubist")

  set.seed(1)
  cb_mult_fit <-
    cb_mult %>%
    fit(Sale_Price ~ Neighborhood + Longitude + Latitude +
          Gr_Liv_Area + Central_Air, data = ames_data$ames)

  # check for consistent cubist model:
  expect_output(
    print(summary(cb_mult_fit$fit)),
    "Rule 10/11: \\[143 cases, mean 5.493898, range 5.190332 to 5.788875, est err 0.066138\\]"
  )

  cb_mult_fit_res <- tidy(cb_mult_fit)
  expect_equal(max(cb_mult_fit_res$rule_num), 23)
  expect_equal(max(cb_mult_fit_res$committee), 10)

  n <- nrow(cb_mult_fit_res)

  expect_equal(cb_mult_fit_res$statistic[[n]]$error, 0.066138)
  expect_equal(unname(cb_mult_fit_res$estimate[[n]]$estimate[1]), -3184.858676)
  expect_equal(unname(cb_mult_fit_res$estimate[[n]]$estimate[2]), -27.66)
  expect_equal(unname(cb_mult_fit_res$estimate[[n]]$estimate[3]), 14.22)
  expect_equal(unname(cb_mult_fit_res$estimate[[n]]$estimate[4]), 0.54)
  expect_true(!grepl("\\( Gr_Liv_Area", cb_mult_fit_res$rule[n]))
  expect_true(!grepl("\\( Neighborhood", cb_mult_fit_res$rule[n]))
  expect_true(grepl("\\( Latitude", cb_mult_fit_res$rule[n]))
  expect_true(grepl("\\( Longitude", cb_mult_fit_res$rule[n]))
  expect_true(!grepl("\\( Central_Air", cb_mult_fit_res$rule[n]))

  # ------------------------------------------------------------------------------

  # case with no rule conditions inspired by
  # https://github.com/tidymodels/tidymodels.org/issues/205

  ames2 <- ames_data$ames
  set.seed(1)
  ames2$Neighborhood <- sample(ames2$Neighborhood)

  cb_single <- cubist_rules(committees = 1) %>% set_engine("Cubist")

  cb_single_fit <-
    cb_single %>%
    fit(Sale_Price ~ Neighborhood + Gr_Liv_Area, data = ames2)

  cb_mult_fit_res <- tidy(cb_single_fit)
  expect_true(nrow(cb_mult_fit_res) == 1)
  expect_equal(cb_mult_fit_res$rule[1], "<no conditions>")
})


test_that("tunable", {
  cubist_rules_Cubist <-
    cubist_rules(committees = tune(), neighbors = tune(), max_rules = tune()) %>%
    set_engine("Cubist") %>%
    tunable()

  expect_equal(
    cubist_rules_Cubist$call_info[cubist_rules_Cubist$name == "committees"][[1]]$range,
    c(1L, 100L)
  )

  expect_equal(
    cubist_rules_Cubist$call_info[cubist_rules_Cubist$name == "neighbors"][[1]]$range,
    c(0L, 9L)
  )
})

test_that("mode specific package dependencies", {
  expect_identical(
    get_from_env(paste0("cubist_rules", "_pkgs")) %>%
      dplyr::filter(engine == "Cubist", mode == "classification") %>%
      dplyr::pull(pkg),
    list()
  )

  expect_identical(
    get_from_env(paste0("cubist_rules", "_pkgs")) %>%
      dplyr::filter(engine == "Cubist", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("Cubist", "rules"))
  )
})


test_that('check_args() works', {
  skip_on_cran()
  skip_if_not_installed("Cubist")
  skip_if_not_installed("parsnip", "1.2.1.9001")
  chi_data <- make_chi_data()

  expect_snapshot(
    error = TRUE,
    {
      spec <- cubist_rules(committees = c(1, 2, 3)) %>% 
        set_engine("Cubist") %>%
        set_mode("regression")
      fit(spec, ridership ~ ., data = chi_data$chi_mod)
    }
  )

  expect_snapshot(
    {
      spec <- cubist_rules(committees = 0) %>% 
        set_engine("Cubist") %>%
        set_mode("regression")
      res <- fit(spec, ridership ~ ., data = chi_data$chi_mod)
    }
  )

  expect_snapshot(
    {
      spec <- cubist_rules(committees = 1000) %>% 
        set_engine("Cubist") %>%
        set_mode("regression")
      res <- fit(spec, ridership ~ ., data = chi_data$chi_mod)
    }
  )

  expect_snapshot(
    error = TRUE,
    {
      spec <- cubist_rules(neighbors = c(1, 2, 3)) %>% 
        set_engine("Cubist") %>%
        set_mode("regression")
      fit(spec, ridership ~ ., data = chi_data$chi_mod)
    }
  )

  expect_snapshot(
    {
      spec <- cubist_rules(neighbors = -1) %>% 
        set_engine("Cubist") %>%
        set_mode("regression")
      res <- fit(spec, ridership ~ ., data = chi_data$chi_mod)
    }
  )

  expect_snapshot(
    {
      spec <- cubist_rules(neighbors = 1000) %>% 
        set_engine("Cubist") %>%
        set_mode("regression")
      res <- fit(spec, ridership ~ ., data = chi_data$chi_mod)
    }
  )
})
