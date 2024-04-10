# check_args() works

    Code
      spec <- cubist_rules(committees = c(1, 2, 3)) %>% set_engine("Cubist") %>%
        set_mode("regression")
      fit(spec, ridership ~ ., data = chi_data$chi_mod)
    Condition
      Error in `fit()`:
      ! Only a single value of `committees` should be passed, not 3.

---

    Code
      spec <- cubist_rules(committees = 0) %>% set_engine("Cubist") %>% set_mode(
        "regression")
      res <- fit(spec, ridership ~ ., data = chi_data$chi_mod)
    Condition
      Warning:
      The number of committees should be `>= 1` and `<= 100`.
      Truncating to 100.

---

    Code
      spec <- cubist_rules(committees = 1000) %>% set_engine("Cubist") %>% set_mode(
        "regression")
      res <- fit(spec, ridership ~ ., data = chi_data$chi_mod)
    Condition
      Warning:
      The number of committees should be `>= 1` and `<= 100`.
      Truncating to 100.

---

    Code
      spec <- cubist_rules(neighbors = c(1, 2, 3)) %>% set_engine("Cubist") %>%
        set_mode("regression")
      fit(spec, ridership ~ ., data = chi_data$chi_mod)
    Condition
      Error in `fit()`:
      ! Only a single value of `neighbors` should be passed, not 3.

---

    Code
      spec <- cubist_rules(neighbors = -1) %>% set_engine("Cubist") %>% set_mode(
        "regression")
      res <- fit(spec, ridership ~ ., data = chi_data$chi_mod)
    Condition
      Warning:
      The number of neighbors should be `>= 0` and `<= 9`.
      Truncating to 0.

---

    Code
      spec <- cubist_rules(neighbors = 1000) %>% set_engine("Cubist") %>% set_mode(
        "regression")
      res <- fit(spec, ridership ~ ., data = chi_data$chi_mod)
    Condition
      Warning:
      The number of neighbors should be `>= 0` and `<= 9`.
      Truncating to 9.

