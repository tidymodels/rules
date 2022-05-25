skip_if_not_installed("modeldata")
library(modeldata)

library(dplyr)

# ------------------------------------------------------------------------------

make_chi_data <- function() {
  data("Chicago", package = "modeldata")

  Chicago$Cubs <- factor(ifelse(Chicago$Cubs_Home, "home", "away"))

  chi_mod  <- Chicago %>% slice(-(1:10)) %>% select(ridership, Cubs, Austin, Clark_Lake)
  chi_pred <- Chicago %>% slice(  1:10 ) %>% select(           Cubs, Austin, Clark_Lake)
  chi_mod_x <-
    model.matrix(ridership ~ ., data = chi_mod) %>%
    as.data.frame() %>%
    select(-1)
  chi_pred_x <-
    model.matrix( ~ ., data = chi_pred) %>%
    as.data.frame() %>%
    select(-1)

  chi_mod <- as.data.frame(chi_mod)
  chi_pred <- as.data.frame(chi_pred)

  vals <- c(0.01, .1, 1)

  list(chi_mod = chi_mod, chi_pred = chi_pred,
       chi_mod_x = chi_mod_x, chi_pred_x = chi_pred_x,
       Chicago = Chicago, vals = vals)
}

# ------------------------------------------------------------------------------

make_ad_data <- function() {
  data("ad_data", package = "modeldata")

  ad_mod  <- ad_data %>% slice(-(1:10)) %>% select(Class, Genotype, p_tau, MMP10)
  ad_pred <- ad_data %>% slice(  1:10 ) %>% select(       Genotype, p_tau, MMP10)
  ad_mod_x <-
    model.matrix(Class ~ ., data = ad_mod)  %>%
    as.data.frame() %>%
    select(-1)
  ad_pred_x <-
    model.matrix( ~ ., data = ad_pred)  %>%
    as.data.frame() %>%
    select(-1)

  ad_mod <- as.data.frame(ad_mod)
  ad_pred <- as.data.frame(ad_pred)

  list(ad_mod = ad_mod, ad_pred = ad_pred,
       ad_mod_x = ad_mod_x, ad_pred_x = ad_pred_x,
       ad_data = ad_data, lvls = levels(ad_mod$Class), vals = c(0.01, .1, 1))
}

# ------------------------------------------------------------------------------

make_ames_data <- function() {
  data(ames, package = "modeldata")

  ames <-
    ames %>%
    dplyr::mutate(
      Sale_Price = log10(Sale_Price),
      Gr_Liv_Area = log10(Gr_Liv_Area)
    )

  list(ames = ames)
}

# ------------------------------------------------------------------------------

make_hpc_data <- function(x) {
  data(hpc_data, package = "modeldata")

  set.seed(1001)
  keep <- sample(1:nrow(hpc_data), 1510)
  in_mod  <- keep[1:1500]
  in_pred <- keep[1501:1510]

  hpc_mod  <- hpc_data %>% slice(in_mod ) %>% select(class, compounds, protocol, input_fields)
  hpc_pred <- hpc_data %>% slice(in_pred) %>% select(       compounds, protocol, input_fields)

  hpc_mod <- as.data.frame(hpc_mod)
  hpc_pred <- as.data.frame(hpc_pred)

  vals <- c(0.01, .1, 1)
  lvls <- levels(hpc_mod$class)

  list(hpc_mod = hpc_mod, hpc_pred = hpc_pred, hpc_data = hpc_data,
       vals = vals, lvls = lvls)
}

expect_error_free <- function(...) {
  testthat::expect_error(..., regexp = NA)
}
