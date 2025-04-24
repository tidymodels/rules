make_chi_data <- function() {
  Chicago <- modeldata::Chicago

  Chicago$Cubs <- factor(ifelse(Chicago$Cubs_Home, "home", "away"))

  chi_mod <- Chicago %>%
    dplyr::slice(-(1:10)) %>%
    dplyr::select(ridership, Cubs, Austin, Clark_Lake)
  chi_pred <- Chicago %>%
    dplyr::slice(1:10) %>%
    dplyr::select(Cubs, Austin, Clark_Lake)
  chi_mod_x <-
    model.matrix(ridership ~ ., data = chi_mod) %>%
    as.data.frame() %>%
    dplyr::select(-1)
  chi_pred_x <-
    model.matrix(~., data = chi_pred) %>%
    as.data.frame() %>%
    dplyr::select(-1)

  chi_mod <- as.data.frame(chi_mod)
  chi_pred <- as.data.frame(chi_pred)

  vals <- c(0.01, .1, 1)

  list(
    chi_mod = chi_mod,
    chi_pred = chi_pred,
    chi_mod_x = chi_mod_x,
    chi_pred_x = chi_pred_x,
    Chicago = Chicago,
    vals = vals
  )
}

# ------------------------------------------------------------------------------

make_ad_data <- function() {
  ad_data <- modeldata::ad_data

  ad_mod <- ad_data %>%
    dplyr::slice(-(1:10)) %>%
    dplyr::select(Class, Genotype, p_tau, MMP10)
  ad_pred <- ad_data %>%
    dplyr::slice(1:10) %>%
    dplyr::select(Genotype, p_tau, MMP10)
  ad_mod_x <-
    model.matrix(Class ~ ., data = ad_mod) %>%
    as.data.frame() %>%
    dplyr::select(-1)
  ad_pred_x <-
    model.matrix(~., data = ad_pred) %>%
    as.data.frame() %>%
    dplyr::select(-1)

  ad_mod <- as.data.frame(ad_mod)
  ad_pred <- as.data.frame(ad_pred)

  list(
    ad_mod = ad_mod,
    ad_pred = ad_pred,
    ad_mod_x = ad_mod_x,
    ad_pred_x = ad_pred_x,
    ad_data = ad_data,
    lvls = levels(ad_mod$Class),
    vals = c(0.01, .1, 1)
  )
}

# ------------------------------------------------------------------------------

make_ames_data <- function() {
  ames <- modeldata::ames

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
  hpc_data <- modeldata::hpc_data

  set.seed(1001)
  keep <- sample(1:nrow(hpc_data), 1510)
  in_mod <- keep[1:1500]
  in_pred <- keep[1501:1510]

  hpc_mod <- hpc_data %>%
    dplyr::slice(in_mod) %>%
    dplyr::select(class, compounds, protocol, input_fields)
  hpc_pred <- hpc_data %>%
    dplyr::slice(in_pred) %>%
    dplyr::select(compounds, protocol, input_fields)

  hpc_mod <- as.data.frame(hpc_mod)
  hpc_pred <- as.data.frame(hpc_pred)

  vals <- c(0.01, .1, 1)
  lvls <- levels(hpc_mod$class)

  list(
    hpc_mod = hpc_mod,
    hpc_pred = hpc_pred,
    hpc_data = hpc_data,
    vals = vals,
    lvls = lvls
  )
}

expect_error_free <- function(...) {
  testthat::expect_error(..., regexp = NA)
}
