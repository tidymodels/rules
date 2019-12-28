library(modeldata)
library(dplyr)

# ------------------------------------------------------------------------------

data("Chicago")

Chicago$Cubs <- factor(ifelse(Chicago$Cubs_Home, "home", "away"))

chi_mod  <- Chicago %>% slice(-(1:10)) %>% select(ridership, Cubs, Austin, Clark_Lake)
chi_pred <- Chicago %>% slice(  1:10 ) %>% select(           Cubs, Austin, Clark_Lake)
chi_mod_x <-
  model.matrix(ridership ~ ., data = chi_mod)  %>%
  as.data.frame() %>%
  select(-1)
chi_pred_x <-
  model.matrix( ~ ., data = chi_pred)  %>%
  as.data.frame() %>%
  select(-1)

# ------------------------------------------------------------------------------

data("ad_data")

ad_mod  <- ad_data %>% slice(-(1:10)) %>% select(Class, Genotype, p_tau, MMP10)
ad_pred <- ad_data %>% slice(  1:10 ) %>% select(       Genotype, p_tau, MMP10)

# ------------------------------------------------------------------------------

data("hpc_data")

hpc_mod  <- hpc_data %>% slice(-(1:10)) %>% select(class, compounds, protocol, input_fields)
hpc_pred <- hpc_data %>% slice(  1:10 ) %>% select(       compounds, protocol, input_fields)
