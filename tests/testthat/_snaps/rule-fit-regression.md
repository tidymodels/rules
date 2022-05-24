# rule_fit handles mtry vs mtry_prop gracefully

    The supplied argument `mtry = 0.5` must be greater than or equal to 1. 
    
    `mtry` is currently being interpreted as a count rather than a proportion. Supply `counts = FALSE` to `set_engine` to supply this argument as a proportion rather than a count. 
    
    See `?details_rule_fit_xrf` for more details.

---

    The supplied argument `mtry = 3` must be less than or equal to 1. 
    
    `mtry` is currently being interpreted as a proportion rather than a count. Supply `counts = TRUE` to `set_engine` to supply this argument as a count rather than a proportion. 
    
    See `?details_rule_fit_xrf` for more details.

---

    Code
      pars_fit_8 <- rule_fit(mtry = 0.5, trees = 5) %>% set_engine("xrf",
        colsample_bytree = 0.5) %>% set_mode("regression") %>% fit(Sale_Price ~
        Neighborhood + Longitude + Latitude + Gr_Liv_Area + Central_Air, data = ames_data$
        ames)
    Warning <rlang_warning>
      The following arguments cannot be manually modified and were removed: colsample_bytree.
    Error <rlang_error>
      The supplied argument `mtry = 0.5` must be greater than or equal to 1. 
      
      `mtry` is currently being interpreted as a count rather than a proportion. Supply `counts = FALSE` to `set_engine` to supply this argument as a proportion rather than a count. 
      
      See `?details_rule_fit_xrf` for more details.

