# early stopping works in xrf_fit

    Code
      suppressMessages(rf_fit_3 <- fit(rf_mod_3, mpg ~ ., data = mtcars))
    Condition
      Warning:
      `early_stop` was reduced to 4.

# xrf_fit guards xgb_control

    Code
      suppressMessages(fit(rf_mod, mpg ~ ., data = mtcars))
    Condition
      Warning:
      The argument `xgb_control` cannot be manually modified and was removed.
    Output
      parsnip model object
      
      An eXtreme RuleFit model of 7 rules.
      
      Original Formula:
      
      mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb 

