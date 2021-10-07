# rules (development version)

* Model definition functions (e.g. `cubist_rules()`) were moved to the parsnip package.

# rules 0.1.2

* Maintenance release demanded by CRAN.

# rules 0.1.1

* Added `tidy()` methods for cubist and RuleFit models to get back the rules and model coefficients. 

* Clean up `tunable()` methods that define the default parameter ranges for the tuning parameters. 

* Changes to test for cases when CRAN cannot get `xgboost` to work on their Solaris configuration. 

# rules 0.1.0

* The default encoding methods were changed such that `parsnip` will respect the encoding that the underlying model uses. 

# rules 0.0.3

* Documentation changes (some demanded by CRAN).

* Changes to make `rules` work well with PSOCK cluster parallelism. 

# rules 0.0.2

* Changes for new `dplyr` version. 

# rules 0.0.1

* First CRAN release
