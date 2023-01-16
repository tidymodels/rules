# rules (development version)

* The `tidy()` method for RuleFit models was not using the penalty value. This is corrected and a single penalty value is required for using the function. (#66)

* Fixed bug where predict sometimes didn't work for xrf models.

* C50 tree `tidy()` method nor works with threes by showing terminal nodes in rules.

# rules 1.0.0

* `tidy()` method for Cubist models now has an option for how many committees to tidy. 

* Added a `tidy()` method for C5.0 rule-based models. 

* The `mtry_prop` parameter was moved to the dials package and is now re-exported here for backward compatibility.

* A bug was fixed related to `multi_predict()` with C5.0 rule-based models (#49).

* The `mtry` argument is now mapped to `colsample_bynode` rather than `colsample_bytree`. This is consistent with parsnip's interface to `xgboost` as of parsnip 0.1.6. `colsample_bytree` can still be optimized by passing it in as an engine argument to `set_engine()` (#60).

* Introduced support for early stopping in `rule_fit()` via the `stop_iter` argument. See `parsnip::details_rule_fit_xrf`. Note that this is a _main_ argument to `rule_fit()` requiring parsnip 1.0.0.

# rules 0.2.0

* Model definition functions (e.g. `cubist_rules()`) were moved to the parsnip package.

# rules 0.1.2

* Maintenance release demanded by CRAN.

# rules 0.1.1

* Added `tidy()` methods for cubist and RuleFit models to get back the rules and model coefficients. 

* Clean up `tunable()` methods that define the default parameter ranges for the tuning parameters. 

* Changes to test for cases when CRAN cannot get `xgboost` to work on their Solaris configuration. 

* Case weights were enabled for [C5_rules()] and [cubist_rules()].

# rules 0.1.0

* The default encoding methods were changed such that `parsnip` will respect the encoding that the underlying model uses. 

# rules 0.0.3

* Documentation changes (some demanded by CRAN).

* Changes to make `rules` work well with PSOCK cluster parallelism. 

# rules 0.0.2

* Changes for new `dplyr` version. 

# rules 0.0.1

* First CRAN release
