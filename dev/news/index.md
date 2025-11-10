# Changelog

## rules (development version)

## rules 1.0.2

CRAN release: 2023-03-08

- Updated <Authors@R>.

## rules 1.0.1

CRAN release: 2023-01-17

- The [`tidy()`](https://generics.r-lib.org/reference/tidy.html) method
  for RuleFit models was not using the penalty value. This is corrected
  and a single penalty value is required for using the function.
  ([\#66](https://github.com/tidymodels/rules/issues/66))

- Fixed bug where predict sometimes didn’t work for xrf models.

- C50 tree [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
  method nor works with threes by showing terminal nodes in rules.

## rules 1.0.0

CRAN release: 2022-06-23

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) method for
  Cubist models now has an option for how many committees to tidy.

- Added a [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
  method for C5.0 rule-based models.

- The `mtry_prop` parameter was moved to the dials package and is now
  re-exported here for backward compatibility.

- A bug was fixed related to
  [`multi_predict()`](https://parsnip.tidymodels.org/reference/multi_predict.html)
  with C5.0 rule-based models
  ([\#49](https://github.com/tidymodels/rules/issues/49)).

- The `mtry` argument is now mapped to `colsample_bynode` rather than
  `colsample_bytree`. This is consistent with parsnip’s interface to
  `xgboost` as of parsnip 0.1.6. `colsample_bytree` can still be
  optimized by passing it in as an engine argument to
  [`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)
  ([\#60](https://github.com/tidymodels/rules/issues/60)).

- Introduced support for early stopping in
  [`rule_fit()`](https://parsnip.tidymodels.org/reference/rule_fit.html)
  via the `stop_iter` argument. See
  [`parsnip::details_rule_fit_xrf`](https://parsnip.tidymodels.org/reference/details_rule_fit_xrf.html).
  Note that this is a *main* argument to
  [`rule_fit()`](https://parsnip.tidymodels.org/reference/rule_fit.html)
  requiring parsnip 1.0.0.

## rules 0.2.0

CRAN release: 2022-03-14

- Model definition functions
  (e.g. [`cubist_rules()`](https://parsnip.tidymodels.org/reference/cubist_rules.html))
  were moved to the parsnip package.

## rules 0.1.2

CRAN release: 2021-08-07

- Maintenance release demanded by CRAN.

## rules 0.1.1

CRAN release: 2021-01-16

- Added [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
  methods for cubist and RuleFit models to get back the rules and model
  coefficients.

- Clean up
  [`tunable()`](https://generics.r-lib.org/reference/tunable.html)
  methods that define the default parameter ranges for the tuning
  parameters.

- Changes to test for cases when CRAN cannot get `xgboost` to work on
  their Solaris configuration.

- Case weights were enabled for \[C5_rules()\] and \[cubist_rules()\].

## rules 0.1.0

CRAN release: 2020-10-28

- The default encoding methods were changed such that `parsnip` will
  respect the encoding that the underlying model uses.

## rules 0.0.3

CRAN release: 2020-10-02

- Documentation changes (some demanded by CRAN).

- Changes to make `rules` work well with PSOCK cluster parallelism.

## rules 0.0.2

CRAN release: 2020-06-10

- Changes for new `dplyr` version.

## rules 0.0.1

CRAN release: 2020-05-20

- First CRAN release
