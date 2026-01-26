# Internal function wrappers

These are not supported when called by the user.

## Usage

``` r
c5_fit(x, y, trials = 1, minCases = 2, cost = NULL, ...)

c5_pred(object, new_data, trials = object$fit$trials["Actual"], ...)

# S3 method for class 'C5_rules'
tunable(x, ...)

cubist_fit(x, y, committees = 1, neighbors = 0, max_rules = NA, ...)

get_neighbors(x)

# S3 method for class 'cubist_rules'
tunable(x, ...)

xrf_fit(
  formula,
  data,
  max_depth = 6,
  nrounds = 15,
  eta = 0.3,
  colsample_bynode = NULL,
  colsample_bytree = NULL,
  min_child_weight = 1,
  gamma = 0,
  subsample = 1,
  validation = 0,
  early_stop = NULL,
  counts = TRUE,
  event_level = c("first", "second"),
  lambda = 0.1,
  objective = NULL,
  ...
)

xrf_pred(object, new_data, lambda = object$fit$lambda, type, ...)

# S3 method for class 'rule_fit'
tunable(x, ...)
```
