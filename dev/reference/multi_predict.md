# `multi_predict()` methods for rule-based models

[`multi_predict()`](https://parsnip.tidymodels.org/reference/multi_predict.html)
methods for rule-based models

## Usage

``` r
# S3 method for class '`_cubist`'
multi_predict(object, new_data, type = NULL, neighbors = NULL, ...)

# S3 method for class '`_xrf`'
multi_predict(object, new_data, type = NULL, penalty = NULL, ...)
```

## Arguments

- object:

  A [model
  fit](https://parsnip.tidymodels.org/reference/model_fit.html).

- new_data:

  A rectangular data object, such as a data frame.

- type:

  A single character value or `NULL`. This argument is ignored in the
  method for `_cubist` objects and is handled internally (since
  `type = "numeric"` is always used).

- neighbors:

  A numeric vector of neighbors values between zero and nine.

- ...:

  Not currently used.

- penalty:

  Non-negative penalty values.
