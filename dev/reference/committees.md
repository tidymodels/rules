# Parameter functions for Cubist models

Committee-based models enact a boosting-like procedure to produce
ensembles. `committees` parameter is for the number of models in the
ensembles while `max_rules` can be used to limit the number of possible
rules.

## Usage

``` r
committees(range = c(1L, 100L), trans = NULL)

max_rules(range = c(1L, 500L), trans = NULL)
```

## Arguments

- range:

  A two-element vector holding the *defaults* for the smallest and
  largest possible values, respectively.

- trans:

  A `trans` object from the `scales` package, such as
  [`scales::log10_trans()`](https://scales.r-lib.org/reference/transform_log.html)
  or
  [`scales::reciprocal_trans()`](https://scales.r-lib.org/reference/transform_reciprocal.html).
  If not provided, the default is used which matches the units used in
  `range`. If no transformation, `NULL`.

## Value

A function with classes "quant_param" and "param"

## Examples

``` r
committees()
#> # Committees (quantitative)
#> Range: [1, 100]
committees(4:5)
#> # Committees (quantitative)
#> Range: [4, 5]

max_rules()
#> Max. Rules (quantitative)
#> Range: [1, 500]
```
