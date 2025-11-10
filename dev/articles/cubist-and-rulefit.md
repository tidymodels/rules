# What's the difference between Cubist and RuleFit?

[Cubist](https://www.rulequest.com/cubist-win.html) and
[RuleFit](https://statweb.stanford.edu/~jhf/R_RuleFit.html) are two
rule-based regression models. They are similar in some ways but
otherwise very different. This is a short description of their
approaches.

Cubist is for numeric outcomes while RuleFit can work with numeric and
categorical outcomes. For this document, we’ll focus on numeric outcomes
(without loss of generality).

## Both start by making rules from trees

For each model, an an initial tree-based model is created. Let’s look at
a really simple example tree:

``` r
if (A < a) {
  if (B < b) {
    node <- 1
  } else {
    node <- 2
  }
} else
  node <- 3
}
```

In this case, two predictors are used in splits: `A` and `B`. With this
model, the split to the left is the “\<” condition. There are three
terminal nodes. This tree-based structure can be converted to a set of
distinct rules. Each rule is a collection of `if/then` statements that
define the path to the terminal nodes. In this example:

``` r
rule_1 <- (A <  a) & (B <  b)
rule_2 <- (A <  a) & (B >= b)
rule_3 <- (A >= a)
```

As is, these paths through the tree are mutually exclusive. For deep
trees, these rules can become overly complex and may contain
redundancies. Cubist takes the approach of simplifying rules whenever
possible (Quinlan (1987)). For example,

``` r
rule_x <- (A <  10)  & (A <  7) & (B >= 1)
```

becomes

``` r
rule_y <- (A < 7) & (B >= 1)
```

Cubist also has an approach that can *prune* conditions inside of the
rule that simplifies the structure without degrading performance. If `B`
doesn’t matter much in the rule above, then Cubist could reduce it to
`A < 7`.

Cubist and RuleFit are also ensemble methods. For RuleFit, any tree
ensemble model could generate a set of rules. For the `xrf` package, the
`xgboost` package creates the initial set of rules. The rules create be
each tree are pooled into a broader rule set. Cubist takes a different
approach and uses model committees (see
[*APM*](http://appliedpredictivemodeling.com/) Chapter 14). This is
similar to boosting but creates a pseudo-outcome for each tree in the
ensemble. This outcome adjusts the trees based on the size of the
residual from the previous tree.

At the end of the rule generating process, the rules are unlikely to
mutually exclusive. Any data point is likely to be fall into multiple
rules.

Now let’s look at how each model uses the rules.

## Cubists makes a model for each rule

Before delving into the rules, let’s start with the *model tree*
initially created by Cubist (Quinlan (1992)). An initial tree is created
and a regression model is created for *each split in the tree*. Each
linear regression is created on the subset of the data covered by the
current rule and uses only the predictors in the current rule. In the
tree shown above, the first split produces two models with `A` as a
predictor. The data for each are filtered either with `A < a` or
`A >= a`. The entire set of models are:

``` r
split_1_low  <- filter(data, A <  a)
model_1_low  <- lm(y ~ A, data = split_1_low)

split_1_high <- filter(data, A >= a)
model_1_high <- lm(y ~ A, data = split_1_high)

split_2_low  <- filter(data, A <  a & B <  b)
model_2_low  <- lm(y ~ A + B, data = split_2_low)

split_2_high <- filter(data, A <  a & B >= b)
model_2_high <- lm(y ~ A + B, data = split_2_high)
```

Cubist does some feature selection on these models so they may not
contain all of the possible predictors. Also, since Cubist prunes the
rules, there may be not appear to be a connection between the variables
used in a rule and its corresponding model.

The models associated with the rules are actually average of many models
further up the tree. Since these are linear models, the models used by
Cubist for each rule have coefficients that are averages:

| Rule | Logic            | Blended Models                   |
|------|------------------|----------------------------------|
| 1    | `A < a & B < b`  | `model_1_low` and `model_2_low`  |
| 2    | `A < a & B >= b` | `model_1_low` and `model_2_high` |
| 2    | `A >= a`         | `model_1_high`                   |

The equations for averaging were first described in Quinlan (1992) but
the updated equation for Cubist can be found in Chapter 14 of
[*APM*](http://appliedpredictivemodeling.com/).

There is a model for each committee and rule within committee. When
predicting, a new observation is compared to the conditions in the rules
to determine which rules are active for this data point. The active
linear models predict the new sample and these predictions are averaged
to produce the final prediction value.

Perhaps unrelated to this document, which focuses on how rules are used,
Cubist also does a nearest-neighbor correction to the predicted values
(Quinlan R (1993)).

Let’s look at an example model on the Palmer penguin data:

``` r
library(rules)
#> Loading required package: parsnip
data(penguins, package = "modeldata")

cubist_fit <- 
  cubist_rules(committees = 2) |> 
  set_engine("Cubist") |> 
  fit(body_mass_g ~ ., data = penguins)
cubist_fit
#> parsnip model object
#> 
#> 
#> Call:
#> cubist.default(x = x, y = y, committees = 2)
#> 
#> Number of samples: 333 
#> Number of predictors: 6 
#> 
#> Number of committees: 2 
#> Number of rules per committee: 5, 1
```

The [`summary()`](https://rdrr.io/r/base/summary.html) function shows
the details of the rules.

``` r
summary(cubist_fit$fit)
#> 
#> Call:
#> cubist.default(x = x, y = y, committees = 2)
#> 
#> 
#> Cubist [Release 2.07 GPL Edition]  Mon Nov 10 19:17:00 2025
#> ---------------------------------
#> 
#>     Target attribute `outcome'
#> 
#> Read 333 cases (7 attributes) from undefined.data
#> 
#> Model 1:
#> 
#>   Rule 1/1: [107 cases, mean 3419.2, range 2700 to 4150, est err 208.3]
#> 
#>     if
#>  flipper_length_mm <= 202
#>  sex = female
#>     then
#>  outcome = -1068 + 108 bill_depth_mm + 10.7 flipper_length_mm
#>            + 14 bill_length_mm
#> 
#>   Rule 1/2: [92 cases, mean 3972.0, range 3250 to 4775, est err 275.6]
#> 
#>     if
#>  flipper_length_mm <= 202
#>  sex = male
#>     then
#>  outcome = 319.1 + 22.3 flipper_length_mm - 21 bill_length_mm
#>            + 12 bill_depth_mm
#> 
#>   Rule 1/3: [58 cases, mean 4679.7, range 3950 to 5200, est err 206.6]
#> 
#>     if
#>  flipper_length_mm > 202
#>  sex = female
#>     then
#>  outcome = -3923.3 + 30.4 flipper_length_mm + 136 bill_depth_mm
#>            + 5 bill_length_mm
#> 
#>   Rule 1/4: [23 cases, mean 4698.9, range 3950 to 6050, est err 275.8]
#> 
#>     if
#>  bill_depth_mm > 16.4
#>  flipper_length_mm > 202
#>     then
#>  outcome = -7845.8 + 58.6 flipper_length_mm
#> 
#>   Rule 1/5: [53 cases, mean 5475.0, range 4750 to 6300, est err 239.3]
#> 
#>     if
#>  bill_depth_mm <= 16.4
#>  sex = male
#>     then
#>  outcome = -138.7 + 46 bill_length_mm + 89 bill_depth_mm
#>            + 8.9 flipper_length_mm
#> 
#> Model 2:
#> 
#>   Rule 2/1: [333 cases, mean 4207.1, range 2700 to 6300, est err 315.4]
#> 
#>  outcome = -5815.1 + 49.7 flipper_length_mm
#> 
#> 
#> Evaluation on training data (333 cases):
#> 
#>     Average  |error|              278.7
#>     Relative |error|               0.41
#>     Correlation coefficient        0.90
#> 
#> 
#>  Attribute usage:
#>    Conds  Model
#> 
#>     47%           sex
#>     42%   100%    flipper_length_mm
#>     11%    47%    bill_depth_mm
#>            47%    bill_length_mm
#> 
#> 
#> Time: 0.0 secs
```

Note that the only rule in the second committee has no conditions; all
data points being predicted are affected by that rule. Also, it is
possible for the linear model within a rule to only contain an
intercept.

The [`tidy()`](https://generics.r-lib.org/reference/tidy.html) function
can extract the rule and model data:

``` r
cb_res <- tidy(cubist_fit)
cb_res
#> # A tibble: 6 × 5
#>   committee rule_num rule                            estimate statistic
#>       <int>    <int> <chr>                           <list>   <list>   
#> 1         1        1 ( sex == 'female' ) & ( flippe… <tibble> <tibble> 
#> 2         1        2 ( sex == 'male' ) & ( flipper_… <tibble> <tibble> 
#> 3         1        3 ( flipper_length_mm > 202 ) & … <tibble> <tibble> 
#> 4         1        4 ( flipper_length_mm > 202 ) & … <tibble> <tibble> 
#> 5         1        5 ( bill_depth_mm <= 16.4 ) & ( … <tibble> <tibble> 
#> 6         2        1 <no conditions>                 <tibble> <tibble>
```

The `estimate` and `statistic` columns contain tibbles with parameter
estimates and rule statistics, respectively. They can be easily expanded
using [`unnest()`](https://tidyr.tidyverse.org/reference/unnest.html):

``` r
library(tidyr)
cb_res |> 
  dplyr::select(committee, rule_num, statistic) |> 
  unnest(cols = c(statistic))
#> # A tibble: 6 × 8
#>   committee rule_num num_conditions coverage  mean   min   max error
#>       <int>    <int>          <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1         1        1              2      107 3419.  2700  4150  208.
#> 2         1        2              2       92 3972   3250  4775  276.
#> 3         1        3              2       58 4680.  3950  5200  207.
#> 4         1        4              2       23 4699.  3950  6050  276.
#> 5         1        5              2       53 5475   4750  6300  239.
#> 6         2        1              0      333 4207.  2700  6300  315.
```

For the first committee, rule four contains two conditions and covers 23
data points from the original data set. We can find these data points by
converting the character string for the rule to an R expression, then
evaluate the expression on the data set:

``` r
library(dplyr)
library(purrr)
library(rlang)

rule_4_filter <- 
  cb_res |> 
  dplyr::filter(rule_num == 4) |> 
  pluck("rule") |>   # <- character string
  parse_expr() |>    # <- R expression
  eval_tidy(penguins) # <- logical vector

penguins |> 
  dplyr::slice(which(rule_4_filter))
#> # A tibble: 23 × 7
#>    species island    bill_length_mm bill_depth_mm flipper_length_mm
#>    <fct>   <fct>              <dbl>         <dbl>             <int>
#>  1 Adelie  Dream               41.1          18.1               205
#>  2 Adelie  Dream               40.8          18.9               208
#>  3 Adelie  Biscoe              41            20                 203
#>  4 Adelie  Torgersen           44.1          18                 210
#>  5 Gentoo  Biscoe              59.6          17                 230
#>  6 Gentoo  Biscoe              44.4          17.3               219
#>  7 Gentoo  Biscoe              49.8          16.8               230
#>  8 Gentoo  Biscoe              50.8          17.3               228
#>  9 Gentoo  Biscoe              52.1          17                 230
#> 10 Gentoo  Biscoe              52.2          17.1               228
#> # ℹ 13 more rows
#> # ℹ 2 more variables: body_mass_g <int>, sex <fct>
```

## RuleFit makes one model with rule predictors

RuleFit uses rules in a more straightforward way. It creates an initial
tree (or ensemble of trees) from the data. Rules are extracted from this
initial model and converted to a set of binary features. These features
are then added to a regularized regression model (along with the
original columns). For example:

``` r
data_with_rules <- 
  data |> 
  mutate(
    rule_1 = ifelse(A <  a & B <  b, 0, 1),
    rule_2 = ifelse(A <  a & B >= b, 0, 1),
    rule_2 = ifelse(A >= a,          0, 1)
  )

rule_fit_model <- 
  glmnet(x = data_with_rules |> select(A, B, starts_with("rule_") |> as.matrix(),
         y = data_with_rules$y,
         alpha = 1)
```

It is common to use a lasso model to regularize the model and eliminate
non-informative features.

While the details can depend on the implementation, the rules do not
appear to be pruned or simplified.

The tuning parameters for this model inherits the parameter of the
tree-based model as well as the amount of regularization used in the
`glmnet` model. The complexity of the rules is determined by the allowed
depth of the tree. For example, using a depth of four means that each
rule may have up to four terms that define it. The number of rules would
be primarily determined by the number of boosting iterations.

RuleFit, as implemented by the `xrf` package, required all of the data
to be complete (i.e, non-missing). In the original data set, the body
mass is encoded as integer and `xrf` requires a double, so:

``` r
penguins <- 
  penguins |> 
  mutate(body_mass_g = body_mass_g + 0.0) |> 
  na.omit()
```

Fitting the model:

``` r
rule_fit_spec <- 
  rule_fit(trees = 10, tree_depth = 5, penalty = 0.01) |> 
  set_engine("xrf") |>
  set_mode("regression") 

rule_fit_fit <- 
  rule_fit_spec |> 
  fit(body_mass_g ~ ., data = penguins)
```

``` r
rule_fit_fit
#> parsnip model object
#> 
#> An eXtreme RuleFit model of 112 rules.
#> 
#> Original Formula:
#> 
#> body_mass_g ~ species + island + bill_length_mm + bill_depth_mm + [truncated]
```

To get the rules and their associated coefficients, the
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) method can be
used again:

``` r
rf_res <- tidy(rule_fit_fit, penalty = 0.01)
rf_res
#> # A tibble: 110 × 3
#>    rule_id           rule                                      estimate
#>    <chr>             <chr>                                        <dbl>
#>  1 (Intercept)       ( TRUE )                                   5753.  
#>  2 bill_depth_mm     ( bill_depth_mm )                           -15.9 
#>  3 bill_length_mm    ( bill_length_mm )                            1.10
#>  4 flipper_length_mm ( flipper_length_mm )                        -7.67
#>  5 islandDream       ( island == 'Dream' )                       -29.2 
#>  6 islandTorgersen   ( island == 'Torgersen' )                   -16.8 
#>  7 r0_2              ( species == 'Gentoo' )                     472.  
#>  8 r0_3              ( sex != 'male' ) & ( species != 'Gentoo…   -78.8 
#>  9 r1_2              ( flipper_length_mm >= 211.5 )              219.  
#> 10 r1_3              ( flipper_length_mm <  194.5 ) & ( flipp…  -142.  
#> # ℹ 100 more rows
```

Note that the units of each predictor have been scaled to be between
zero and one.

Looking at the rules, there are examples of some rules, such as:

``` r
( bill_depth_mm >= 14.1499996 ) & 
( flipper_length_mm <  227 ) & 
( flipper_length_mm <  228.5 ) & 
( flipper_length_mm >= 197.5 ) & 
( flipper_length_mm >= 224.5 )"
```

which can be simplified to have fewer conditions:

``` r
( bill_depth_mm >= 14.1499996 ) & 
( flipper_length_mm <  227 ) & 
( flipper_length_mm >= 197.5 ) & 
```

Also, the [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
function can extract the same information but use the original
predictors as the unit:

``` r
rf_variable_res <- tidy(rule_fit_fit, unit = "columns", penalty = 0.01)
rf_variable_res
#> # A tibble: 452 × 3
#>    rule_id term              estimate
#>    <chr>   <chr>                <dbl>
#>  1 r0_3    species             -78.8 
#>  2 r0_2    species             472.  
#>  3 r0_3    sex                 -78.8 
#>  4 r1_3    flipper_length_mm  -142.  
#>  5 r1_2    flipper_length_mm   219.  
#>  6 r1_3    flipper_length_mm  -142.  
#>  7 r2_7    flipper_length_mm     9.04
#>  8 r2_7    sex                   9.04
#>  9 r2_7    species               9.04
#> 10 r3_3    flipper_length_mm  -101.  
#> # ℹ 442 more rows
```

A single rule might be represented with multiple rows of this version of
the data. These results can also be used to compute a rough estimate or
variable importance using the absolute value of the coefficients:

``` r
num_rules <- sum(grepl("^r[0-9]*_", unique(rf_res$rule_id))) + 1

rf_variable_res |> 
  dplyr::filter(term != "(Intercept)") |> 
  group_by(term) |> 
  summarize(effect = sum(abs(estimate)), .groups = "drop") |> 
  ungroup() |> 
  # normalize by number of possible occurrences
  mutate(effect = effect / num_rules ) |> 
  arrange(desc(effect))
#> # A tibble: 6 × 2
#>   term              effect
#>   <chr>              <dbl>
#> 1 flipper_length_mm 219.  
#> 2 bill_depth_mm     175.  
#> 3 bill_length_mm    175.  
#> 4 species            69.5 
#> 5 sex                52.0 
#> 6 island              9.84
```

## References

- Quinlan R (1987). “Simplifying Decision Trees.” *International Journal
  of Man-Machine Studies*, 27(3), 221-234.

- Quinlan R (1992). “Learning with Continuous Classes.” *Proceedings of
  the 5th Australian Joint Conference On Artificial Intelligence*,
  pp. 343-348.

- Quinlan R (1993). “Combining Instance-Based and Model-Based Learning.”
  *Proceedings of the Tenth International Conference on Machine
  Learning*, pp. 236-243.
