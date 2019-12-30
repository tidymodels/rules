# rules

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/rules)](https://cran.r-project.org/package=rules)
[![R build status](https://github.com/tidymodels/rules/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/rules)
<!-- badges: end -->

`rules` is a "`parsnip`-adjacent" packages with model definitions for different rule-based models, including:

 * cubist models that have discrete rule sets that contain linear models with an ensemble method similar to boosting
 * classification rules where a ruleset is derived from an initial tree fit
 * _rule-fit_ models that begin with rules extracted from a tree ensemble which are then added to a regularized linear or logistic regression. 

## Installation

Th package is not yet on CRAN and can be installed via: 

``` r
# install.packages("devtools")
devtools::install_github("tidymodels/rules")
```

