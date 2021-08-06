
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rules

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/tidymodels/rules/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/rules/actions)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/rules/branch/master/graph/badge.svg)](https://codecov.io/gh/tidymodels/rules?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/rules)](https://cran.r-project.org/package=rules)
<!-- badges: end -->

## Introduction

rules is a “[parsnip](https://parsnip.tidymodels.org/)-adjacent” package
with model definitions for different rule-based models, including:

  - cubist models that have discrete rule sets that contain linear
    models with an ensemble method similar to boosting
  - classification rules where a ruleset is derived from an initial tree
    fit
  - *rule-fit* models that begin with rules extracted from a tree
    ensemble which are then added to a regularized linear or logistic
    regression.

## Installation

You can install the released version of rules from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rules")
```

Install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("tidymodels/rules")
```

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

  - For questions and discussions about tidymodels packages, modeling,
    and machine learning, please [post on RStudio
    Community](https://community.rstudio.com/new-topic?category_id=15&tags=tidymodels,question).

  - If you think you have encountered a bug, please [submit an
    issue](https://github.com/tidymodels/rules/issues).

  - Either way, learn how to create and share a
    [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
    (a minimal, reproducible example), to clearly communicate about your
    code.

  - Check out further details on [contributing guidelines for tidymodels
    packages](https://www.tidymodels.org/contribute/) and [how to get
    help](https://www.tidymodels.org/help/).
