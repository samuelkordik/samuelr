
<!-- README.md is generated from README.Rmd. Please edit that file -->

# samuelr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/samuelkordik/samuelr/graph/badge.svg)](https://app.codecov.io/gh/samuelkordik/samuelr)
<!-- badges: end -->

Welcome to my personal R package! This is a collection of useful tools
and utilities I use to solve common problems I encounter. Feel free to
borrow what you find beneficial.

## Installation

You can install the development version of samuelr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("samuelkordik/samuelr")
```

## Key Functions

### if_error

Function wrapper to gracefully handle errors (similar to Excel’s IFERROR
function):

``` r
library(samuelr)
log_or_na <- if_error(log) # Returns log() but if there's an error, just returns NA.
log_or_na(2)
#> [1] 0.6931472
log_or_na("A")
#> non-numeric argument to mathematical function
#> [1] NA
```

### Use Defined Columns

This is a useful function for functional programming with uncertain
data. By providing a set of defined column names with regex pattern
matching, the function identifies and renames columns in the provided
data to enable building flexible functional data.
