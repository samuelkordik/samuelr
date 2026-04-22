
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

### Formatting functions

`fmt_date()` is a helper function to make vectorized date formatting
slightly smoother. This allows for two simple formats: “abbr” which
yields “Jan 1, 2025”, and “short” which creates “1/1/2025”. It also
supports printing two-value vectors as a range, i.e., “1/1/2025 -
7/15/2025”, or “Jan 1, 2025 to Jul 15, 2025”.

`fmt_comma()` produces comma-separated numbers, optionally with some
number of decimals.

`fmt_compact()` produces a compact version of numbers (e.g., 1,000
becomes 1K)

`fmt_pct()` produces percent formatted nubers. Defaults to two decimal
places.

``` r
library(samuelr)
nums <- c(2032200, 80, 20500000, 984710, 4188.6, 70, 27247000, 9.994, 950, 7e+06,NA)

list(
    dates = fmt_date(as.Date("2025-01-01"), format = "abbr"),
    comma = fmt_comma(nums),
    compact = fmt_compact(nums),
    pcts = fmt_pct(runif(10))
)
#> $dates
#> [1] "Jan 1, 2025"
#> 
#> $comma
#>  [1] "2,032,200"  "80"         "20,500,000" "984,710"    "4,189"     
#>  [6] "70"         "27,247,000" "10"         "950"        "7,000,000" 
#> [11] "NA"        
#> 
#> $compact
#>  [1] "2.03M"   "80"      "20.5M"   "984.71K" "4.19K"   "70"      "27.25M" 
#>  [8] "9.99"    "950"     "7M"      "NA"     
#> 
#> $pcts
#>  [1] "15%" "79%" "96%" "43%" "95%" "29%" "64%" "92%" "90%" "32%"
```

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
