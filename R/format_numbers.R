#' Number Formatting Helpers
#'
#' @description
#' These functions are helpful wrappers around existing formatting functions that
#' are designed to make it faster and easier to code number formatting.
#'
#' @details
#' Included functions:
#' - `fmt_comma` produces a prettified version of numbers with thousands separator.
#' - `fmt_compact` produces a compact version of numbers (i.e., 1000 becomes "1K").
#' - `fmt_pct` produces a percent formatted version of numbers
#'
#'
#' @param x Input number
#' @param decimals Number of decimal places (default 0)
#' @param ... Additional arguments to pass onto wrapped function
#' @inheritDotParams gt::vec_fmt_number -x
#' @name fmt
#' @returns A character vector
#'
#' @examples
#' # Here's an example numeric vector for these examples:
#' nums <- c(2032200, 80, 20500000, 984710, 4188.6, 70, 27247000, 9.994, 950, 7e+06,NA)
#'
#' # fmt_comma is just a pretty print wrapper
#' fmt_comma(nums)
#' #>  [1] "2,032,200.00"  "80.00"         "20,500,000.00" "984,710.00"    "4,188.60"
#' #   [6] "70.00"         "27,247,000.00" "9.99"          "950.00"        "7,000,000.00"
#' #   [11] "NA"
#'
#' # fmt_compact produces a compact variant (shortened, with suffixes for million,thousand, etc.)
#' fmt_compact(nums)
#' #> [1] "2.03M"   "80"      "20.5M"   "984.71K" "4.19K"   "70"      "27.25M"  "9.99"    "950"
#' #  [10] "7M"      "NA"
#'
#' # fmt_pct gives percent sign formatting:
#' fmt_pct(runif(10))
#' #> [1] "71.76%" "52.90%" "89.98%" "0.90%"  "8.16%"  "55.14%" "25.21%" "84.29%" "29.92%" "32.05%"
#'
#'
#'
#'
NULL
#> NULL

#' @rdname fmt
#' @param drop_trailing_zeros Drop zeros after decimal point (default true)
#' @export
fmt_comma <- function(x, decimals = 0, ...) {
    gt::vec_fmt_number(x, decimals, ...)
}

#' @rdname fmt
#' @export
fmt_compact <- function(x, decimals = 2, drop_trailing_zeros = TRUE, ...) {
    gt::vec_fmt_number(
        x,
        decimals,
        drop_trailing_zeros = drop_trailing_zeros,
        suffixing = TRUE
    )
}

#' @rdname fmt
#' @export
fmt_pct <- function(x, decimals = 0, ...) {
    gt::vec_fmt_percent(x, decimals, ...)
}
