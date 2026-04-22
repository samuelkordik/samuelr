#' Opinionated Date Formatting
#'
#' My own clumsily structured formatting effort for dates
#'
#' If range is true then it prints a 2-item range as a range.
#'
#' @param dates vector of dates
#' @param format one of "abbr" or "short"
#' @param range is this a range or not?
#' @param range_collapse string for collapse of range, defaults to " to " for
#' "abbr" format and " - " for "short" format.
#'
#' @returns character vector
#' @export
#'
#' @examples
#' x <- as.Date("2025-01-01")
#' fmt_date(x, "abbr") # Jan 1, 2025
#' fmt_date(x, "short") # 1/1/2025
#'
#' a <- c(x, as.Date("2025-07-15"))
#' fmt_date(a, "abbr", range = T) # Jan 1, 2025 to Jul 15, 2025
#' fmt_date(a, "short", range=T, range_collapse = " – ") # 1/1/2025 – 7/15/2025
#'
fmt_date <- function(
    dates,
    format = c("abbr", "short"),
    range = F,
    range_collapse = NULL
) {
    print_pretty_date <- function(dates, format) {
        switch(
            format,
            abbr = {
                paste0(
                    lubridate::month(dates, label = T),
                    " ",
                    lubridate::day(dates),
                    ", ",
                    lubridate::year(dates)
                )
            },
            short = {
                paste0(
                    lubridate::month(dates),
                    "/",
                    lubridate::day(dates),
                    "/",
                    lubridate::year(dates)
                )
            }
        )
    }

    if (range) {
        if (is.null(range_collapse)) {
            if (format == "short") {
                range_collapse <- " - "
            } else {
                if (format == "abbr") {
                    range_collapse <- " to "
                } else {
                    range_collapse <- " to "
                    cli::cli_alert_warning(
                        "No value provided for range_collapse, defaulting to ' to '."
                    )
                }
            }
        }

        paste(fmt_date(dates, format), collapse = range_collapse)
    } else {
        print_pretty_date(dates, format)
    }
}
