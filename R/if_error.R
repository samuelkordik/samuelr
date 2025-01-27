#' Wrap Function to Return Value If Error Occurs
#'
#' @param .f Function to Wrap
#' @param .on_error Value to return if error occurs (defaults to `NA`)
#' @param .verbose Whether or not to pass on error messages.
#'
#' @returns Either function output (on success) or .on_error (if error occurs)
#' @export
#'
#' @examples
#'
if_error <- function(.f, .on_error = NA, .verbose = TRUE) {
library(attempt)
    .f <- rlang::as_function(.f)

    function(...) {
      tryCatch(.f(...),
               error = function(cnd) {
                 if(.verbose) message(cnd$message)
                 return(.on_error)
               }
               )
    }
}
