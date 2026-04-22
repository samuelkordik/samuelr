#' Returns Tukey Outlier Fences for vector
#'
#' Calculates outlier fences using the Tukey formula:
#' [Q_1 - k(Q_3 - Q_1) ), ( Q_3 + k(Q_3 - Q_1) ],
#'
#' with k values for 1.5 (outliers) and 3 (extreme outliers)
#'
#' @param x numeric vector
#'
#' @returns list of upper and lower fences for k=1.5 and k=3
#' @export
#'
#' @examples
#' x <- runif(50, 0, 500)
#' tukey_fences(x)
tukey_fences <- function(x) {
    stopifnot(is.numeric(x))

    q_x <- quantile(x, c(0.25, 0.5, 0.75))
    names(q_x) <- NULL
    iqr_x <- q_x[3] - q_x[1]

    list(
        tukey_lower = q_x[1] - 1.5 * iqr_x,
        tukey_upper = q_x[3] + 1.5 * iqr_x,
        tukey_extreme_lower = q_x[1] - 3 * iqr_x,
        tukey_extreme_upper = q_x[3] + 3 * iqr_x
    )
}

#' Label Tukey Outlier Values
#'
#' Using tukey outlier fences, labels outlier values
#' as high, low, extremely high, or extremely low.
#'
#' @param x numeric vector
#'
#' @returns factor vector
#' @export
#'
label_tukey_outliers <- function(x) {
    fences <- tukey_fences(x)

    out <- dplyr::case_when(
        x > fences$tukey_extreme_upper ~ "Extreme High Outlier",
        x > fences$tukey_upper ~ "High Outlier",
        x < fences$tukey_lower ~ "Low Outlier",
        x < fences$tukey_extreme_lower ~ "Extreme Low Outlier",
        TRUE ~ "Not Outlier"
    )

    forcats::fct(
        out,
        levels = c(
            "Extreme Low Outlier",
            "Low Outlier",
            "Not Outlier",
            "High Outlier",
            "Extreme High Outlier"
        )
    )
}
