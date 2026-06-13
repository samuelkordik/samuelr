#' Styled histogram
#'
#' A ggplot2 wrapper that produces a consistently-styled histogram using the
#' `hrbrthemes::theme_ipsum_rc()` theme and `dfr_cols("omd_navy")` as the default
#' fill colour. Supports optional fill mapping and log-scaled x-axis.
#'
#' @param .data A data frame.
#' @param x <[`data-masking`][rlang::args_data_masking]> Bare column name for
#'   the x aesthetic (required).
#' @param fill <[`data-masking`][rlang::args_data_masking]> Optional bare
#'   column name to map to the fill aesthetic. When omitted, bars are filled
#'   with the fixed navy colour. When supplied,
#'   `hrbrthemes::scale_fill_ipsum()` is applied.
#' @param log_x Logical. If `TRUE`, applies `ggplot2::scale_x_log10()` with
#'   log-spaced breaks and comma labels. Default `FALSE` (linear scale with
#'   `hrbrthemes::scale_x_comma()`).
#' @param bins Integer. Number of histogram bins. Passed to
#'   [ggplot2::geom_histogram()]. Default `50`.
#' @param title Plot title. Passed to [ggplot2::labs()]. Defaults to
#'   `ggplot2::waiver()` (no title).
#' @param subtitle Plot subtitle. Defaults to `ggplot2::waiver()`.
#' @param xlab X-axis label. Defaults to `ggplot2::waiver()` (column name).
#' @param ylab Y-axis label. Defaults to `ggplot2::waiver()` (column name).
#' @param caption Caption below the plot. Defaults to `ggplot2::waiver()`.
#' @param legend.position Legend position string passed to
#'   [ggplot2::theme()]. Default `"none"`.
#'
#' @returns A `ggplot` object.
#' @export
#'
#' @examples
#' data.frame(
#'     id = replicate(
#'        500,
#'        paste(sample(letters, 3, replace = TRUE), collapse=""),
#'        simplify=T),
#'      n = rnorm(500, mean=50, sd = 10)
#' ) |>
#' hist_graph(n, title = "Normal Distribution")
#'
hist_graph <- function(
    .data,
    x,
    fill = NULL,
    log_x = FALSE,
    bins = 50,
    title = ggplot2::waiver(),
    subtitle = ggplot2::waiver(),
    xlab = ggplot2::waiver(),
    ylab = ggplot2::waiver(),
    caption = ggplot2::waiver(),
    legend.position = "none"
) {
    # Capture missing status before entering any other function — missing() only
    # works correctly when called directly in the function body.
    has_fill <- !missing(fill)

    # Build aes mapping using {{ }} (embrace) so column names pass through
    # tidy-eval rather than as literal strings.
    mapping <- if (has_fill) {
        ggplot2::aes(x = {{ x }}, fill = {{ fill }})
    } else {
        ggplot2::aes(x = {{ x }})
    }

    # Build geom — fixed navy fill when fill is not mapped to a column.
    ghist <- if (has_fill) {
        ggplot2::geom_histogram(color = "white", bins = bins)
    } else {
        ggplot2::geom_histogram(
            color = "white",
            fill = dfr_cols("omd_navy"),
            bins = bins
        )
    }

    # Only apply the discrete fill scale when fill is actually mapped.
    fill_scale <- if (has_fill) hrbrthemes::scale_fill_ipsum() else NULL

    # Log-transform the x-axis with log-spaced breaks and comma labels when
    # requested; otherwise use hrbrthemes comma formatting.
    x_scale <- if (log_x) {
        ggplot2::scale_x_log10(
            labels = scales::label_comma(),
            breaks = scales::breaks_log()
        )
    } else {
        hrbrthemes::scale_x_comma()
    }

    .data |>
        ggplot2::ggplot(mapping) +
        ghist +
        hrbrthemes::theme_ipsum_rc(grid = "Y") +
        fill_scale +
        x_scale +
        hrbrthemes::scale_y_comma(
            expand = ggplot2::expansion(mult = c(0, 0.1))
        ) +
        ggplot2::labs(
            title = title,
            subtitle = subtitle,
            x = xlab,
            y = ylab,
            caption = caption
        ) +
        ggplot2::theme(legend.position = legend.position)
}
