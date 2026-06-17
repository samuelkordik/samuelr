#' Styled horizontal bar chart
#'
#' A ggplot2 wrapper that produces a consistently-styled horizontal bar chart
#' using the `hrbrthemes::theme_ipsum_rc()` theme `dfr_cols("omd_navy")` as the default fill colour. Supports both pre-computed values
#' (`x` + `y`) and count-based bars (`x` or `y` alone), optional fill mapping,
#' and auto-positioned bar labels.
#'
#' @param .data A data frame.
#' @param x <[`data-masking`][rlang::args_data_masking]> Bare column name for
#'   the x aesthetic. Required unless `y` is the only positional aesthetic.
#'   When both `x` and `y` are supplied, `stat = "identity"` is used (values
#'   are pre-computed); when only `x` is supplied, `geom_bar()` counts rows.
#' @param y <[`data-masking`][rlang::args_data_masking]> Bare column name for
#'   the y aesthetic. Optional; see `x` for stat behaviour.
#' @param fill <[`data-masking`][rlang::args_data_masking]> Optional bare
#'   column name to map to the fill aesthetic.  When omitted, bars are filled
#'   with the fixed navy colour. When supplied, `hrbrthemes::scale_fill_ipsum()`
#'   is applied.
#' @param label <[`data-masking`][rlang::args_data_masking]> Optional bare
#'   column name to use as bar labels. Labels are automatically positioned
#'   inside the bar (white, right-aligned) when the bar is wide enough to fit
#'   the text, otherwise outside (black, left-aligned). Positioning uses a
#'   linear scale by default and a log-space heuristic when `log_x = TRUE`.
#' @param title Plot title. Passed to [ggplot2::labs()]. Defaults to
#'   `ggplot2::waiver()` (no title).
#' @param subtitle Plot subtitle. Defaults to `ggplot2::waiver()`.
#' @param xlab X-axis label. Defaults to `ggplot2::waiver()` (column name).
#' @param ylab Y-axis label. Defaults to `ggplot2::waiver()` (column name).
#' @param caption Caption below the plot. Defaults to `ggplot2::waiver()`.
#' @param legend.position Legend position string passed to
#'   [ggplot2::theme()]. Default `"none"`.
#' @param log_x Logical. If `TRUE`, assumes the x-axis is on a log10 scale and
#'   uses multiplicative nudging for label placement. Default `FALSE`.
#' @param align_title "panel" aligns the title to the left edge of the graph,
#'   "plot" aligns the title to the left edge of the y-axis labels. Defaults "plot".
#' @param ... Additional arguments passed to [hrbrthemes::theme_ipsum_rc()]
#'
#' @returns A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' mtcars |>
#'     bar_graph(y = as.character(cyl),
#'               title = "Number of Vehicles by Cylinder Count",
#'               xlab = "Number of Vehicles", ylab = "Number of Cylinders"
#'               )
#' }

bar_graph <- function(
    .data,
    x = NULL,
    y = NULL,
    fill = NULL,
    label = NULL,
    title = ggplot2::waiver(),
    subtitle = ggplot2::waiver(),
    xlab = ggplot2::waiver(),
    ylab = ggplot2::waiver(),
    caption = ggplot2::waiver(),
    legend.position = "none",
    log_x = FALSE,
    align_title = c("panel, plot"),
    ...
) {
    # Capture missing status before entering any other function — missing() only
    # works correctly when called directly in the function body.
    has_x <- !missing(x)
    has_y <- !missing(y)
    has_fill <- !missing(fill)
    has_label <- !missing(label)

    if (!has_x && !has_y) {
        stop("Either x or y must be supplied")
    }

    # Build aes mapping using {{ }} (embrace) so column names are passed through
    # tidy-eval rather than as literal strings.
    mapping <- if (has_x && has_y && has_fill) {
        ggplot2::aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }})
    } else if (has_x && has_y) {
        ggplot2::aes(x = {{ x }}, y = {{ y }})
    } else if (has_y && has_fill) {
        ggplot2::aes(y = {{ y }}, fill = {{ fill }})
    } else if (has_x && has_fill) {
        ggplot2::aes(x = {{ x }}, fill = {{ fill }})
    } else if (has_y) {
        ggplot2::aes(y = {{ y }})
    } else {
        ggplot2::aes(x = {{ x }})
    }

    # Build geom — stat = "identity" required when both x and y are pre-computed.
    # Fixed colour only when fill is not mapped to a column.
    gbars <- if (has_x && has_y) {
        if (has_fill) {
            ggplot2::geom_bar(stat = "identity")
        } else {
            ggplot2::geom_bar(stat = "identity", fill = dfr_cols("omd_navy"))
        }
    } else {
        if (has_fill) {
            ggplot2::geom_bar()
        } else {
            ggplot2::geom_bar(fill = "#00286F")
        }
    }

    # Only apply the discrete fill scale when fill is actually mapped.
    fill_scale <- if (has_fill) hrbrthemes::scale_fill_ipsum() else NULL

    # ── Label layer ──────────────────────────────────────────────────────────────
    # Position logic: a label "fits inside" a bar when the bar is wider than the
    # estimated text width.  Estimate: assume ~60 characters span the full axis.
    #
    # Linear scale: one character = max(x) / 60 data units; gap is additive.
    # Log scale:    one character = log10(max(x)) / 60 log-units; bar width is
    #   log10(x) (axis assumed to start near 1); gap is multiplicative so that
    #   label positions move by a constant fraction of a decade rather than a
    #   fixed data-unit offset.
    #
    # Inside → white, right-aligned (hjust = 1), nudged just inside the bar end.
    # Outside → black, left-aligned (hjust = 0), nudged just past the bar end.
    glabels <- NULL
    colour_scale <- NULL

    if (has_label) {
        x_vals <- dplyr::pull(.data, {{ x }})
        label_vals <- as.character(dplyr::pull(.data, {{ label }}))

        # Max character width per label, handling multi-line labels split by \n
        max_chars <- vapply(
            label_vals,
            function(l) {
                max(nchar(strsplit(l, "\n", fixed = TRUE)[[1]]))
            },
            integer(1)
        )

        if (log_x) {
            # Work in log10 space: bar width = log10(x), axis spans 0..log10(max).
            log_max <- log10(max(x_vals, na.rm = TRUE))
            char_width <- log_max / 60
            inside <- log10(x_vals) > max_chars * char_width
            # gap_factor: multiply/divide to nudge by 1% of the log axis span
            gap_factor <- 10^(log_max * 0.01)
            .data[[".label_x"]] <- ifelse(
                inside,
                x_vals / gap_factor,
                x_vals * gap_factor
            )
        } else {
            char_width <- max(x_vals, na.rm = TRUE) / 60
            inside <- x_vals > max_chars * char_width
            gap <- max(x_vals, na.rm = TRUE) * 0.01
            .data[[".label_x"]] <- ifelse(inside, x_vals - gap, x_vals + gap)
        }

        # Inject positioning columns directly — avoids tidy-eval complications
        # inside the mutate call when mixing quoted and unquoted column refs.
        .data[[".label_hjust"]] <- ifelse(inside, 1, 0)
        .data[[".label_color"]] <- ifelse(inside, "white", "black")

        glabels <- ggplot2::geom_text(
            ggplot2::aes(
                x = .label_x,
                label = {{ label }},
                hjust = .label_hjust,
                colour = .label_color
            ),
            vjust = 0.5,
            size = 4,
            lineheight = 0.9,
            show.legend = FALSE
        )
        # .label_color holds literal colour strings; identity scale prevents ggplot2
        # from treating them as factor levels to be mapped to a palette.
        colour_scale <- ggplot2::scale_colour_identity()
    }

    (.data |>
        ggplot2::ggplot(mapping) +
        gbars +
        glabels +
        hrbrthemes::theme_ipsum_rc(...) +
        fill_scale +
        colour_scale) -> plot_out

    if (missing(align_title)) {
        align_title <- "plot"
    } else if (!(align_title %in% c("panel", "plot"))) {
        cli::cli_alert_warning(
            "`align_title` improperly set, defaulting to 'plot'"
        )
        align_title <- "plot"
    }

    if (align_title == "panel") {
        plot_out +
            ggplot2::labs(
                title = title,
                subtitle = subtitle,
                x = xlab,
                y = ylab,
                caption = caption
            ) +
            ggplot2::theme(legend.position = legend.position) -> plot_out
    } else {
        plot_out +
            ggplot2::labs(
                x = xlab,
                y = ylab
            ) +
            ggplot2::theme(legend.position = legend.position) -> plot_out

        plot_out +
            patchwork::plot_annotation(
                title = title,
                subtitle = subtitle,
                caption = caption,
                theme = hrbrthemes::theme_ipsum_rc()
            )
    }
}
