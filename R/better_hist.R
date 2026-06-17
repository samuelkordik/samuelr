#' A Better Histogram
#'
#' An opinionated histogram, using improved binwidth calculations and
#' embracing an opinionated sense of style.
#'
#' Calculates binwidth using Freedman-Diaconis method.
#'
#' @param .data dataframe
#' @param aes_x aesthestics x argument for `ggplot()`
#' @param label_args Arguments for `labs`
#' @param ... Additional arguments passed to `geom_histogram`
#'
#' @returns ggplot2 plot object
#' @export
#'
better_hist <- function(.data,
                    aes_x,
                    label_args=list(),
                    add_mean = TRUE,
                    add_sd = NULL,
                    add_median = NULL,
                    add_min = NULL,
                    add_max = NULL,
                    add_density = FALSE,
                    lower_bound = NULL,
                    upper_bound = NULL,
                    ...
) {
  if (is.null(label_args$title))
    label_args$title <- paste0(rlang::as_string(rlang::expr(.data)), " Histogram")

  if (is.null(label_args$y))
    label_args$y <- "count"

  aes_x <- rlang::enquo(aes_x)

  # Calculate summary stats
  x_vals <- .data[[rlang::as_name(aes_x)]]
  x_vals <- x_vals[!is.na(x_vals)]
  mu <- mean(x_vals, na.rm=TRUE)
  sigma  <- sd(x_vals, na.rm=TRUE)
  med <- median(x_vals, na.rm=TRUE)

  # Calculate binwidth
  fd_binwidth <- 2 * IQR(x_vals, na.rm=TRUE) / length(x_vals)^(1/3)

  # Get density values
  density_data <- density(.data[[rlang::as_name(aes_x)]])
  max_density <- max(density_data$y)

  hist_info <- ggplot2::ggplot_build(
    ggplot2::ggplot(.data, ggplot2::aes(x = !!aes_x)) +
      ggplot2::geom_histogram(binwidth = fd_binwidth)
  )$data[[1]]

  max_count <- max(hist_info$count)
  scale_factor <- 0.7*max_count/max_density
  message(paste0("scale_factor = 0.7*",max_count,"/",round(max_density,4),"= ", round(scale_factor,4)))

  # Set up colors
  hist_fill <- dfr_colors$omd_navy
  #hist_lines <- ifelse(add_density, dfr_colors$dark_navy, "white")
  hist_lines <- "white"
  stat_lines <- dfr_colors$aqua

  # Get names
  yaxis <- ifelse(is.null(label_args$y), "Count", label_args$y)
  secaxis <- ifelse(is.null(label_args[["sec"]]), "Density", label_args[["sec"]])

  # Set bounds
  if (is.null(lower_bound)) lower_bound <- 0

  if (is.null(upper_bound)) {
    upper_bound <- quantile(x_vals, 0.999, na.rm=TRUE)
    message("Setting upper bound at 0.9999 quantile.")
  }

  message(paste0("Limiting x axis to (", lower_bound, ", ", upper_bound, ")"))

  # Annotations
  locate_y_height <- function(x) {
    hist_info[hist_info$xmin <= x & hist_info$xmax > x,]["y"]
  }

  max_label <- pmin(upper_bound,
                   hist_info[length(hist_info)-1,]$ymax + 3)

  annotations <- tibble::tibble(
    x = numeric(),
    y = numeric(),
    label = character()
  )

  if (!is.null(add_min)) {
    annotations |>
      tibble::add_row(x = round(min(x_vals),2),
              y = hist_info[1,]$ymax + 3,
              label = "Min:") -> annotations
  }

  if (!is.null(add_max)) {
    annotations |>
      tibble::add_row(x = round(max(x_vals), 2),
              y = hist_info[length(hist_info)-1,]$ymax + 10,
              label = "Max:") -> annotations
  }

  if (!is.null(add_median)) {
    annotations |>
      tibble::add_row(x = round(med, 2),
              y = locate_y_height(med) |> dplyr::pull("y")+5,
              label = "Median:") -> annotations
  }

  if (!is.null(add_mean)) {
    annotations |>
      tibble::add_row(x = round(mu, 2),
              y = locate_y_height(mu) |> dplyr::pull("y")+ 5,
              label = "Mean:") -> annotations
  }

  if (!is.null(add_sd)) {
    for (sd_x in add_sd) {
      xsd <- mu - sd_x * sigma
      xsdz <- mu + sd_x * sigma
      annotations |> tibble::add_row(
        x = xsd,
        y = locate_y_height(xsd) |> dplyr::pull(y) + 5,
        label = paste0("-", sd_x, "𝜎")
      ) |> tibble::add_row(
        x = xsdz,
        y = locate_y_height(xsd) |> dplyr::pull(y) + 5,
        label = paste0(sd_x, "𝜎")
      ) -> annotations
    }
  }


  .data |>
    ggplot2::ggplot(ggplot2::aes(x = !!aes_x)) +
    ggplot2::geom_histogram(fill = hist_fill, color = hist_lines, binwidth = fd_binwidth,
                            ...) +
    hrbrthemes::theme_ipsum_rc(grid="Y") -> p

  if (add_density) {

    p <- p +
      ggplot2::geom_density(ggplot2::aes(y = ..density.. * scale_factor),
                   fill = dfr_colors$yellow,
                   color = dfr_colors$omd_navy,
                   alpha = 0.5) +
      hrbrthemes::scale_y_comma(name = yaxis,
                    sec.axis = ggplot2::sec_axis(~ . / scale_factor, name = secaxis),
                    expand = ggplot2::expansion(mult = c(0, 0.1)))

  } else {
    p <- p + hrbrthemes::scale_y_comma(expand = ggplot2::expansion(mult = c(0, 0.1)))
  }

  if (length(label_args) > 0) {
    p <- p + do.call(ggplot2::labs, label_args)
  }

  if (add_mean) {
    p <- p + ggplot2::geom_vline(xintercept = mu, color = stat_lines, linewidth=1)
  }

  if (!is.null(add_median)) {
    p <- p + ggplot2::geom_vline(xintercept = med, color = stat_lines, linewidth=1)
  }

  if (!is.null(add_sd)) {
    for (sd_x in add_sd) {
      p <- p +
        ggplot2::geom_vline(xintercept = mu - sd_x * sigma,
                   color = stat_lines, linewidth=.9, linetype = "dashed") +
        ggplot2::geom_vline(xintercept = mu + sd_x * sigma,
                   color = stat_lines, linewidth=.9, linetype = "dashed")
    }
  }


  p <- p + ggplot2::xlim(lower_bound, upper_bound) +
    ggplot2::geom_label(data = annotations,
              ggplot2::aes(x = x, y = y, label = paste(label, x)),
              fontface = "bold")

  p



}
