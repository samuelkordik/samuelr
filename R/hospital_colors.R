# Hospital-system corporate branding colors, mirrored from my Tableau custom
# palettes (~/Documents/My Tableau Repository/Preferences.tps). Unlike the DFR
# palette (an interpolating ramp), these are a categorical brand mapping: each
# receiving-facility system always gets its own corporate color, so the scales
# below are built on scale_*_manual() keyed by system name.

hospital_colors <- c(
    "BUMC" = "#F5B946",
    "Parkland" = "#B02AA5",
    "Medical City Plano" = "#871B22",
    "THR Presby" = "#41924D",
    "Childrens" = "#ED2939",
    "Methodist" = "#215AA8",
    "VA" = "#162E51"
)

# The matching categorical palette from Tableau ("HospColors") for when you need
# generic distinct colors rather than a name->brand mapping.
hospital_palette_categorical <- c(
    "#F5B946", "#B02AA5", "#871B22", "#41924D", "#005999"
)

#' Access hospital-system branding colors
#'
#' Corporate brand colors for receiving-facility hospital systems, for coloring
#' visualizations by patient destination.
#'
#' @param ... Unquoted or quoted hospital-system names (keys of
#'   `hospital_colors`, e.g. `"Parkland"`, `"BUMC"`). With no arguments, returns
#'   the full named vector.
#'
#' @returns A named character vector of hex colors.
#' @export
#'
#' @examples
#' hospital_cols()
#' hospital_cols("Parkland", "Methodist")
hospital_cols <- function(...) {
    cols <- c(...)

    if (is.null(cols)) {
        return(hospital_colors)
    }

    if (any(!(cols %in% names(hospital_colors)))) {
        warning(paste0(
            "Invalid hospital names: ",
            paste(
                paste0("\"", cols[!(cols %in% names(hospital_colors))], "\""),
                collapse = ", "
            )
        ))
    }

    hospital_colors[cols[cols %in% names(hospital_colors)]]
}

#' Color/fill scales for hospital-destination branding
#'
#' Map a hospital-system variable to each system's corporate brand color. These
#' wrap [ggplot2::scale_colour_manual()] / [ggplot2::scale_fill_manual()] with
#' `values = hospital_cols()`, so the levels of your variable must match the
#' names in [hospital_cols()] (e.g. `"Parkland"`, `"THR Presby"`). Override or
#' extend the mapping by passing your own `values`.
#'
#' Use these when the color/fill aesthetic encodes **destination facility**;
#' reserve `scale_*_dfr()` for DFR-internal categories.
#'
#' @param ... Additional arguments passed to the underlying
#'   `ggplot2::scale_*_manual()` (e.g. `values`, `breaks`, `labels`, `name`).
#' @param na.value Color for systems not present in the mapping. Defaults to the
#'   DFR grey (`#A5A5A5`).
#'
#' @returns A ggplot2 scale.
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot2::ggplot(trips, ggplot2::aes(x, y, color = destination_system)) +
#'   ggplot2::geom_point() +
#'   scale_color_hospital()
#' }
scale_color_hospital <- function(..., na.value = "#A5A5A5") {
    args <- list(...)
    if (is.null(args$values)) args$values <- hospital_cols()
    args$na.value <- na.value
    do.call(ggplot2::scale_colour_manual, args)
}

#' @rdname scale_color_hospital
#' @export
scale_fill_hospital <- function(..., na.value = "#A5A5A5") {
    args <- list(...)
    if (is.null(args$values)) args$values <- hospital_cols()
    args$na.value <- na.value
    do.call(ggplot2::scale_fill_manual, args)
}
