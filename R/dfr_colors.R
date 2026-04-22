#https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

dfr_colors <- list(
    omd_navy = "#00286F",
    omd_red = "#d80000",
    dfr_gold = "#e8bd00",
    dark_navy = "#15324c",
    grey = "#A5a5a5",
    red = "#c6291c",
    aqua = "#7bd3f9",
    yellow = "#f5bf42",
    accent1 = "#00399e",
    accent2 = "#004acf",
    accent3 = "#005cff",
    green = "#7eaa55"
)

#' Access DFR Colors
#'
#' @param ... color choices
#'
#' @returns DFR color
#' @export
#'
#' @examples
#' dfr_cols("omd_navy")
dfr_cols <- function(...) {
    cols <- c(...)

    if (is.null(cols)) {
        return(dfr_colors)
    }

    if (any(!(cols %in% names(dfr_colors)))) {
        warning(paste0(
            "Invalid color names: ",
            paste(
                paste0("\"", cols[!(cols %in% names(dfr_colors))], "\""),
                collapse = ", "
            )
        ))
    }

    lapply(cols, function(col) {
        dfr_colors[[col]]
    })

    #dfr_colors[[cols %in% names(dfr_colors)]]
}

dfr_palettes <- list(
    `main` = dfr_cols("omd_navy", "omd_red", "dfr_gold", "dark_navy"),
    `accent` = dfr_cols("accent1", "accent2", "accent3", "aqua"),
    `navy` = dfr_cols(
        "dark_navy",
        "omd_navy",
        "accent1",
        "accent2",
        "accent3",
        "aqua"
    )
)

#' Return function to interpolate DFR color palette
#'
#' @param palette Character name of palette in dfr_palettes
#' @param reverse Boolean indicating whether or not palette should be reversed.
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @returns Color palette
#' @export
#'
dfr_pal <- function(palette = "main", reverse = FALSE, ...) {
    pal <- dfr_palettes[[palette]]

    if (reverse) {
        pal <- rev(pal)
    }

    colorRampPalette(pal, ...)
}

#' Color scale constructor for DFR colors
#'
#' @param palette Character name of palette in dfr_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_dfr <- function(
    palette = "main",
    discrete = TRUE,
    reverse = FALSE,
    ...
) {
    pal <- dfr_pal(palette = palette, reverse = reverse)

    if (discrete) {
        discrete_scale("color", paste0("dfr_", palette), palette = pal, ...)
    } else {
        scale_color_gradientn(colours = pal(256), ...)
    }
}

#' Fill scale constructor for dfr colors
#'
#' @param palette Character name of palette in dfr_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_dfr <- function(
    palette = "main",
    discrete = TRUE,
    reverse = FALSE,
    ...
) {
    pal <- dfr_pal(palette = palette, reverse = reverse)

    if (discrete) {
        discrete_scale("fill", paste0("dfr_", palette), palette = pal, ...)
    } else {
        scale_fill_gradientn(colours = pal(256), ...)
    }
}
