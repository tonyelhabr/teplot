


#' Custom colors
#'
#' @description colors
#' @details These were hand-picked, keeping in mind 'oppositional' colors
#' (i.e. red and green, blue and orange, purple and yellow, etc.)
#' @format character (vector). Named.
#' @export
"te_colors"

# te_colors <-
#   c(
#     `firebrick` = "#B22222",
#     `chartreuse` = "#7FFF00",
#     `royalblue` = "#4169E1",
#     `darkorange` = "#FF8C00",
#     `darkmagenta` = "#8B008B",
#     `gold` = "#FFD700",
#     `hotpink` = "#FF69B4",
#     `deepskyblue` = "#00BFFF",
#     `lightgrey` = "#CCCCCC",
#     `darkgrey` = "#8C8C8C"
#   )

te_colors <-
  c(
    `blue` = "#4169E1",
    `orange` = "#FF8C00",
    `red` = "#B22222",
    `green` = "#7FFF00",
    `purple` = "#8B008B",
    `yellow` = "#FFD700",
    `turquoise` = "#00BFFF",
    `pink` = "#FF69B4",
    `darkgrey` = "#8C8C8C",
    `lightgrey` = "#CCCCCC"

  )

# # https://github.com/clauswilke/dataviz/blob/master/pitfalls_of_color_use.Rmd.
# # http://jfly.iam.u-tokyo.ac.jp/color/#redundant1
# te_colors <-
#   c(
#     `orange` = "#E69F00",
#     `skyblue` = "#56B4E9",
#     `bluishgreen` = "#009E73",
#     `yellow` = "#F0E442",
#     `blue`= "#0072B2",
#     `vermillion` = "#D55E00",
#     `reddishpurple` = "#CC79A7",
#     `darkgrey` = "#999999",
#     `black` = "#000000"
#   )
# scales::show_col(te_colors)
# devtools::use_data(te_colors, internal = FALSE, overwrite = TRUE)

#' Extract hex values from te_colors
#'
#' @param ... dots. Names of \code{te_colors} to subset.
te_cols <- function(...) {
  nms <- c(...)

  if (is.null(nms))
    return (te_colors)

  te_colors[nms]
}

te_palettes <- list(
  `discrete` = te_cols(),
  `continuous` = te_cols("purple", "yellow")
)

#' Return function to interpolate a \code{te_color} palette
#'
#' @description Returns a function to be used by other functions.
#' @details Calls \code{scales::manual_pal()} if \code{discrete = TRUE} or  \code{colorRampPalette()} if
#' \code{discrete = FALSE}.
#' @param palette character. Name of palette in \code{te_palettes} list.
#' @param discrete logical. Indicates whether the color palette is discrete or not.
#' @param reverse logical. Indicates whether the palette should be reversed.
#' @importFrom scales manual_pal
#' @importFrom grDevices colorRampPalette
#' @source \url{https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2}.
#' \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/scales.r}
te_pal <- function(palette = "discrete",
                   discrete = TRUE,
                   reverse = FALSE) {
  if((discrete) & (palette != "discrete")) {
    discrete <- FALSE
    message("Setting `discrete = FALSE`.")
  } else if ((!discrete) & (palette == "discrete")) {
    discrete <- TRUE
    message("Setting `discrete = TRUE`.")
  }

  pal <- te_palettes[[palette]]

  pal <- unname(pal)
  if (reverse)
    pal <- rev(pal)

  # NOTE: For some reason, `colorRampPalette()` is not retuning expected output with discrete scale.
  if(discrete) {
    ret <- scales::manual_pal(pal)
  } else {
    ret <- grDevices::colorRampPalette(pal)
  }
  # grDevices::colorRampPalette(pal)
  ret
}

#' Color scale constructor for \code{te_color}
#'
#' @description Color function for use with \code{ggplot2}.
#' @details None.
#' @inheritParams te_pal
#' @param ... dots. Additional arameters passed to \code{ggplot2::discrete_scale()} if
#' \code{discrete = TRUE} or
#' \code{ggplot2::scale_fill_gradientn()} if \code{discrete = FALSE}.
#' @source \url{https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2}.
#' \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/scales.r}
#' @export
scale_color_te <-
  function(palette = "discrete",
           discrete = TRUE,
           reverse = FALSE,
           ...) {
    pal <- te_pal(palette = palette, discrete = discrete, reverse = reverse)

    if (discrete) {
      ret <- ggplot2::discrete_scale("colour", scale_name = palette, palette = pal, ...)
    } else {
      ret <- ggplot2::scale_color_gradientn(colours = pal(256), ...)
    }
    ret
  }


#' Fill scale constructor for \code{te_color}
#'
#' @description Color function for use with \code{ggplot2}.
#' @details None.
#' @inheritParams scale_color_te
#' @source \url{https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2}.
#' \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/scales.r}
#' @export
scale_fill_te <-
  function(palette = "discrete",
           discrete = TRUE,
           reverse = FALSE,
           ...) {
    pal <- te_pal(palette = palette, discrete = discrete, reverse = reverse)

    if (discrete) {
      ret <- ggplot2::discrete_scale("fill", paste0("te_", palette), palette = pal, ...)
    } else {
      ret <- ggplot2::scale_fill_gradientn(colours = pal(256), ...)
    }
    ret
  }
