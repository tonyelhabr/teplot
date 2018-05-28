


#' Create a scale to add to a \code{ggplot2} plot
#'
#' @description Creates pretty breaks for a continuous scale.
#' @details Only works for a single state.
#' @param ... dots. Arguments passed to underlying \code{ggplot2} function.
#' @return gg object.
#' @rdname scale_pretty
#' @export
scale_y_pretty_comma <- function(...) {
  ggplot2::scale_y_continuous(...,
                              breaks = scales::pretty_breaks(),
                              labels = scales::comma_format())
}

#' @rdname scale_pretty
#' @export
scale_y_pretty_percent <- function(...) {
  ggplot2::scale_y_continuous(...,
                              breaks = scales::pretty_breaks(),
                              labels = scales::percent_format())
}

#' @rdname scale_pretty
#' @export
scale_x_pretty_comma <- function(...) {
  ggplot2::scale_x_continuous(...,
                              breaks = scales::pretty_breaks(),
                              labels = scales::comma_format())
}

#' @rdname scale_pretty
#' @export
scale_x_pretty_percent <- function(...) {
  ggplot2::scale_x_continuous(...,
                              breaks = scales::pretty_breaks(),
                              labels = scales::percent_format())
}

#' @rdname scale_pretty
#' @export
scale_xy_pretty_comma <- function(...) {
  list(scale_x_pretty_comma(...), scale_y_pretty_comma(...))
}

#' @rdname scale_pretty
#' @export
scale_xy_pretty_percent <- function(...) {
  list(scale_x_pretty_percent(...), scale_y_pretty_percent(...))
}

#' Quick access to RColorBrewer palette 'Set1'.
#'
#' @description Use to incorporate RColorBrewer palette 'Set1' in \code{ggplot}s.
#' @details This is a color palette that I use quite often (simply
#' as an alternative to \code{ggplot2}'s default palette.
#' @return gg object.
#' @rdname scale_set1
#' @export
scale_color_set1 <- function() {
  ggplot2::scale_color_brewer(palette = "Set1")
}

#' @rdname scale_set1
#' @export
scale_fill_set1 <- function() {
  ggplot2::scale_fill_brewer(palette = "Set1")
}
