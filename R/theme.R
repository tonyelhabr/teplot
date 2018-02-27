
#' Custom theme
#'
#' @description  A custom \code{ggplot2} theme.
#' @details Option "a" is an 'original' theme.
#' Option "b" is an adapted \code{hrbrthemes::theme_ipusm()} theme.
#' @param option character. Either "a" or "b".
#' @param ... dots. Parameters passed to theme specified by option.
#' @rdname theme_te
#' @export
theme_te <- function(..., option = c("a", "b")) {
  option <- match.arg(option)
  # out <-
  #   switch(option,
  #          `1` = ..
  #   )
  if(option == "a") {
    out <- theme_te_a(...)
  } else if (option == "b") {
    out <- theme_te_b(...)
  }
  out
}

#'  Custom theme
#'
#' @description A custom \code{ggplot2} theme.
#' @details None.
#' @param base_family character.
#' @param base_size,plot_title_size,subtitle_size numeric.
#' @param ... dots. Additional parameters passed to \code{ggplot2::theme()}.
#' @export
#' @seealso \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r}
theme_te_a <- function (base_family = "",
                      base_size = 11,
                      plot_title_size = 16,
                      subtitle_size = 12,
                      ...) {
  out <-
    ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  out <-
    out +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank())

  out <-
    out +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )

  out <-
    out +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = plot_title_size,
        face = "bold",
        hjust = 0
      ),
      # plot.subtitle = ggplot2::element_text(size = subtitle_size, face = "bold.italic", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = subtitle_size, hjust = 0),
      plot.caption = ggplot2::element_text(face = "italic"),
      ...
    )
  out
}


#' Tony's theme (inspired by hrbrmstr)
#'
#' @description Borrows heavily from \code{hrbrthemes::theme_ipsum}.
#' @details Legend is formatted in a different manner than  \code{hrbrthemes::theme_ipsum}.
#' Not copied: \code{grid_col, grid, axis_co, axis, ticks}.
#' @param base_family,plot_title_face,subtitle_face,strip_text_face,caption_face,axis_title_face character.
#' @param plot_title_family,subtitle_family,strip_text_family,caption_family,axis_title_family character. Inhereit from base_family.
#' @param axis_title_just character.
#' @param base_size,plot_title_size,plot_title_margin,subtitle_size,subtitle_margin,strip_text_size,caption_size,caption_margin,axis_title_size,axis_text_size numeric.
#' @param plot_margin numeric vector.
#' @param ... dots. Additional parameters passed to \code{ggplot2::theme()}.
#' @export
theme_te_b <- function (base_family = "Arial Narrow",
                        base_size = 11.5,
                        plot_title_family = base_family,
                        plot_title_size = 18,
                        plot_title_face = "bold",
                        plot_title_margin = 10,
                        subtitle_family = base_family,
                        subtitle_size = 12,
                        subtitle_face = "plain",
                        subtitle_margin = 15,
                        strip_text_family = base_family,
                        strip_text_size = 12,
                        strip_text_face = "plain",
                        caption_family = base_family,
                        caption_size = 9,
                        caption_face = "italic",
                        caption_margin = 10,
                        axis_text_size = base_size,
                        axis_title_family = subtitle_family,
                        axis_title_size = 9,
                        axis_title_face = "plain",
                        axis_title_just = "rt",
                        plot_margin = ggplot2::margin(5, 5, 5, 5),
                        ...) {
  out <-
    ggplot2::theme_minimal(base_family = base_family, base_size = base_size)
  out <- out +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank())

  # Heavily copied from here on.
  xj <-
    switch(
      tolower(substr(axis_title_just, 1, 1)),
      b = 0,
      l = 0,
      m = 0.5,
      c = 0.5,
      r = 1,
      t = 1
    )
  yj <-
    switch(
      tolower(substr(axis_title_just, 2, 2)),
      b = 0,
      l = 0,
      m = 0.5,
      c = 0.5,
      r = 1,
      t = 1
    )

  out <-
    out + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = axis_text_size, margin =
          ggplot2::margin(t = 0)
      )
    )
  out <-
    out + ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        size = axis_text_size,
        margin = ggplot2::margin(r = 0)
      )
    )
  out <-
    out + ggplot2::theme(
      axis.title = ggplot2::element_text(
        size = axis_title_size, family =
          axis_title_family
      )
    )
  out <-
    out + ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        hjust = xj,
        size = axis_title_size,
        family = axis_title_family,
        face = axis_title_face
      )
    )
  out <-
    out + ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        hjust = yj,
        size = axis_title_size,
        family = axis_title_family,
        face = axis_title_face
      )
    )
  out <-
    out + ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(
        hjust = yj,
        size = axis_title_size,
        angle = 90,
        family = axis_title_family,
        face = axis_title_face
      )
    )
  out <-
    out + ggplot2::theme(
      strip.text = ggplot2::element_text(
        hjust = 0,
        size = strip_text_size,
        face = strip_text_face,
        family = strip_text_family
      )
    )
  out <-
    out + ggplot2::theme(panel.spacing = grid::unit(2, "lines"))
  out <-
    out + ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0,
        size = plot_title_size,
        margin = ggplot2::margin(b = plot_title_margin),
        family = plot_title_family,
        face = plot_title_face
      )
    )
  out <-
    out + ggplot2::theme(
      plot.subtitle = ggplot2::element_text(
        hjust = 0,
        size = subtitle_size,
        margin = ggplot2::margin(b = subtitle_margin),
        family = subtitle_family,
        face = subtitle_face
      )
    )
  out <-
    out + ggplot2::theme(
      plot.caption = ggplot2::element_text(
        hjust = 1,
        size = caption_size,
        margin = ggplot2::margin(t = caption_margin),
        family = caption_family,
        face = caption_face
      )
    )
  out <- out + ggplot2::theme(plot.margin = plot_margin)
  out
}

#' @rdname theme_te
#' @export
theme_te_dx <- function(..., option = c("a", "b")) {
  option <- match.arg(option)
  if(option == "a") {
    out <- theme_te_a_dx(...)
  } else if (option == "b") {
    out <- theme_te_b_dx(...)
  }
  out
}

#' @rdname theme_te_a
#' @export
theme_te_a_dx <-
  function(...)
    theme_te_a(panel.grid.major.x = ggplot2::element_blank(), ...)

#' @rdname theme_te_b
#' @export
theme_te_b_dx <-
  function(...)
    theme_te_b(panel.grid.major.x = ggplot2::element_blank(), ...)


#' @rdname theme_te
#' @export
theme_te_facet <- function(..., option = c("a", "b")) {
  option <- match.arg(option)
  if(option == "a") {
    out <- theme_te_a_facet(...)
  } else if (option == "b") {
    out <- theme_te_b_facet(...)
  }
  out
}

#' @rdname theme_te_a
#' @export
theme_te_a_facet <-
  function(...)
    theme_te_a(panel.background = ggplot2::element_rect(), ...)

#' @rdname theme_te_b
#' @export
theme_te_b_facet <-
  function(...)
    theme_te_b(panel.background = ggplot2::element_rect(), ...)

#' @rdname theme_te
#' @export
theme_te_facet_dx <- function(..., option = c("a", "b")) {
  option <- match.arg(option)
  if(option == "a") {
    out <- theme_te_a_facet_dx(...)
  } else if (option == "b") {
    out <- theme_te_b_facet_dx(...)
  }
  out
}

#' @rdname theme_te_a
#' @export
theme_te_a_facet_dx <- function(...)
  theme_te(
    panel.background = ggplot2::element_rect(),
    panel.grid.major.x = ggplot2::element_blank(),
    ...
  )

#' @rdname theme_te_b
#' @export
theme_te_b_facet_dx <- function(...)
  theme_te_b(
    panel.background = ggplot2::element_rect(),
    panel.grid.major.x = ggplot2::element_blank(),
    ...
  )
