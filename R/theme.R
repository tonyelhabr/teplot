
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
  # ret <-
  #   switch(option,
  #          `1` = ..
  #   )
  if(option == "a") {
    ret <- theme_te_a(...)
  } else if (option == "b") {
    ret <- theme_te_b(...)
  }
  ret
}

#'  Custom theme
#'
#' @description A custom \code{ggplot2} theme. Similar to \code{theme_te_b} but makes
#' less specific choices about margins.
#' @details Uses \code{ggplot2::theme_minimal()} as basis.
#' @inheritParams theme_te_b
#' @param ... dots. Additional parameters passed to \code{ggplot2::theme()}.
#' @export
#' @seealso \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r}
theme_te_a <-
  function(base_family = "Arial Narrow",
           base_size = 12,
           title.size = 18,
           title.face = "bold",
           subtitle.size = 16,
           subtitle.face = "plain",
           caption.size = 12,
           caption.face = "plain",
           ...) {
  ret <-
    ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  ret <-
    ret +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    )

  ret <-
    ret +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )

  ret <-
    ret +
    ggplot2::theme(
      plot.title =
        ggplot2::element_text(
        size = title.size,
        face = title.face,
        hjust = 0
        ),
      plot.subtitle =
        ggplot2::element_text(
          size = subtitle.size,
          face = subtitle.face,
          hjust = 0
        ),
      plot.caption =
        ggplot2::element_text(
          size = caption.size,
          face = caption.face,
          hjust = 0
        ),
    )

  # ret <-
  #   ret +
  #   ggplot2::theme(...)
  ret
}


#' Custom theme (inspired by hrbrmstr)
#'
#' @description Custom \code{ggplot2} theme that is based on \code{hrbrthemes::theme_ipsum()}.
#' @details Uses \code{ggplot2::theme_minimal()} as basis.
#' Legend is formatted in a different manner than \code{hrbrthemes::theme_ipsum()}.
#' Parameters not borrowed: \code{grid_col, grid, axis.co, axis, ticks}.
#' @param base_family,title.face,subtitle.face,strip.text.face,caption.face,axis.title.face character.
#' @param title.family,subtitle.family,strip.text.family,caption.family,axis.title.family character. Inherit from base_family.
#' @param axis.title.just character.
#' @param base_size,title.size,title.margin,subtitle.size,subtitle.margin,strip.text.size,caption.size,caption.margin,axis.title.size,axis.text.size numeric.
#' @param plot.margin numeric vector.
#' @param ... dots. Additional parameters passed to \code{ggplot2::theme()}.
#' @export
theme_te_b <-
  function(base_family = "Arial Narrow",
           base_size = 12,
           title.family = base_family,
           title.size = 18,
           title.face = "bold",
           title.margin = 10,
           subtitle.family = base_family,
           subtitle.size = 12,
           subtitle.face = "plain",
           subtitle.margin = 10,
           strip.text.family = base_family,
           strip.text.size = 12,
           strip.text.face = "plain",
           caption.family = base_family,
           caption.size = 12,
           caption.face = "plain",
           caption.margin = 10,
           axis.text.size = base_size,
           axis.title.family = subtitle.family,
           axis.title.size = 12,
           axis.title.face = "plain",
           axis.title.just = "rt",
           plot.margin = ggplot2::margin(1, 1, 1, 1),
           ...) {
    ret <-
      ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

    ret <-
      ret +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = ggplot2::element_blank()
      )

    # Heavily copied from here on.
    xj <-
      switch(
        tolower(substr(axis.title.just, 1, 1)),
        b = 0,
        l = 0,
        m = 0.5,
        c = 0.5,
        r = 1,
        t = 1
      )

    yj <-
      switch(
        tolower(substr(axis.title.just, 2, 2)),
        b = 0,
        l = 0,
        m = 0.5,
        c = 0.5,
        r = 1,
        t = 1
      )

    ret <-
      ret +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          size = axis.text.size, margin =
            ggplot2::margin(t = 0)
        )
      )

    ret <-
      ret +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(
          size = axis.text.size,
          margin = ggplot2::margin(r = 0)
        )
      )

    ret <-
      ret +
      ggplot2::theme(
        axis.title = ggplot2::element_text(
          size = axis.title.size, family =
            axis.title.family
        )
      )

    ret <-
      ret +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          hjust = xj,
          size = axis.title.size,
          family = axis.title.family,
          face = axis.title.face
        )
      )

    ret <-
      ret +
      ggplot2::theme(
        axis.title.y = ggplot2::element_text(
          hjust = yj,
          size = axis.title.size,
          family = axis.title.family,
          face = axis.title.face
        )
      )

    ret <-
      ret +
      ggplot2::theme(
        axis.title.y.right = ggplot2::element_text(
          hjust = yj,
          size = axis.title.size,
          angle = 90,
          family = axis.title.family,
          face = axis.title.face
        )
      )

    ret <-
      ret +
      ggplot2::theme(
        strip.text = ggplot2::element_text(
          hjust = 0,
          size = strip.text.size,
          face = strip.text.face,
          family = strip.text.family
        )
      )
    # ret <-
    #   ret + ggplot2::theme(panel.spacing = grid::unit(2, "lines"))
    ret <-
      ret +
      ggplot2::theme(panel.spacing = grid::unit(0.25, "lines"))

    ret <-
      ret +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0,
          size = title.size,
          margin = ggplot2::margin(b = title.margin),
          family = title.family,
          face = title.face
        )
      )

    ret <-
      ret +
      ggplot2::theme(
        plot.subtitle = ggplot2::element_text(
          hjust = 0,
          size = subtitle.size,
          margin = ggplot2::margin(b = subtitle.margin),
          family = subtitle.family,
          face = subtitle.face
        )
      )

    ret <-
      ret +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(
          hjust = 0,
          size = caption.size,
          margin = ggplot2::margin(t = caption.margin),
          family = caption.family,
          face = caption.face
        )
      )
    ret <- ret + ggplot2::theme(plot.margin = plot.margin)
    ret
  }

#' @rdname theme_te
#' @export
theme_te_dx <- function(option = c("a", "b"), ...) {
  option <- match.arg(option)
  if(option == "a") {
    ret <- theme_te_a_dx(...)
  } else if (option == "b") {
    ret <- theme_te_b_dx(...)
  }
  ret
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
    ret <- theme_te_a_facet(...)
  } else if (option == "b") {
    ret <- theme_te_b_facet(...)
  }
  ret
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
    ret <- theme_te_a_facet_dx(...)
  } else if (option == "b") {
    ret <- theme_te_b_facet_dx(...)
  }
  ret
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
