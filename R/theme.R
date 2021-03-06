
#' Custom theme
#'
#' @description A custom \code{ggplot2} theme.
#' @details Uses \code{ggplot2::theme_minimal()} as basis. Customized
#' for personal preferences.
#' @param base_family,title.face,subtitle.face,caption.face,strip.text.face,axis.title.face,axis.text.face character.
#' @param base_size,title.size,subtitle.size,caption.size,strip.text.size,axis.title.size,axis.text.size numeric.
#' @param axis.title.just character. One of one of \code{'blmcrt'}
#' @param legend.position gg element. Set to \code{"bottom"} by default
#' (instead of "right").
#' @param legend.title gg element. Set to a non-trivial default
#' (instead of \code{ggplot2::element_text()}).
#' @param panel.grid.minor gg element. Set to a non-trivial default
#' (instead of \code{ggplot2::element_line()}).
#' @param plot.background gg element. Was set to a non-trivial default
#' (instead of \code{ggplot2::element_blank()}) in a previous version.
#' @param panel.spacing gg element. Set to a non-trivial default
#' (instead of \code{5.5pt}).
#' @param ... dots. Additional parameters passed to \code{ggplot2::theme()}.
#' @export
#' @seealso \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r}.
#' \url{\code{hrbrthemes::theme_ipsum()}}
theme_te <-
  function(base_family = "Arial Narrow",
           base_size = 12,
           title.size = 16,
           title.face = "bold",
           subtitle.size = base_size,
           subtitle.face = "plain",
           caption.size = base_size,
           caption.face = subtitle.face,
           strip.text.size = base_size,
           strip.text.face = subtitle.face,
           axis.title.size = base_size,
           axis.title.face = subtitle.face,
           axis.text.size = base_size,
           axis.text.face = subtitle.face,
           legend.position = "bottom",
           legend.title = ggplot2::element_blank(),
           panel.grid.minor = ggplot2::element_blank(),
           # plot.background = ggplot2::element_rect(color = "black", size = 1),
           plot.background = ggplot2::element_blank(),
           panel.spacing = grid::unit(1, "lines"),
           axis.title.just = "rt",
           ...) {
  ret <-
    ggplot2::theme_minimal(base_family = base_family, base_size = base_size)


  ret <-
    ret +
    ggplot2::theme(
      legend.position = legend.position
    )

  ret <-
    ret +
    ggplot2::theme(
      legend.title = legend.title
    )

  ret <-
    ret +
    ggplot2::theme(
      panel.grid.minor = panel.grid.minor
    )


  ret <-
    ret + ggplot2::theme(
      plot.background = plot.background
    )

  ret <- ret + theme(panel.spacing = panel.spacing)

  # This part is is from hrbrthemes::theme_ipsum().


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
      plot.title =
        ggplot2::element_text(
          hjust = 0,
          size = title.size,
          face = title.face,
        ),
      plot.subtitle =
        ggplot2::element_text(
          hjust = 0,
          size = subtitle.size,
          face = subtitle.face,
        ),
      plot.caption =
        ggplot2::element_text(
          hjust = 0,
          size = caption.size,
          face = caption.face
        ),
      strip.text =
        ggplot2::element_text(
          hjust = 0,
          size = strip.text.size,
          face = strip.text.face
        ),
      axis.title.x =
        ggplot2::element_text(
          hjust = xj,
          size = axis.title.size,
          face = axis.title.face
        ),
      axis.title.y =
        ggplot2::element_text(
          hjust = yj,
          size = axis.title.size,
          face = axis.title.face
        ),
      axis.title.y.right =
        ggplot2::element_text(
        hjust = yj,
        angle = 90,
        size = axis.title.size,
        face = axis.title.face
      ),
      axis.text.x =
        ggplot2::element_text(
          margin = ggplot2::margin(t = 0),
          size = axis.text.size,
          face = axis.text.face,
        ),
      axis.text.y =
        ggplot2::element_text(
          margin = ggplot2::margin(r = 0),
          size = axis.text.size,
          face = axis.text.face,
        )
    )

  ret <-
    ret +
    ggplot2::theme(...)
  ret
  }

#' Update \code{ggplot2} font defaults for text geoms
#'
#' @description Updates \code{ggplot2::geom_label()} and \code{ggplot2::geom_text()} font defaults.
#' @details None.
#' @param family,face,size,color character.
#' @export
#' @source \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r}.
update_geom_font_defaults <-
  function(family = "Arial Narrow",
           face = "plain",
           size = 3.5,
           color = "#2b2b2b") {
    ggplot2::update_geom_defaults("text",
                                  list(
                                    family = family,
                                    face = face,
                                    size = size,
                                    color = color
                                  ))
    ggplot2::update_geom_defaults("label",
                                  list(
                                    family = family,
                                    face = face,
                                    size = size,
                                    color = color
                                  ))
  }


#' @rdname theme_te
#' @export
theme_te_dx <-
  function(...)
    theme_te(panel.grid.major.x = ggplot2::element_blank(), ...)

#' @rdname theme_te
#' @export
theme_te_facet <-
  function(...)
    theme_te(panel.background = ggplot2::element_rect(), ...)

#' @rdname theme_te
#' @export
theme_te_facet_dx <- function(...)
  theme_te(
    panel.background = ggplot2::element_rect(),
    panel.grid.major.x = ggplot2::element_blank(),
    ...
  )

# `%+replace%` <- ggplot2::`%+replace%`

#' Custom theme for maps
#'
#' @description A custom \code{ggplot2} theme.
#' @details Uses \code{theme_te()} as basis.
#' @inheritParams theme_te
#' @param panel.background.fill,panel.background.color,strip.background.fill,strip.background.color character.
#' Passed directly to \code{ggplot2::theme()} parameters with similar names.
#' @param ... dots. Passed directly to \code{theme_te()}
#' @rdname theme_te_map
#' @seealso \url{http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html}.
#' \url{https://gist.github.com/hrbrmstr/33baa3a79c5cfef0f6df}.
#' @export
theme_map <-
  function(base_family = "Arial Narrow",
           base_size = 12,
           panel.background.fill = NULL,
           panel.background.color = NULL,
           strip.background.fill = panel.background.fill,
           strip.background.color = panel.background.color,
           ...) {
    # ret <- ggplot2::theme_bw(base_family = base_family, base_size = base_size)
    ret <-
      theme_te(
        base_family = base_family,
        base_size = base_size,
        ...
      )

    ret <-
      ret %+replace%
      ggplot2::theme(
        # axis.text = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        # axis.title = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        panel.spacing = grid::unit(0, "lines"),
        panel.background = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        legend.justification = c(0, 0),
        legend.position = c(0, 0)
      )

    ret <-
      ret %+replace%
      ggplot2::theme(
        # These two are my customizations.
        panel.background =
          ggplot2::element_rect(
            fill = panel.background.fill,
            color = panel.background.color
          ),
        strip.background =
          ggplot2::element_rect(
            fill = strip.background.fill,
            color = strip.background.color
          )
      )
    # ret <- ret + theme_te(...)
    ret
  }

# #' @rdname theme_map
# #' @export
# theme_te_map <- theme_map

#' Custom theme for tile plot
#'
#' @description A custom \code{ggplot2} theme.
#' @details Uses \code{theme_te()} as basis.
#' @inheritParams theme_te
#' @param panel.grid.major,panel.grid.minor,axis.text.x gg elements.
#' Set to a non-trivial default
#' Passed directly to \code{ggplot2::theme()} parameters with similar names.
#' @param ... dots. Passed directly to \code{ggplot2::theme_bw()}
#' @rdname theme_te_tile
#' @export
theme_te_tile <-
  function(base_family = "Arial Narrow",
           base_size = 12,
           panel.grid.major = ggplot2::element_blank(),
           panel.grid.minor = ggplot2::element_blank(),
           axis.text.x = ggplot2::element_text(angle = 90),
           ...) {
    ret <-
      theme_te(
        base_family = base_family,
        base_size = base_size,
        ...
      )

    ret <-
      ret %+replace%
      ggplot2::theme(
        panel.grid.major = panel.grid.major,
        panel.grid.minor = panel.grid.minor,
        axis.text.x = axis.text.x
      )
    ret
  }

