
#' Custom theme
#'
#' @description A custom \code{ggplot2} theme.
#' @details Uses \code{ggplot2::theme_minimal()} as basis.
#' @param base_family,title.face,subtitle.face,caption.face,strip.text.face,axis.title.face,axis.text.face character.
#' @param base_size,title.size,subtitle.size,caption.size,strip.text.size,axis.title.size,axis.text.size numeric.
#' @param ... dots. Additional parameters passed to \code{ggplot2::theme()}.
#' @export
#' @seealso \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r}
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
           ...) {
  ret <-
    ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  ret <-
    ret +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
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
          size = axis.title.size,
          face = axis.title.face
        ),
      axis.title.y =
        ggplot2::element_text(
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

#' Custom theme for maps
#'
#' @description A custom \code{ggplot2} theme.
#' @details Uses \code{ggplot2::theme_te()} as basis.
#' @param panel.background.fill,panel.background.color,strip.background.fill,strip.background.color character.
#' Passed directly to \code{ggplot2::theme()} parameters with similar names.
#'
#' @rdname theme_te
#' @seealso \url{http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html}.
#' @export
theme_te_map <-
  function(...,
           panel.background.fill = NA,
           panel.background.color = NA,
           strip.background.fill = panel.background.fill,
           strip.background.color = panel.background.color) {
    # ggplot2::theme_bw() +
    theme_te(...) +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
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
  }


