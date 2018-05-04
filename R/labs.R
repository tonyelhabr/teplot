
#' Set x and y labels to \code{NULL}
#'
#' @description Sets the x and y labels to \code{NULL}.
#' @details None.
#' @return gg object.
#' @export
labs_xy_null <-
  function() {
    ggplot2::labs(x = NULL, y = NULL)
  }
