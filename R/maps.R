
#' Convert state abbreviation to name
#'
#' @description Convert U.S. state abbreviation to full name.
#' @details Used by other package functions to coerce \code{state} parameter to proper format
#' for working with \code{ggplot2::map_data()}
#' @param state character for state name. May either be an abbreviation or full name and
#' may be either lower-case or upper-case.
#' @return lower-case character representing full state name.
#' @rdname convert_state_abb_to_name
#' @export
convert_state_abb_to_name <- function(state = NULL) {
    stopifnot(!is.null(state), length(state) == 1)
    if(!nchar(state) == 2) {
      return(tolower(state))
    }
    idx <- which(datasets::state.abb == toupper(state))
    stopifnot(length(idx) == 1)
    ret <- tolower(datasets::state.name[idx])
    states_valid <- unique(ggplot2::map_data("state")$region)
    stopifnot(length(which(states_valid == ret)) == 1)
    ret
  }


#' Get data for \code{ggplot2} U.S. state map
#'
#' @inheritParams convert_state_abb_to_name
#'
#' @return data.frame.
#' @seealso \code{ggplot2::map_data()}
#' @rdname get_map_data
#' @export
get_map_data_county <- function(state = NULL) {
  stopifnot(!is.null(state), length(state) == 1)
  state <- convert_state_abb_to_name(state)
  region <- NULL
  subset(ggplot2::map_data("county"), region == state)
}


#' @rdname get_map_data
#' @export
get_map_data_county_tx <- function(state = "texas") {
  get_map_data_county(state = state)
}

#' @rdname get_map_data
#' @export
get_map_data_state <- function(state = NULL) {
  stopifnot(!is.null(state), length(state) == 1)
  state <- convert_state_abb_to_name(state)
  region <- NULL
  subset(ggplot2::map_data("state"), region == state)
}

#' @rdname get_map_data
#' @export
get_map_data_state_tx <- function(state = "texas") {
  get_map_data_state(state = state)
}

#' Get the inverse of a color
#'
#' @description None.
#' @details used by the \code{create_map_state()} function.
#' @param color character. Either a hex code or a valid color name.
#'
#' @return character.
#' @rdname get_color_inv
#' @export
get_color_inv <- function(color = NULL) {
  stopifnot(!is.null(color), length(color) == 1)
  grDevices::rgb(t(255 - grDevices::col2rgb(color)), max = 255)
}

#' Create a \code{ggplot2} map for a single U.S. state
#'
#' @description Create the state layer for a \code{ggplot2} plot.
#' @details Only works for a single state. It is expected that \code{theme_te_map()} is called beforehand.
#' @inheritParams convert_state_abb_to_name
#' @param show_county logical. Whether or not to show county lines for specified state.
#' @param fill,color character. Either a hex code or a valid color name.
#' Parameters passed directly to \code{ggplot2::geom_polygon()}.
#'
#' @return gg.
#' @seealso \code{ggplot2::map_data()}
#' @rdname create_map_state
#' @export
create_map_state <-
  function(state = NULL,
           show_county = TRUE,
           fill = "white",
           color = "black") {
    stopifnot(!is.null(state), length(state) == 1)
    data_state <- get_map_data_state(state)

    if (color == fill) {
      color_old <- color
      color <- get_color_inv(color_old)
      message(sprintf("Inverting `color` from %s to %s (to contrast with `fill = %s`).", color_old, color, fill))
    }

    lat <- long <- group <- NULL

    ret <-
      ggplot2::ggplot(
      data = data_state,
      ggplot2::aes(x = long, y = lat, group = group),
      color = color,
      fill = fill
    ) +
      ggplot2::geom_polygon(color = color, fill = fill) +
      ggplot2::coord_fixed(1.3)


    if(show_county) {
      data_county <- get_map_data_county(state)
      ret <-
        ret +
        ggplot2::geom_polygon(
          data = data_county,
          fill = fill,
          color = color
        )
      # Do this to add the state border back on top.
      ret <-
        ret +
        ggplot2::geom_polygon(
          fill = fill,
          color = color
        )
    }

    ret

  }