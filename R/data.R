
#' Texas map for \code{ggplot2} maps
#'
#' @description  \code{ggmap} base layer for state of Texas.
#' @details Raw download using \code{ggmap} package.
#' @format ggmap
"ggmap_stamen_tx_raw"

#' Texas map for \code{ggplot2} maps
#'
#' @description  \code{gg} base layer for state of Texas.
#' @details Transformed version of raw ggmap object without axis text or titles.
#' @format gg
"ggmap_stamen_tx"

#' Texas map for \code{ggplot2} maps
#'
#' @description SpatialPolygonsDataFrame layer for state of Texas.
#' @details Converted to \code{sp::CRS("+proj=longlat +datum=WGS84")}.
#' @source \url{http://colby.edu/~mgimond/Spatial/Data//texas.rds}
#' @seealso \url{https://mgimond.github.io/Spatial/interpolation-in-r.html}
#' @format SpatialPolygonsDataFrame
"spdf_tx"

#' Texas map for \code{ggplot2} maps
#'
#' @description tmap layer for state of Texas.
#' @details Transformed version of \code{spdf_tx}, where \code{tmap::tm_polygons(col = "white", alpha = 0)}
#' has been called after conversion to a tmap object (via \code{tmap::tm_shape(spdf_tx)}).
#' @seealso \url{https://mgimond.github.io/Spatial/interpolation-in-r.html}
#' @format tmap
"tmap_tx"

#' Data for \code{spdf_tx} map example
#'
#' @description Data to be used for examples with SpatialPolygonsDataFrame class data.
#' @details No transformations applied to the raw data.
#' @source \url{http://colby.edu/~mgimond/Spatial/Data/pricip.rds}
##' @seealso \url{https://mgimond.github.io/Spatial/interpolation-in-r.html}
#' @format SpatialPointsDataFrame
"spdf_tx_precip"