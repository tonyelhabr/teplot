

# # Modified from https://mgimond.github.io/Spatial/interpolation-in-r.html.
# convert_df_to_spdf <-
#   function(data = NULL,
#            spdf_base = NULL,
#            col_lon = "lon",
#            col_lat = "lat") {
#     lon <- data[, col_lon]
#     lat <- data[, col_lat]
#     spdf <-
#       sp::SpatialPointsDataFrame(cbind(data$lon, data$lat),
#                                  data,
#                                  match.ID = TRUE)
#     spdf@bbox <- spdf_base@bbox
#     sp::proj4string(spdf) <- sp::proj4string(spdf_base)
#     spdf
#   }
#
# # Modified from https://mgimond.github.io/Spatial/interpolation-in-r.html.
# convert_spointsdf_to_spolysdf <-
#   function(spdf = NULL) {
#     th <-
#       methods::as(spatstat::dirichlet(spatstat::as.ppp(spdf)), "SpatialPolygons")
#     sp::proj4string(th) <- sp::proj4string(spdf)
#     th_z <- sp::over(th, spdf, fn = mean)
#     spdf_tess <- sp::SpatialPolygonsDataFrame(th, th_z)
#     spdf_tess
#   }
#
# # Modified from https://mgimond.github.io/Spatial/interpolation-in-r.html.
# create_spdf_tess_trimmed <-
#   function(spdf = NULL, spdf_base = NULL) {
#     spdf_tess <- convert_spointsdf_to_spolysdf(spdf)
#     spdf_tess_trimmed <- raster::intersect(spdf_base, spdf_tess)
#     spdf_tess_trimmed
#   }
#
# get_price_idw_raster <-
#   function(spdf = NULL,
#            formula = NULL,
#            grid_n = 50000,
#            idw_idp = 2.0) {
#     grd <-
#       data.frame(sp::spsample(spdf, "regular", n = grid_n))
#     names(grd) <- c("lon", "lat")
#     sp::coordinates(grd) <- c("lon", "lat")
#     sp::gridded(grd) <- TRUE
#     sp::fullgrid(grd) <- TRUE
#     sp::proj4string(grd) <- sp::proj4string(spdf)
#     # Since this is written directly in C, not sure how to suprress message.
#     # Also, need to not hard-code this formula.
#     idw_spdf <-
#       gstat::idw(as.formula(formula),
#                  spdf,
#                  newdata = grd,
#                  idp = idw_idp)
#     r <- raster::raster(idw_spdf)
#     r
#   }
#
#
# get_price_idw_raster_trimmed <-
#   function(spdf = NULL,
#            spdf_base = NULL,
#            grid_n = 50000,
#            idw_idp = 2.0) {
#     idw_spdf <- get_price_idw_raster(spdf, grid_n, idw_idp)
#     r <- raster::raster(idw_spdf)
#     r_trimmed <- raster::mask(r, spdf_base)
#     r_m
#   }
