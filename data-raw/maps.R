
library("ggmap")
library("sp")
library("tmap")

ggmap_stamen_tx_raw <-
  ggmap::get_map(
    location = "san angelo, tx",
    zoom = 6,
    source = "stamen",
    # maptype = "toner"
    maptype = "toner-background"
  )

save(ggmap_stamen_tx_raw, file = file.path("data", "ggmap_stamen_tx_raw.rdata"))
devtools::use_data(ggmap_stamen_tx_raw, overwrite = T)

ggmap_stamen_tx <-
  ggmap::ggmap(ggmap_stamen_tx_raw) +
  ggplot2::theme(axis.text = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank())
save(ggmap_stamen_tx, file = file.path("data", "ggmap_stamen_tx.rdata"))
devtools::use_data(ggmap_stamen_tx, overwrite = T)


spdf_tx_gz <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/texas.rds"))
spdf_tx_raw <- readRDS(spdf_tx_gz)
spdf_tx <- sp::spTransform(spdf_tx_raw, sp::CRS("+proj=longlat +datum=WGS84"))
save(spdf_tx, file = file.path("data", "spdf_tx.rdata"))
devtools::use_data(spdf_tx, overwrite = T)

tmap_tx <-
  tmap::tm_shape(spdf_tx) +
  tmap::tm_polygons(col = "white", alpha = 0)
save(tmap_tx, file = file.path("data", "tmap_tx.rdata"))
devtools::use_data(tmap_tx, overwrite = T)

spdf_tx_precip_gz <- gzcon(url("http://colby.edu/~mgimond/Spatial/Data/precip.rds"))
spdf_tx_precip <- readRDS(spdf_tx_precip_gz)
save(spdf_tx_precip, file = file.path("data", "spdf_tx_precip.rdata"))
devtools::use_data(spdf_tx_precip, overwrite = T)

