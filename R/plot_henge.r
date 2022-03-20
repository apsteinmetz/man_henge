# plot manhattanhenge
library(rayshader)

lower_man_ras
elev_img <- raster::aggregate(lower_man_ras, fact = 3)

elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000),
  nrow = ncol(elev_img),
  ncol = nrow(elev_img)
)

zscale = 30

ambmat <- ambient_shade(elev_matrix, zscale = zscale,
                        multicore = TRUE)
raymat <- ray_shade(elev_matrix, sunaltitude = 45,zscale = zscale,
                    lambert = TRUE,
                    multicore = TRUE)

elev_matrix %>%
  sphere_shade(texture = "bw") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat) %>%
  plot_map()
