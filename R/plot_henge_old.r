# plot manhattanhenge
library(rayshader)

ras_crop <-  lower_man_ras %>%
  raster::crop(extent(lower_man_ras) *.2)



#elev_img <- raster::aggregate(lower_man_ras, fact = 3)
elev_img <- ras_crop

elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000),
  nrow = ncol(elev_img),
  ncol = nrow(elev_img)
)

zscale = 10

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

#Plot in 3D
rgl::clear3d()
elev_matrix %>%
  sphere_shade(texture = "bw") %>%
#  add_shadow(raymat,0.3) %>%
  #  add_shadow(ambmat,0) %>%
  plot_3d(elev_matrix,zscale=zscale,zoom = .5)
