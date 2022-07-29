# plot manhattanhenge
library(rayshader)
library(raster)
library(sf)
library(rgeos)
library(tidyverse)

lower_man <- sf::st_read("data/DA12_3D_Buildings_Multipatch.gdb.zip")

lm_flat <- as_Spatial(st_zm(lower_man))

extent(lm_flat)

reduce_extent <- function(lmx,ratio = 0.1){
  new_xmax = xmin(lmx) + (xmax(lmx) - xmin(lmx))*ratio
  new_ymax = ymin(lmx) + (ymax(lmx) - ymin(lmx))*ratio
  xmax(lmx) <- new_xmax
  ymax(lmx) <- new_ymax
  return(lmx)
}

lm_flat_sm <- crop(lm_flat,reduce_extent(extent(lm_flat),0.1))

lmx <- extent(lower_man)
elev_matrix <- matrix(0,
  ncol = xmax(lmx) - xmin(lmx),
  nrow = ymax(lmx) - ymin(lmx)
)


# BINS <- lm_flat_sm %>%
#   st_as_sf() %>%
#   as_tibble %>%
#   pull(BIN)
#
# lower_man_sm <- lower_man %>%
#   filter(BIN %in% BINS)

zscale = 10

rgl::clear3d()
elev_matrix %>%
  sphere_shade(texture = "bw") %>%
  render_polygons(lower_man) %>%
  plot_map()

#Plot in 3D
rgl::clear3d()
elev_matrix %>%
  sphere_shade(texture = "bw") %>%
#  add_shadow(raymat,0.3) %>%
  #  add_shadow(ambmat,0) %>%
  plot_3d(elev_matrix,zscale=zscale,zoom = .5)
