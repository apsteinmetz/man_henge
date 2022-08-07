# read LIDAR TIF files from ZIP

library(tidyverse)
library(raster)
library(rayshader)



tif_file <- "data/NYC_dem/NYC_000_001.tif"

fulltif = raster::raster(tif_file)
oe <- extent(fulltif)

crop_extent <- function(oe,fact = 0.1){
  ne <- oe
  ne@xmin <- oe@xmax - (oe@xmax-oe@xmin) * fact
  ne@ymin <- oe@ymax - (oe@ymax-oe@ymin) * fact
  return(ne)
}

localtif <- raster::aggregate(fulltif,20)

save(localtif,file = "data/localras.rdata")
load(file = "data/localras.rdata")


#And convert it to a matrix:
elmat <- raster_to_matrix(localtif)
elmat <- elmat[900:1250,500:1000]

hillshade <- elmat %>% sphere_shade(texture = "bw")
plot_map(hillshade)

rgl::clear3d()

plot_3d(hillshade,elmat)
render_highquality()

hist(elmat)
