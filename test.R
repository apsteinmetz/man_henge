# do we have buildings?
library(tidyverse)
library(rayshader)
library(rayrender)
library(raster)
library(sf)
library(OpenStreetMap)
#library(geoviz)
#library(reproj)
#library(terra)
library(jpeg)
#library(oceanmap)



# Manhattan west from 19th to 26th
# best view of Manhattanhenge is down 23rd
#henge_extent <- extent(c(981000,992000,207000,214000))

# mini to test
henge_extent <- extent(c(981000,983000,211000,213000))
henge_bbox <- bbox(henge_extent)


#elev_file = "data/lower_man_lidar.tif"
elev_file = "data/nyc_lidar_23rd.tif"
elev_img <- raster::raster(elev_file) %>%
   crop(extent(henge_extent)) %>%
#  raster::aggregate(fact = 1) %>%
  {.}

# get new extent for map overlay
latlon_extent <- projectRaster(elev_img,crs = longlat()) %>%
  extent()


# get map overlay
man_lr <- c(latlon_extent@ymin,latlon_extent@xmin)
man_ul <- c(latlon_extent@ymax,latlon_extent@xmax)
map_overlay <- OpenStreetMap::openmap(man_ul,man_lr,
                                      minNumTiles = 12,
                                      type = "bing",
                                      mergeTiles = TRUE)

map_ras <- raster(map_overlay)

jpeg(filename = "temp.jpg",
    width = ncol(elev_img),
    height = nrow(elev_img))
plotRGB(map_ras)
dev.off()

overlay <- jpeg::readJPEG("temp.jpg")

elmat = matrix(
  raster::extract(elev_img, raster::extent(elev_img),
                  method = 'simple'),
  nrow = ncol(elev_img),
  ncol = nrow(elev_img)
)

elmat[is.na(elmat)] <- 0
elmat[elmat < 0] <- 0


elmat %>%
  sphere_shade(texture = "bw") %>%
#  add_overlay(overlay,
#              alphalayer = .8) %>%
  rayshader::plot_map(elmat,
                      rotate = 0)

 rgl::clear3d()
 elmat %>%
   sphere_shade(texture = "bw") %>%
   add_water(detect_water(elmat)) %>%
#   add_overlay(overlay,
#               alphalayer = .8) %>%
   plot_3d(elmat,zscale = 5)

 # render_highquality()

