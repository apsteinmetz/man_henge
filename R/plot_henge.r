# plot manhattanhenge
library(rayshader)
library(rayrender)
library(raster)
library(sf)
library(tidyverse)
# library(osmdata)
library(geoviz)
library(suncalc)

load(file = "data/lm_avg.rdata")

# 10 is low, 1 is highest
DETAIL_LEVEL = 1

# Manhattan west from 19th to 26th
# best view of Manhattanhenge is down 23rd
# henge_extent <- extent(c(981000,992000,207000,214000))

# mini to test
henge_extent <- extent(c(981000,983000,211000,213000))
henge_bbox <- bbox(henge_extent)

lm_crop <- st_crop(lm_avg,henge_extent)

# LIDAR ground elevations
# elev_file = "data/be_nyc_025.tif"
elev_file = "data/nyc_lidar_23rd.tif"
elev_img <- raster::raster(elev_file) %>%
  crop(extent(lm_crop)) %>%
  raster::aggregate(fact = DETAIL_LEVEL)
  #raster::disaggregate(fact = 2)

elmat = matrix(
  raster::extract(elev_img, raster::extent(elev_img), method = 'bilinear'),
  nrow = ncol(elev_img),
  ncol = nrow(elev_img)
)
elmat[is.na(elmat)] <- 0
elmat[elmat < 0] <- 0

# # quick and dirty using polygon to raster
# # alternate using heightmap
# lm_xyz <- raster(crs = crs(lm_crop),
#                  vals = 0,
#                  resolution = c(20,20),
#                  ext = extent(lm_crop)) %>%
#   rasterize(select(lm_crop,SHAPE,Z), .,background = 0)
#
# elev_matrix_bldg <- matrix(
#   raster::extract(lm_xyz, raster::extent(lm_crop),
#                   buffer = 1000),
#   ncol = nrow(lm_xyz),
#   nrow = ncol(lm_xyz)
# )
# elev_matrix <- elev_matrix + elev_matrix_bldg

overlay_image_toner <-
  slippy_overlay(elev_img,
                 image_source = "stamen",
                 image_type = "toner",
                 png_opacity = 0.5)

overlay_image_wc <-
  slippy_overlay(elev_img,
                 image_source = "stamen",
                 image_type = "watercolor",
                 png_opacity = 0.5)



# manhattanhenge sunangle 119/299
# manhattanhenge scene rotation (theta) 61/241

sunangle = 119
zscale = 20

scene <- elmat %>%
  sphere_shade(sunangle = sunangle, texture = "bw") %>%
  # add_overlay(elevation_overlay) %>%
  add_overlay(overlay_image_wc) %>%
  add_overlay(overlay_image_toner) %>%
  {.}


#Render the 'rayshader' scene

rgl::clear3d()
scene %>%
  plot_3d(elmat,zscale = zscale,
          phi = 2,
          theta = 61,
          zoom = .2,
          fov = 1)


render_polygons(lm_crop,
                  extent = extent(lm_crop),
                  data_column_top = "Z",
                  color = "grey",
                light_direction = sunangle,
                light_altitude = 45,
                scale_data = .8/(DETAIL_LEVEL),
                parallel = FALSE

                )



system.time(
newscene <- render_highquality(lightaltitude = 30,
                   lightdirection = 299,
                   lightsize = 5000,
                   lightintensity = 1000,
                   parallel = TRUE,
                   camera_lookat = c(0,0,0),
                   print_scene_info = TRUE)
)
render_snapshot()

lmx <- extent(lower_man)
aspect <- (xmax(lmx) - xmin(lmx)) / (ymax(lmx) - ymin(lmx))
long_side = 540
elev_matrix <- matrix(0,
                      ncol = long_side,
                      nrow = long_side / aspect
)

crop_tibble <- function(xyz,.extent){
  xyz <- filter(xyz,X >= .extent@xmin)
  xyz <- filter(xyz,X <= .extent@xmax)
  xyz <- filter(xyz,Y >= .extent@ymin)
  xyz <- filter(xyz,Y <= .extent@ymax)
  return (xyz)
}


