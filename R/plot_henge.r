# plot manhattanhenge
library(rayshader)
library(rayrender)
library(raster)
library(sf)
library(rgeos)
library(tidyverse)
library(osmdata)
library(OpenStreetMap)

lower_man <- sf::st_read("data/DA12_3D_Buildings_Multipatch.gdb.zip") %>%
  mutate(BIN = as.character(BIN),DOITT_ID = as.character(DOITT_ID))

# Manhattan west from 19th to 26th
# best view of Manhattanhenge is down 23rd
henge_extent <- extent(c(981000,992000,207000,214000))

# sf data has one row per building
# change single building multipolygon to collection of single polygons.
# each building may be multiple polygons with multiple heights.
# simplify
# delete empties that are created.
lm_1poly <- lower_man %>%
  st_cast("POLYGON") %>%
  st_simplify() %>%
  filter(!st_is_empty(SHAPE))

# extract Z dimension. Can be used as heightmap
lm_z <- lm_1poly %>%
  st_coordinates() %>%
  as_tibble() %>%
  group_by(L2) %>%
  summarise(X = X[1],Y = Y[1],Z = Z[1]) %>%
  ungroup() %>%
  select(-L2)

# add Z as new field to flattened objects.
lm_flat <- lm_1poly %>%
  st_zm() %>%
  bind_cols(lm_z) %>%
  mutate(footprint = st_area(SHAPE)) %>%
  select(BIN,SHAPE,Z,footprint)


# save space and shorten render.
# Collapse each building to a single polygon
# and create average height weighted by footprint of each polygon
lm_avg <- lm_flat %>%
  group_by(BIN) %>%
  summarise(SHAPE = st_union(SHAPE),
            Z = weighted.mean(as.numeric(Z),as.numeric(footprint)),
            footprint = sum(footprint))


save(lm_avg,file = "data/lm_avg.rdata")
load(file = "data/lm_avg.rdata")

# Manhattan west from 19th to 26th
# best view of Manhattanhenge is down 23rd

# henge_extent <- extent(c(981000,992000,207000,214000))

# mini to test
henge_extent <- extent(c(981000,983000,211000,213000))
henge_bbox <- bbox(henge_extent)

lm_crop <- st_crop(lm_avg,henge_extent)

# lmx <- extent(lm_crop)
# aspect <- (xmax(lmx) - xmin(lmx)) / (ymax(lmx) - ymin(lmx))
# long_side = 800
# elev_matrix_dummy <- matrix(0,
#                       nrow = long_side,
#                       ncol = long_side / aspect
# )

# LIDAR ground elevations
elev_file = "data/be_nyc_025.tif"
# elev_file = "data/23rd_osm.tiff"
elev_img <- raster::raster(elev_file) %>%
  crop(extent(lm_crop)) %>%
  raster::aggregate(fact = 20)

elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img),
                  buffer = 1000),
  ncol = nrow(elev_img),
  nrow = ncol(elev_img)
)

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

# OSM overlay
streetmap <- osmdata(henge_bbox)

rgl::clear3d()
elev_matrix %>%
  sphere_shade(texture = "bw") %>%
  plot_3d(elev_matrix,water = TRUE,zscale = 100)

render_polygons(lm_crop,
                  extent = extent(lm_crop),
                  data_column_top = "Z",
                  color = "grey",
                scale_data = 0.2,
                parallel = FALSE)

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


