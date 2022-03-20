# animate manhattanhenge
# data from https://github.com/CityOfNewYork/nyc-geo-metadata/blob/master/Metadata/Metadata_3DBuildingModel.md#3-attribute-information
# gdb 12 and 13 are Manhattan

library(tidyverse)
library(sf)
library(raster)
library(fasterize)

#upper_man <- sf::st_read("data/DA13_3D_Buildings_Multipatch.gdb.zip")
lower_man <- sf::st_read("data/DA12_3D_Buildings_Multipatch.gdb.zip")
#manhattan <- st_join(lower_man,upper_man)

sf_poly_to_raster <- function(sf_obj,res = 10){
  # expects polygon or multipolygon geometry

  # reduce each builting to a single polygon
  sf_obj <- sf_obj %>%
    st_union(by_feature = TRUE)

  # split polygons into xyz points
  sf_obj_pts <- sf_obj %>%
    st_cast("POINT",warn = F)

  # extract the coordinates. We want Z
  sf_obj_coord <-
    sf_obj_pts %>%
    st_coordinates %>%
    as_tibble() %>%
    group_by(X,Y)

  # take max building height as whole building height.
  sf_obj_z <- sf_obj_pts %>%
    as_tibble %>%
    transmute(BIN,z = sf_obj_coord$Z) %>%
    dplyr::select(BIN,z) %>%
    group_by(BIN) %>%
    summarise(z = mean(z))

  # add Z feature back to spatial object
  sf_obj <- left_join(sf_obj,sf_obj_z,by="BIN")

  # Generate empty raster layer and rasterize points
  raster_template <- raster(crs = crs(lower_man),
                            ext = extent(lower_man),
                            res = res)

  sf_obj_raster <- fasterize(sf_obj,
                             raster_template,fun="max",
                             field="z",
                             background = 0)
  return(sf_obj_raster)

}

lower_man_ras <-  lower_man %>%
  # some structures resist conversion from multipoly to poly
  # and this breaks the raster conversion. I don't know why
  # this gets rid of them
  filter(DOITT_ID != 0) %>%
  sf_poly_to_raster()
