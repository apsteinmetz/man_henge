library(tidyverse)
library(sf)
library(fasterize)
library(raster)
library(stars)

find_image_coordinates <-
  function(x, y, bbox, image_width, image_height) {
    x_img <-
      round(image_width * (x- min(bbox$xmin, bbox$xmax)) /
              abs(bbox$xmin - bbox$xmax))
    y_img <-
      round(image_height * (y - min(bbox$ymin, bbox$ymax)) /
              abs(bbox$ymin - bbox$ymax))
    list(x = x_img, y = y_img)
  }


# 133 w 12th
#  filter(BIN == "1010624") %>%

bbox <- sf::st_bbox(lower_man)

sf_obj <-lower_man[1:100,]


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


# test using a subset and merge polygons into one per building


plot(man_raster)


img_xy <- find_image_coordinates(temp$X,temp$Y,bbox,
                       raster_template@ncols,raster_template@nrows)


temp_xyz <- tibble(x=img_xy$x,y=img_xy$y,z=temp$Z)

temp_xyz <- temp_xyz %>%
#  expand(x,y) %>%
  complete(expand(temp_xyz,x,y),fill = list(z=0)) %>%
  group_by(x,y) %>%
  summarise(z = mean(z),.groups = "drop") %>%
  filter(x >0,y>0) %>%
  pull(z) %>%
  matrix(nrow = raster_template@nrows,raster_template@ncols) %>%
  {.}

plot(temp_xyz)

faster

