# plot manhattanhenge
library(rayshader)
library(rayrender)
library(raster)
library(sf)
library(tidyverse)
# library(osmdata)
library(geoviz)

lower_man <- sf::st_read("data/DA12_3D_Buildings_Multipatch.gdb.zip") %>%
  mutate(BIN = as.character(BIN),DOITT_ID = as.character(DOITT_ID))

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
