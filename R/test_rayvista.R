library(tidyverse)
library(rayshader)
volcano %>%
  sphere_shade() %>%
  plot_3d(volcano,zscale = 2)

render_highquality()

devtools::install_github("h-a-graham/rayvista", dependencies=TRUE)

library(rayvista)

.lat <- 57.21956608144513
.long <- -6.092690805001252

cuillins <- plot_3d_vista(lat = .lat, long = .long,show_vista = FALSE)

# set negative elevations to zero
cuillins$dem_matrix[cuillins$dem_matrix < 0] <- 0

rgl::clear3d()
cuillins$texture %>%
  plot_3d(cuillins$dem_matrix)
rayshader::render_snapshot(clear=TRUE)




# NYC
.lat <- 40.744757
.long <- -73.997263

# elevatr::set_opentopo_key(Sys.getenv("OPEN_TOPO_KEY"))
nyc <- plot_3d_vista(lat = .lat, long = .long,
                     elevation_src = "aws",
                     fill_holes = FALSE,
                     overlay_detail = 16,
                     radius = 1000,
                     show_vista = FALSE)

rgl::clear3d()
nyc

nyc$texture %>%
  # sphere_shade(texture = "bw") %>%
  plot_3d(nyc$dem_matrix,zscale = .5)


render_label(heightmap= nyc$dem_matrix, text='NYC', lat = .lat,
             long=.long, extent = attr(nyc$dem_matrix, 'extent'),altitude=600,
             clear_previous = T, zscale = .5)

render_compass()

render_scalebar(limits=c(
  round(dim(nyc$dem_matrix)[2]*attr(nyc$dem_matrix, 'resolution')/1000,1)),
  label_unit = 'km')

render_snapshot(clear=TRUE)



