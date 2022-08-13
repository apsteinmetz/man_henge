# path render test
library(tidyverse)
library(raster)
#remotes::install_github("tylermorganwall/rayrender")
library(rayrender)
#remotes::install_github("tylermorganwall/rayimage")
library(rayimage)
#remotes::install_github("tylermorganwall/rayrollercoaster")
library(rayrollercoaster)

SIZE_REDUCE = 1/30
#view scene
disk(radius=1000,y=-1,
     material=diffuse(checkerperiod = 6,checkercolor="#0d401b", color="#496651")) %>%
  add_object(obj_model("data/mini_man.obj", y=-0.02, texture=TRUE, scale_obj = SIZE_REDUCE)) %>%
  add_object(sphere(y=30,z=10,radius=5,material = light(intensity=40))) %>%
  render_scene(lookfrom=c(20,20,20),fov=0,ortho_dimensions=c(30,30), width=800,height=800)


#prepare to choose path
loaded_texture = png::readPNG("data/mini_man.png")
dim(loaded_texture)

plot_image("data/mini_man.png")
raw_vals = locator(n=5)
vals <- raw_vals
vals$z = vals$y
vals$y = rep(5,length(vals$y))

#x1,x2,y1,y2
viewport = par("usr")

selected_points = do.call(cbind,vals)
# selected_points
#Center at origin, flip, and scale by SIZE_REDUCE to match our rayrender model
selected_points[,1] = (selected_points[,1] - dim(loaded_texture)[1]/2)*SIZE_REDUCE
selected_points[,3] = -(selected_points[,3] - dim(loaded_texture)[2]/2)*SIZE_REDUCE

keylist = list()
for(i in 1:nrow(selected_points)) {
  keylist[[i]] = sphere(x=selected_points[i,1],
                        y=selected_points[i,2],
                        z=selected_points[i,3],radius=0.3,
                        material=diffuse(color="purple"))
}

keyscene = do.call(rbind,keylist)

scene = disk(radius=1000,y=-1,
     material=diffuse(checkerperiod = 6,checkercolor="#0d401b", color="#496651")) %>%
  add_object(obj_model("data/mini_man.obj", y=-0.02, texture=TRUE, scale_obj = SIZE_REDUCE)) %>%
  add_object(sphere(y=30,z=-10,radius=5,material = light(intensity=40))) %>%
  #add_object(path(points=selected_points,width=0.1, closed = FALSE,
  #                material=diffuse(color="red"))) %>%
  #add_object(keyscene) %>%
  {.}

  render_scene(scene,lookfrom = c(0, 10, 0), fov = 0, ortho_dimensions = c(30, 30), camera_up = c(0, 0, -1),
               width = 800, height = 800, samples = 1)

  generate_rayrender_coaster(scene, fov = 120)

