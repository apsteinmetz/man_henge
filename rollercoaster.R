#rollercoaster
library(ggplot2)
library(rayrender)
library(rayshader)

ggdiamonds = ggplot(diamonds, aes(x, depth)) +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon", n = 200, bins = 100,contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")
ggdiamonds

plot_gg(ggdiamonds,width=7,height=7,scale=250,
        windowsize=c(1100,700),
        raytrace=FALSE,
        zoom = 0.55, phi = 30, triangulate = TRUE, max_error = 0)
render_snapshot()

save_obj("data/ggplot.obj")
rgl::rgl.close()

disk(radius=1000,y=-1,
  material=diffuse(checkerperiod = 6,checkercolor="#0d401b", color="#496651")) %>%
  add_object(obj_model("data/ggplot.obj", y=-0.02, texture=TRUE, scale_obj = 1/100)) %>%
  add_object(sphere(y=30,z=10,radius=5,material = light(intensity=40))) %>%
  render_scene(lookfrom=c(20,20,20),fov=0,ortho_dimensions=c(30,30), width=800,height=800)

library(rayimage)
library(raster)
plot_image("data/ggplot.png")
vals = locator(n=8)
vals$z = vals$y
vals$y = rep(5,length(vals$y))
selected_points = do.call(cbind,vals)
selected_points

loaded_texture = png::readPNG("data/ggplot.png")
dim(loaded_texture)

#Center at origin, flip, and scale by 1/100 to match our rayrender model
selected_points[,1] = (selected_points[,1] - 2100/2)/100
selected_points[,3] = -(selected_points[,3] - 2100/2)/100


# Now let’s plot it on top of the rayrender model to see what it looks like.
# We ensure the path is closed by setting closed = TRUE in the path() function.
# We’ll also generate some spheres to place at the key frames to mark their positions.
keylist = list()
for(i in 1:nrow(selected_points)) {
  keylist[[i]] = sphere(x=selected_points[i,1],
                        y=selected_points[i,2],
                        z=selected_points[i,3],radius=0.3,
                        material=diffuse(color="purple"))
}

keyscene = do.call(rbind,keylist)

disk(radius=1000,y=-1,
     material=diffuse(checkerperiod = 6,checkercolor="#0d401b", color="#496651")) %>%
  add_object(obj_model("data/ggplot.obj", y=-0.02, texture=TRUE, scale_obj = 1/100)) %>%
  add_object(sphere(y=30,z=-10,radius=5,material = light(intensity=40))) %>%
  add_object(path(points=selected_points,width=0.1, closed = TRUE,
                  material=diffuse(color="red"))) %>%
  add_object(keyscene) %>%
  render_scene(lookfrom = c(0, 10, 0), fov = 0, ortho_dimensions = c(30, 30), camera_up = c(0, 0, -1),
               width = 800, height = 800, samples = 250)


# Now comes the only bit of iterative manual work:
# we’re going to play with the y-coordinates to make this more “rollercoaster”
# and less “monorail”. I just adjusted each point's y-coordinate and repeatedly
# rendered the result until I got what I wanted. We’ll also nudge one of the
# points to avoid running into the data.

selected_points_new = selected_points
selected_points_new[,2] = c(0.3, 0.8, 4, 0.8, 0.5,
                            2.5, 1, 0.2, 1, 3,
                            2, 0.3, 2, 0.5, 2,
                            1.5, 0.9, 0.7, 0.3, 0.2)

# selected_points_new[17,1] = 1.5

keylist_adjusted = list()
for(i in 1:nrow(selected_points_new)) {
  keylist_adjusted[[i]] = sphere(x=selected_points_new[i,1],
                                 y=selected_points_new[i,2],
                                 z=selected_points_new[i,3],
                                 radius=0.3, material=diffuse(color="purple"))
}
keyscene_adj = do.call(rbind,keylist_adjusted)

disk(radius=1000,y=-1,
     material=diffuse(checkerperiod = 6,checkercolor="#0d401b", color="#496651")) %>%
  add_object(obj_model("data/ggplot.obj", y=-0.02, texture=TRUE, scale_obj = 1/100)) %>%
  add_object(sphere(y=30,z=10,radius=5,material = light(intensity=40))) %>%
  add_object(path(points=selected_points_new,width=0.1, closed = TRUE,
                  material=diffuse(color="red"))) %>%
  add_object(keyscene_adj) %>%
  render_scene(lookfrom = c(20, 20, 20),fov = 0,ortho_dimensions = c(30, 30),
               width = 800, height = 800, samples = 256)

# A real rollercoaster will have struts, and we can actually use the new
# animation function to generate these for us! Generating the camera motion is
# as simple as passing the above matrix of key frames to the
# generate_camera_motion() function. By default, the function places the camera
# at equally spaced intervals. Each row in the resulting data frame describes a
# camera position, orientation, and other properties. We can extract the camera
# position and use it to draw struts with cylinders.

camera_motion = generate_camera_motion(selected_points_new, closed=TRUE,
                                       frames = 360, constant_step = TRUE)
head(camera_motion)

strutlist = list()
for(i in 1:nrow(camera_motion)) {
  strutlist[[i]] = segment(start = as.numeric(camera_motion[i,1:3])-c(0,0.05,0),
                           end =as.numeric(camera_motion[i,1:3])-c(0,20,0),
                           radius=0.02, material=diffuse(color="grey10"))

}

strutscene = do.call(rbind,strutlist)

disk(radius=1000,y=-1,
     material=diffuse(checkerperiod = 6,checkercolor="#0d401b", color="#496651")) %>%
  add_object(obj_model("data/ggplot.obj", y=-0.02, texture=TRUE, scale_obj = 1/100)) %>%
  add_object(sphere(y=30,z=10,radius=5,material = light(intensity=40))) %>%
  add_object(path(points=selected_points_new,width = 0.1, closed = TRUE,
                  material=diffuse(color = "red"))) %>%
  add_object(keyscene_adj) %>%
  add_object(strutscene) %>%
  render_scene(lookfrom = c(20, 20, 20), fov = 0, ortho_dimensions = c(30, 30),
               width = 800, height = 800, samples = 256)

# Now let’s generate our camera motion. By default, generate_camera_motion()
# looks at the origin—in order to look along our curve, we need to pass lookat
# positions as well as lookfrom. We actually want to look along the same path as
# our camera position, but just slightly in front of the current position.
# Rayrender provides a helper argument in generate_camera_motion() to do just
# that: set offset_lookat = 1 and the function will look ahead that distance on
# the Bézier curve. So we just need to pass two copies of selected_points_new to
# the function. We can then pass this and our scene to render_animation() with
# debug = "preview" to quickly (in a few minutes) render a low-quality draft
# preview of the animation. I’ll also add an environment image to better orient
# us as we travel through the scene.

#Offset above track
selected_points_offset = selected_points_new
selected_points_offset[,2] = selected_points_offset[,2] + 0.15

camera_motion_real = generate_camera_motion(selected_points_offset, selected_points_offset,
                                            closed=TRUE, fovs = 90, constant_step = TRUE,
                                            frames=480,
                                            offset_lookat = 1)

disk(radius=1000,y=-1,
     material=diffuse(checkerperiod = 6,checkercolor="#0d401b", color="#496651")) %>%
  add_object(obj_model("ggplot.obj", y=-0.02, texture=TRUE, scale_obj = 1/100)) %>%
  add_object(path(points=selected_points_new,width=0.1, closed = TRUE,
                  material=diffuse(color="red"))) %>%
  add_object(strutscene) ->
  scene

render_animation(scene, camera_motion_real, environment_light = "quarry_03_4k.hdr",
                 debug="preview", filename="testroller", width=800,height=800)

