################################################################################
# Author: Josh Vertalka
# Date: 3-16-2024
# Purpose: Lab 10 of Geography 426
# This lab's focus is on 3D elevation Mapping.  
################################################################################
options(repos = c(
  tylermorganwall = 'https://tylermorganwall.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'
))

install.packages('rayshader') #Take a break - This will take a minute to run. 
install.packages(c("rayshader", "raster", "sp"))
install.packages('rgdal')
library(rayshader)
library(sp)
library(raster)
library(scales)

options(rgl.useNULL = FALSE)
library(ggplot2)
library(whitebox)
library(rayshader)
library(rayrender)
library(raster)
library(spatstat)
library(spatstat.utils)
library(suncalc)
library(sp)
library(lubridate)
library(rgdal)
library(magick)
################################################################################
################################################################################
################################################################################

elevation1 = raster::raster("/Users/nhutnguyen/Downloads/N37W113.hgt")
elevation2 = raster::raster("/Users/nhutnguyen/Downloads/N37W114.hgt")

zion_elevation = raster::merge(elevation1,elevation2)

height_shade(raster_to_matrix(zion_elevation)) %>%
  plot_map()
zion_r = raster::raster("/Users/nhutnguyen/Downloads/LC08_L1TP_038034_20191101_20200825_02_T1_B4.TIF")
zion_g = raster::raster("/Users/nhutnguyen/Downloads/LC08_L1TP_038034_20191101_20200825_02_T1_B3.TIF")
zion_b = raster::raster("/Users/nhutnguyen/Downloads/LC08_L1TP_038034_20191101_20200825_02_T1_B2.TIF")

zion_rbg = raster::stack(zion_r, zion_g, zion_b)
raster::plotRGB(zion_rbg, scale=255^2)
zion_rbg_corrected = sqrt(raster::stack(zion_r, zion_g, zion_b))
raster::plotRGB(zion_rbg_corrected)
raster::crs(zion_r)
raster::crs(zion_elevation)
crs(zion_r)
zion_elevation_utm = raster::projectRaster(zion_elevation, crs = crs(zion_r), method = "bilinear")
crs(zion_elevation_utm)
bottom_left = c(y=-113.155277, x=37.116253)
top_right   = c(y=-112.832502, x=37.414948)

extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm = sp::spTransform(extent_latlong, raster::crs(zion_elevation_utm))

e = raster::extent(extent_utm)
e
zion_rgb_cropped = raster::crop(zion_rbg_corrected, e)
elevation_cropped = raster::crop(zion_elevation_utm, e)

names(zion_rgb_cropped) = c("r","g","b")

zion_r_cropped = rayshader::raster_to_matrix(zion_rgb_cropped$r)
zion_g_cropped = rayshader::raster_to_matrix(zion_rgb_cropped$g)
zion_b_cropped = rayshader::raster_to_matrix(zion_rgb_cropped$b)

zionel_matrix = rayshader::raster_to_matrix(elevation_cropped)

zion_rgb_array = array(0,dim=c(nrow(zion_r_cropped),ncol(zion_r_cropped),3))

zion_rgb_array[,,1] = zion_r_cropped/255 #Red layer
zion_rgb_array[,,2] = zion_g_cropped/255 #Blue layer
zion_rgb_array[,,3] = zion_b_cropped/255 #Green layer

zion_rgb_array = aperm(zion_rgb_array, c(2,1,3))

plot_map(zion_rgb_array)
zion_rgb_contrast = scales::rescale(zion_rgb_array,to=c(0,1))

plot_map(zion_rgb_contrast)
plot_3d(zion_rgb_contrast, zionel_matrix, windowsize = c(1100,900), zscale = 15, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_snapshot(title_text = "Zion National Park, Utah | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
angles= seq(0,360,length.out = 1441)[-1]
for(i in 1:1440) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("zionpark%i.png", i), 
                  title_text = "Zion National Park, Utah | Imagery: Landsat 8 | DEM: 30m SRTM",
                  title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
}
rgl::rgl.close()

#av::av_encode_video(sprintf("zionpark%d.png",seq(1,1440,by=1)), framerate = 30,
# output = "zionpark.mp4")

rgl::rgl.close()
system("ffmpeg -framerate 60 -i zionpark%d.png -pix_fmt yuv420p zionpark.mp4")
