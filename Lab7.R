
library(sf)
library(gstat)
library(sp)
library(raster)
library(ggplot2)
# Load the point data from a GeoPackage
vancouver_data <- st_read("/Users/nhutnguyen/Downloads/Vancouver_Cen2021 (2).gpkg")


vancouver_data_union <- st_union(st_geometry(vancouver_data))

bbox <- st_bbox(vancouver_data) #This creates a bounding box around our study area
grd <- st_make_grid(st_as_sfc(bbox), cellsize = 0.0035) # This creates a grid of points. Adjust cellsize for different resolutions
my_sf_grid <- st_sf(geometry = grd)


#Now we need to find the centroid of our polygons. The interpolation
#algorithm will estimate values inbetween these centroids
centroids <- st_centroid(vancouver_data)

#you should receive a warning sign about the assumption of the 
#attribute being consistent over the geometry area. This should
#sound familiar as this is also an assumption on choropleth maps.

#here's how we do Inverse Distance Weighted Interpolation
idw_result <- gstat::idw(formula = Population ~ 1,  # Variable to interpolate
                         locations = centroids,        # Known points
                         newdata = my_sf_grid,         # Points or grid to predict
                         idp = 2,                     # Power parameter
                         nmax = 30,                   # Max number of points to use
                         maxdist = 1000)              # Max distance for points to influence
idw_sf <- st_as_sf(idw_result, coords = c("x", "y"), crs = 4326)

#plot our map 
ggplot(data = idw_sf) +
  geom_sf(aes(fill = var1.pred), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "Vancouver Population", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
clipped_idw <- st_intersection(idw_sf, vancouver_data) #this might take a minute to run

#####Questions####
########gi#########
#1. IDW assumes that the variable of interest decreases in influence as the distance increases from its measurement location.
#It also uses a fixed math formula to calculate unknown points using weighted averages of the values of the nearest points 
#While Kriging takes in account spatial correlation to estimate variable values which is more statistics based. provides estimates by weighting
#the surrounding observed values to derive a prediction for unmeasured locations.

#2. Contour lines enhance a map's quality by providing clear representations of the terrain which allows readers to understand the topography
#at a quick glance. It can highlight elevations, slopes and landform shapes but it can be difficult to read if the lines are too densely packed
#or drawn inaccurately.
