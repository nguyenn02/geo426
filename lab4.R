####Germany Jpeg####
####################
# Load world map data
world <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

# 1. Global Scale Map
ggplot(data = world) +
  geom_sf() +
  labs(title = "World Map - Global Scale") +
  theme_minimal()
# 3. Country Scale Map - Focusing on Germany
germany <- st_crop(europe, xmin = 5, xmax = 15, ymin = 47, ymax = 55)
ggplot(data = germany) +
  geom_sf() +
  labs(title = "Germany Map - Country Scale") +
  theme_minimal()
#create Jpeg

# Plot the map focused on Germany
p_germany <- ggplot(data = germany) +
  geom_sf() +
  labs(title = "Germany Map - Country Scale") +
  theme_minimal()

# Save the plot to a JPEG file
ggsave("germany_map_country_scale.jpg", plot = p_germany, width = 10, height = 8, dpi = 300)

####Resolution Change####
########################
data(volcano)
volcano_raster <- raster(volcano)
plot(volcano_raster, main = "High-Resolution Raster")
volcano_lowres3 <- aggregate(volcano_raster, fact = 30)
plot(volcano_lowres3, main = "Lower Resolution Raster (Factor = 30)")
lowres3_max <- cellStats(volcano_lowres3, stat = 'max')
cat("Max Elevation (Lower Resolution - Factor 30):", lowres3_max, "\n")

####Smoothing####
#################
#A way around this is to store the areas in the dataset before the simplification
plot(st_geometry(nc), main="Original")
smoothed_nc <- smooth(nc,method = 'ksmooth')
smoothed_nc_spline <- smooth(nc,method = 'spline')
par(mfrow = c(2, 2))
plot(st_geometry(smoothed_nc), main="ksmoothed")
plot (st_geometry(smoothed_nc_spline), main ="splinesmoothed")

####Convex/Concave####
######################
set.seed(35)
n_points <- 45
points <- matrix(runif(10 * n_points, min = -4, max = 10), ncol = 2)
colnames(points) <- c("x", "y")
points_sf <- st_as_sf(data.frame(points), coords = c("x", "y"), crs = 4326)
# Create the convex hull as a polygon
convex_hull_polygon <- st_convex_hull(st_combine(points_sf))

# Create the concave hull as a polygon
concave_hull_polygon <- st_sf(geometry = concaveman(points_sf))
plot(st_geometry(convex_hull_polygon), col = 'pink', border = 'purple', lwd = 2, main = "Woods Area (Convex)")
plot(st_geometry(concave_hull_polygon), col = 'pink', border = 'purple', lwd = 2, main = "Woods Area (Convave)")

#1	When would you want to change the scale of your data?
# The scale of your data should depend on what type of analysis you want to perform. 
#Having a higher resolution scale can make the data visual more easy to interpret 

# 2.	What are some pros and cons of changing your raster resolution to have less resolution?
#Some pros are reducing the file size so it can render faster, simplifying the data so it easier
# to work with understand, and its better to comparability 
#Some cons are loss of details, potential for misinterpretation, and generalization which can cause 
#blurriness with what is being communicated or boundaries

# 3.	When would you want to generalize your polygon data (think countries, states, counties, etc)
#Generalization is used to improve map readability, purpose, audience and scale. 

#4.	How is simplifying line data different from smoothing the line data?
#Simplifying line data reduces the details and size which makes it less clutter to look at while
# smoothing is removing jagged edges for visual clarity 

# 5.	When we are aggregating point to polygons in what situation do you want to use a concave approach? A convex approach?
#The choice between concave or convex depends on what accuracy you want to display in representation
#of the shape distribution and simplicity. Concave is used for detailed and more accurate shapes while
#convex is used for broad and simplier views.

