####Point,Line,Polygon
# Define points
point1 <- st_point(c(0, 5))  # Example: a point at (0,0)
point2 <- st_point(c(15, 6))  # Example: a point at (1,1)

# Create a simple feature column for points
points_sfc <- st_sfc(point1, point2)

# Define polygons
polygon1 <- st_polygon(list(matrix(c(0,5, 15,5, 15,6, 5,6, 0,5), ncol = 2, byrow = TRUE)))
polygon2 <- st_polygon(list(matrix(c(3,4, 5,4, 5,3, 4,3, 3,4), ncol = 2, byrow = TRUE)))

# Create a simple feature column for polygons
polygon_sfc <- st_sfc(polygon1, polygon2)

# Define multilinestrings
multilinestring1 <- st_multilinestring(list(matrix(c(0,5, 0,15, 15,6), ncol = 2, byrow = TRUE)))
multilinestring2 <- st_multilinestring(list(matrix(c(15,6, 15,3, 3,5), ncol = 2, byrow = TRUE)))

# Create a simple feature column for multilinestrings
multilinestring_sfc <- st_sfc(multilinestring1, multilinestring2)

# Print the created sfc objects
print(points_sfc)
print(polygon_sfc)
print(multilinestring_sfc)
###Buffer
plot(points_sfc)
plot(polygon_sfc)
plot(multilinestring_sfc)
buffered_sf <- st_buffer(mixed_sf, dist = 0.1)
###union
union_polygon <- st_union(polygons_sf$geometry)

# Create an sf object for the unioned polygon
union_sf <- st_sf(geometry = st_sfc(union_polygon))

# Plotting the original polygons and their union
par(mfrow = c(1, 3))
plot(polygons_sf, main = "Original Polygons")
plot(st_geometry(union_sf), col = 'green', main = "Union Polygon")
# Plotting
par(mfrow = c(1, 2))
plot(mixed_sf, col = 'red', main = "Original Geometries")
plot(buffered_sf, col = 'blue', main = "Buffered Geometries")
polygons_sf <- st_sf(geometry = polygon_sfc)

polygon1 <- st_polygon(list(matrix(c(0,5, 15,5, 15,6, 0,6, 0,5), ncol = 2, byrow = TRUE)))
polygon2 <- st_polygon(list(matrix(c(3,4, 5,4, 5,3, 3,3, 3,4), ncol = 2, byrow = TRUE)))

# Union
polygon_sfc <- st_sfc(polygon1, polygon2)
multilinestring1 <- st_multilinestring(list(matrix(c(0,5, 0,15, 15,6), ncol = 2, byrow = TRUE)))
multilinestring2 <- st_multilinestring(list(matrix(c(15,6, 15,3, 3,5), ncol = 2, byrow = TRUE)))
multilinestring_sfc <- st_sfc(multilinestring1, multilinestring2)
polygons_sf <- st_sf(geometry = polygon_sfc)
union_polygon <- st_union(polygons_sf$geometry)
union_sf <- st_sf(geometry = st_sfc(union_polygon))
par(mfrow = c(1, 3))
plot(polygons_sf, main = "Original Polygons")
plot(st_geometry(union_sf), col = 'green', main = "Union Polygon") 
install.packages("rgdal")
install.packages("raster")
install.packages("rgdal")
library(raster)
####JPEG

set.seed(123) # For reproducibility
layer1 <- raster(nrows=10, ncols=10, xmn=0, xmx=10, ymn=0, ymx=10)
layer2 <- raster(nrows=10, ncols=10, xmn=0, xmx=10, ymn=0, ymx=10)
layer3 <- raster(nrows=10, ncols=10, xmn=0, xmx=10, ymn=0, ymx=10)
values(layer1) <- runif(ncell(layer1)) * 255
values(layer2) <- runif(ncell(layer2)) * 255
values(layer3) <- runif(ncell(layer3)) * 255
raster_stack <- stack(layer1, layer2, layer3)
output_jpg <- "raster_output.jpg"
# Use plot to visualize (optional, mainly for checking)
plot(raster_stack[[1]])
# Save the plot or raster as a JPG
# There's no direct function in raster or sf to save as JPG, so use jpeg() from the grDevices package
jpeg(filename=output_jpg, width=800, height=600)
plot(raster_stack[[1]], axes=F, box=F) # Example for the first layer, adjust for composites
dev.off() 
library(raster)
library(ggplot2)
set.seed(42)
####tree
canopy_height <- raster(nrow=100, ncol=100, xmn=0, xmx=100, ymn=0, ymx=100)
values(canopy_height) <- runif(ncell(canopy_height), min=0, max=30) # Heights between 0 and 30 meters
plot(canopy_height, main="Simulated Tree Canopy Height", xlab="Meters", ylab="Meters", col=terrain.colors(256))
canopy_df <- as.data.frame(rasterToPoints(canopy_height))
colnames(canopy_df) <- c("Longitude", "Latitude", "Height")
ggplot(canopy_df, aes(x=Longitude, y=Latitude, fill=Height)) +
  geom_tile() +
  scale_fill_gradientn(colors=terrain.colors(256)) +
  labs(title="Tree Canopy Height", x="Meters", y="Meters", fill="Height (m)") +
  theme_minimal()
####Mariage###
mi_counties <- st_read("/Users/nhutnguyen/Desktop/geo426/geo426/mi_counties 2")
library(sf)
shapefile_path <- "/Users/nhutnguyen/Desktop/geo426/geo426/Counties_(v17a).shp"
mi_counties$NAME <- gsub("\\.", "", mi_counties$NAME)
marriages <- read.csv ("/Users/nhutnguyen/Desktop/geo426/geo426/MI_Mariage_rates_2022.csv")
head(mi_counties)
head(marriages)

# Join the marriage data with the spatial data of Michigan counties
# and convert certain columns to numeric for analysis
mi_counties <- mi_counties %>% 
  left_join(marriages, by = c('NAME' = 'County')) %>% 
  mutate_at(c("Marriage.Number", "Marriage.Rate", "Divorce.Number", 
              "Divorce.Rate", "Population"), as.numeric)

# Calculate state-wide average marriage rate
state_rate = sum(mi_counties$Marriage.Number) / sum(mi_counties$Population)
# Calculate expected marriage rate for each county based on state average
mi_counties$marriageExp <- state_rate * mi_counties$Population
# Compute relative risk of marriage for each county
mi_counties$relRisk <- ifelse(mi_counties$marriageExp > 0, 100 * (mi_counties$Marriage.Number / mi_counties$marriageExp), 0)

# Plot the spatial data of Michigan counties
plot(mi_counties)
ggplot(mi_counties) +
  geom_sf(aes(fill = relRisk)) +  # Color counties based on relative risk
  theme_minimal()  # Use minimal theme for better visualization

# Close any open graphics devices
dev.off()

# Generate color palettes using RColorBrewer for choropleth maps
mp5 <- brewer.pal(5, "Greens")  # 5-class Green palette
mp7 <- brewer.pal(7, "Greens")  # 7-class Green palette
mp9 <- brewer.pal(9, "Greens")  # 9-class Green palette
choroLayer(x = mi_counties, var = "Marriage.Number",
           method = "equal", nclass = 7,
           col = mp7,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Marriage \nTotal Marriages: \nEqual Area, 7 Colors")
# Question 1: The main difference between sf and terra is sf is used for vector data while terra is used for
# raster data. Terra handles larger datasets more easily while sf is simple feature so its more useful
# for GIS applications. 

#question 2: Some functions are st_boundary(x) Creates a polygon that encompasses the full extent of the geometry
#st_buffer(x, dist, nQuadSegs) Creates a polygon covering all points of the geometry within a given distance
#st_centroid(x, ..., of_largest_polygon) Creates a point at the geometric centre of the geometry

#question 3: First use st_point to define your points then use st_sfc to create a column then define the polygons
#create a sf for the polygon with polygon_sfc define the multilinestrings then create a sf for it using
# mulitilinesting_sfc. Finally use the plot function

#question 4: Using the filter() function then setting your criteria 

#question 5: Using the sf, ggplot, and st_stats functions to input your data into a graph/map.In the ggplot
# input how the colors should be depicted. Ex: geom_sf(data = interpolated_temp, aes(fill = var1.pred), color = NA) + # var1.pred holds interpolated values
#scale_fill_viridis_c() + labs(fill = "Temperature") +theme_minimal()
