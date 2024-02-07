mean_poverty <- mean(data$poverty, na.rm = TRUE)
median_poverty <- mean(data$poverty, na.rm = TRUE)
variance_poverty <- var(data$poverty, na.rm = TRUE)
sd_poverty <- sd(data$poverty, na.rm = TRUE)

cat("The mean poverty: ", mean_poverty, "\n")
cat("The median for poverty: ", median_poverty, "\n")
cat("The sd for poverty: ", sd_poverty, "\n")

boxplot(data$poverty, main="Boxplot of Poverty", xlab="Poverty Rate")

points <- st_as_sf(data.frame(id = 1:2, x = c(9.74, 9.75), y = c(2.45, 2.46)), coords = c("x", "y"), crs = 4326)
line_data <- data.frame(id = c(1, 1), x = c(9.74, 9.75), y = c(2.45, 2.46))
line_sf <- st_as_sf(line_data, coords = c("x", "y"), crs = 4326)
line_sf_line <- line_sf %>% group_by(id) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("LINESTRING")
plot(st_geometry(points), col = 'black', pch = 19)
plot(st_geometry(line_sf_line), col = 'red')
coords <- matrix(c(9.74, 2.45, 9.75, 2.45, 9.75, 2.46, 9.74, 2.46, 9.74, 2.45), byrow = TRUE, ncol = 2)
poly <- st_polygon(list(coords))
polygon_sf <- st_sf(geometry = st_sfc(poly), crs = 4326)
plot(polygon_sf, col = 'green')
additional_points <- data.frame(id = 3:5, x = c(9.76,9.77,9.78), y = c(2.47,2.48,2.49))
additional_points_sf <- st_as_sf(additional_points, coords = c("x","y"), crs = 4326)
all_points <-rbind(points, additional_points_sf)
plot(st_geometry(all_points), col = 'black', pch = 19)
#1.	Determine the mean Per Capita Income in the United States.
#a.	26093.12 
#2.	Discuss how altering the number of bins in a histogram affects its representation.
#a.	The more bins we have, the harder it is to spot the outliers since the histogram will have a bigger spread. It also shows a bigger skew compared to fewer bins therefore it contains more details of the data. 
#3.	Compare the insights gained from a histogram versus those from a boxplot.
#a.	The boxplot shows more of the data being outliers but the data is more squashed together so it is harder to accurately display the data.  The histogram shows a bit more of a uniform distribution while the boxplot shows a clear skew with the amount of outliers. 
#4.	Suggest the most effective transformation for normalizing the Unemployment Rate data.
#a.	The most effective transformation is to put it into a quantile. The quantile transformation will account for the outliers to make the data the most uniform it can be. 
#5.	Categorize the spatial dimension that trees occupy.
#a.	Point 
