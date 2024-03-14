###Chloropleth map###
#####################
my_color_names<-c("mistyrose", "mistyrose1", "mistyrose2", "mistyrose3")
my_colors <- as.hexmode( c(256^(2:0) %*% col2rgb(my_color_names)) )
my_colors<-paste0("#", my_colors)
my_colors <- colorRampPalette(brewer.pal(8, "RdPu"))(length(my_color_names))
mf_map(mi_counties, 
       var = "Marriage.Number",
       type = "choro",
       pal = my_colors,
       nbreaks = length(my_colors)) 
jpeg("mi_counties_map.jpeg", width = 800, height = 600)
####4 class ordinal map####
###########################
ordinalBrewer <- brewer.pal(n = 4, name = "YlGn")
specialColors <- c("very high" = "#006837", "no data" = "#CCCCCC")  # Example colors
combinedPal <- c(ordinalBrewer, specialColors)
mf_map(mi_counties, 
       var = "OrdinalMarriages",
       type = "typo",
       pal = combinedPal,
       nbreaks = 4)  # Adjusting for 4 classes explicitly if your function supports this parameter
####reproject###
################
mi_counties <- st_read("/Users/nhutnguyen/Desktop/geo426/Lab 5/mi_counties/Counties_(v17a).shp")
mi_counties$NAME <- gsub("\\.", "", mi_counties$NAME)
plot(st_geometry(mi_counties))
webMercator<-"+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs +type=crs"
mi_counties<-st_transform(mi_counties, 4087)
plot(st_geometry(mi_counties))

####Lituania###
###############
library(sf)
library(dplyr)
world <- st_read("/Users/nhutnguyen/Desktop/geo426/Lab 5/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
Lithuania <- world %>% filter(NAME == "Lithuania")

# Reproject to WGS 1984
Lithuania_wgs84 <- st_transform(Lithuania, crs = 4326)

# Reproject to Equal Area projection
Lithuania_equal_area <- st_transform(Lithuania, crs = 3035)

# Open JPEG device
jpeg("Lithuania_Comparison.jpeg", width = 1600, height = 800)
par(mfrow = c(1, 2))

plot(st_geometry(Lithuania_wgs84), main = "Lithuania in WGS 1984")
plot(st_geometry(Lithuania_equal_area), main = "Lithuania in Equal Area Projection")
dev.off()

####Cholorpleth map for MI####
##############################
mi_counties <- st_read("/Users/nhutnguyen/Desktop/geo426/Lab 5/mi_counties/Counties_(v17a).shp")
mi_counties$NAME <- gsub("\\.", "", mi_counties$NAME)
marriages <- read.csv("/Users/nhutnguyen/Desktop/geo426/Lab 5/MI_Mariage_rates_2022.csv")
mi_counties_reprojected <- st_transform(mi_counties, crs = 5070)
color_palette <- brewer.pal(9, "YlOrRd")

ggplot(data = mi_counties_reprojected) +
  geom_sf(aes(fill = RelativeRisk), color = "white", size = 0.1) +
  scale_fill_gradientn(colours = color_palette, name = "Relative Risk") +
  labs(title = "Choropleth Map of Relative Risk in Michigan",
       subtitle = "Projected using Albers Equal Area") +
  theme_minimal() +
  theme(legend.position = "right")

#####Questions######
###################
#1.	Describe the difference between hue, saturation, and lightness (HSL) and the use cases for when to change the values of HSL for different cartography maps.
#Hue is the color itself. Hue can represent different categories or themes 
#Saturation is the purity or greyness. The higher the stauration, the more pure/bright its and the lower saturation makes it more grey
# Saturation can be used to emphasize a certain area or highlight area of intersts 
# Lightness is how dark or light the color is. 0 is black, 50 is pure hue and 100 is white. Use to show a gradient or heirarchy

#2.	What are the RGB values of Red, Blue, and Green?
#Red: 255,0,0
#Blue: 0,255,0
#green: 0,0,255

#3.	What are some pros and cons of EPSG and ESRI based approaches to projections?
#Pros of EPSG: provides a widely recognized and standard coordinate reference and projections
#Cons of EPSG: finding the correct code for a projection can be challenging
#Pros of ESRI: offers projections and coordinate systems made for ESRI and more user friendly
#Cons of ESRI: Not universal and compatiable with non-ESRi software

#4.	What is an EPSG value appropriate for the state of Michigan?
# EPSG: 3078 is the best for statewide applications since it minize distortion across the state due to multiple zones
# in Michigan
