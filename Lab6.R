####Proportional Symbol####
##########################
ggplot() +
  geom_sf(data = abq, fill = "white", color = "darkgrey") +  # Base map
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, color = "darkblue", fill = NA, stroke = 0.5) +  # Black border with no fill
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, fill = "lightblue", alpha = 0.4) +  # Semi-transparent blue fill
  scale_size_continuous(range = c(5, 8), 
                        breaks = c(2500, 5000, 7500),  
                        labels = c("2.5k", "5k", "7.5k")) +
  guides(size = guide_legend(override.aes = list(fill = "lightblue", color = "darkblue", alpha = 0.4, stroke = 1.2))) +
  theme_minimal() +
  labs(size = "Check-ins", title = "Facebook Check-ins in Albuquerque") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("fb_checkin_map.jpeg", width = 10, height = 8, units = "in")

####################
fb_checkin_capped<-fb %>% 
  filter(checkins > 0) %>% 
  select(c(lng, lat, checkins)) %>% 
  mutate(dot_count = ceiling(checkins / 5000)) %>% 
  rename('X' = 'lng', 'Y' = 'lat') %>% 
  st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE) %>% 
  st_transform(st_crs(abq)) %>% 
  filter(st_intersects(geometry, st_geometry(abq_union), sparse = FALSE)[,1]) %>% 
  as.data.frame()
dots_data <- fb_checkin_capped[rep(1:nrow(fb_checkin_capped), fb_checkin_capped$dot_count), ] %>%
  mutate(
    X = X + runif(n(), min = -0.1, max = 0.1),  # Adjust these values based on your coordinate system and desired spread
    Y = Y + runif(n(), min = -0.1, max = 0.1)
ggplot() +
  geom_sf(data = abq, fill = "lightblue", color = "black") +  # Base map
  geom_point(data = dots_data, aes(x = X, y = Y), 
             size = 0.5, color = "darkblue") +  # Solid blue dots
  theme_minimal() +
  labs(title = "Geographically Weighted Dot Density Map\nof Facebook Check-ins",
       caption = "1 dot = 5000 check-ins") +  # Add caption to explain dot representation
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title
    plot.caption = element_text(hjust = 0.5)  # Center the caption
  )
ggsave("my_plot.jpeg", plot, width = 10, height = 8, units = "in")

plot <- ggplot() +
  geom_sf(data = abq, fill = "lightblue", color = "black") +  # Base map
  geom_point(data = dots_data, aes(x = X, y = Y), size = 0.5, color = "darkblue") +  # Solid blue dots
  theme_minimal() +
  labs(title = "Geographically Weighted Dot Density Map\nof Facebook Check-ins",
       caption = "1 dot = 5000 check-ins") +  # Add caption to explain dot representation
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title
    plot.caption = element_text(hjust = 0.5)  # Center the caption
  )

# Save the plot as a JPEG image
ggsave("newdotdensity.jpeg", plot, width = 10, height = 8, units = "in")
#####Choropleth Map Mf_fuction####
######################
title("Choropleth Map")
library(RColorBrewer)
pal <- brewer.pal(n = 7, name = "YlGnBu") 


library(classInt)
breaks <- classIntervals(abq$estimate_DP05_0066, n = 5, style = "jenks")$brks


map <- mf_map(x = abq, var = "estimate_DP05_0066", type = "choro",
              breaks = breaks, pal = pal)
jpeg("ChoroplethMap.jpeg", width = 800, height = 600)
####Choropleth Map ggplot#####

map <- ggplot(data = abq) +
  geom_sf(aes(fill = estimate_DP05_0066), color = "white") + 
  scale_fill_viridis_c(option = "C", direction = -1) + 
  labs(
    title = "Choropleth Map",
    fill = "Value",
    caption = "Source: Your Data Source"
  ) +
  theme_minimal() +  
  theme(
    legend.position = "bottom",  
    text = element_text(family = "sans", color = "#22211d"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  coord_sf(datum = NA)
ggsave("choropleth_map.jpeg", plot = map, width = 10, height = 8, dpi = 300, device = "jpeg")

####Questions###
#1
#Choropleth is use for visualizing data over predetermined area:coutnries, states or counties
#Use shades or colors to represent density or how much of the particular variable 
#Proportional Map represents data quantities associated with specific locations
#Different sizes of symbols 
#Dot density use dots to represent the distribution of a feature or phenomenon over a specific area 
#Each dot represents one or a certain number of occurences
#2
#ggplot2 is a package that creates various static graphics and visualizations. It uses a layering approach to make it
#easier to build up and modify plots. It is flexiable: range of graphs, customizable: elements can be changed, easy to use and R provides
#guides on how to use. It is better to use than mapsf because it has a broader functionality and provides more benefits of extensice customization 
