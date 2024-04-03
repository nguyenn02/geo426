library(shiny)
library(leaflet)
library(sf)
library(spData)


data("us_states")

# Convert to sf 
us_states_sf <- st_as_sf(us_states)

set.seed(123)
random_data <- data.frame(region = us_states_sf$NAME, value = runif(n = nrow(us_states_sf), min = 100, max = 1000))
us_states_sf <- merge(us_states_sf, random_data, by.x = "NAME", by.y = "region")

# Define UI
ui <- fluidPage(
  titlePanel("Choropleth Map"),
  
  # Create a leaflet output
  leafletOutput("choroplethMap")
)

# Define server logic
server <- function(input, output) {
  
  output$choroplethMap <- renderLeaflet({
    leaflet(data = us_states_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5833, lat = 39.8333, zoom = 4) %>%
      addPolygons(fillColor = ~colorNumeric(palette = "viridis", domain = us_states_sf$value)(value),
                  fillOpacity = 0.7, color = "#BDBDC3", weight = 1,
                  label = ~paste(NAME, ":", value),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
