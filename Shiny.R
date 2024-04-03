#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define the user interface
ui <- fluidPage(
  titlePanel("Square Calculator"), 
  sidebarLayout(
    sidebarPanel(
      # Input: Specify a number
      numericInput("num", "Enter a number:", 1)
    ),
    mainPanel(
      # Output: Display the square of the input
      textOutput("square")
    )
  )
)


server <- function(input, output) {
  output$square <- renderText({
    # Calculate square
    input$num^2
  })
}


shinyApp(ui = ui, server = server)
