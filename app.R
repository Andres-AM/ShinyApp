# Required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Scatter Plot"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Width in Jitter:", min = 0.1, max = 1, value = 0.2, step = 0.1),
      selectInput("x_variable", "Select X Variable:", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
      selectInput("y_variable", "Select Y Variable:", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output) {
  # Load the iris dataset
  dataset <- iris
  
  # Generate scatter plot
  output$plot <- renderPlot({
    p <- ggplot(dataset, aes(x = .data[[input$x_variable]], y = .data[[input$y_variable]], color = Species)) +
      geom_jitter(width = input$bins, height = 0, size = 2, alpha = 0.5) +
      labs(title = "Scatter Plot", x = input$x_variable, y = input$y_variable) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal()
    
    p
  })
}

# Run the app
shinyApp(ui, server)
