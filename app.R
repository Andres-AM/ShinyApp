# Required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Boxplot and Jitter Plot"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of Bins:", min = 1, max = 30, value = 10),
      selectInput("variable", "Select Variable:", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
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
  
  # Filter the dataset based on variable
  filteredData <- reactive({
    dataset
  })
  
  # Generate boxplot and jitter plot
  output$plot <- renderPlot({
    p <- ggplot(filteredData(), aes(x = Species, y = .data[[input$variable]], fill = Species)) +
      geom_boxplot(alpha = 0.5, outlier.shape = NA) +
      geom_jitter(aes(color = Species), width = 0.2, height = 0, size = 2, alpha = 0.5) +
      labs(title = "Boxplot and Jitter Plot", y = input$variable) +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1") +
      theme_minimal()
    
    p
  })
}

# Run the app
shinyApp(ui, server)
