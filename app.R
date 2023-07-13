# Required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Chick Weight Visualization"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Width in Jitter:", min = 0.1, max = 1, value = 0.2, step = 0.1),
      selectInput("x_variable", "Select X Variable:", choices = c("Time", "Chick")),
      selectInput("y_variable", "Select Y Variable:", choices = c("weight", "Diet"))
    ),
    mainPanel(
      plotOutput("scatter_plot"),
      plotOutput("boxplot"),
      plotOutput("barplot")
    )
  )
)

# Server
server <- function(input, output) {
  # Load the ChickWeight dataset
  dataset <- ChickWeight
  
  # Generate scatter plot
  output$scatter_plot <- renderPlot({
    p <- ggplot(dataset, aes(x = .data[[input$x_variable]], y = .data[[input$y_variable]], color = Diet)) +
      geom_jitter(width = input$bins, height = 0, size = 2, alpha = 0.5) +
      labs(title = "Chick Weight Scatter Plot", x = input$x_variable, y = input$y_variable) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal()
    
    p
  })
  
  # Generate boxplot
  output$boxplot <- renderPlot({
    p <- ggplot(dataset, aes(x = Diet, y = weight, fill = Diet)) +
      geom_boxplot(alpha = 0.5) +
      labs(title = "Chick Weight Boxplot", x = "Diet", y = "Weight") +
      scale_fill_brewer(palette = "Set1") +
      theme_minimal()
    
    p
  })
  
  # Generate barplot
  output$barplot <- renderPlot({
    p <- ggplot(dataset, aes(x = Chick, fill = Diet)) +
      geom_bar() +
      labs(title = "Chick Count by Diet", x = "Chick", y = "Count") +
      scale_fill_brewer(palette = "Set1") +
      theme_minimal()
    
    p
  })
}

# Run the app
shinyApp(ui, server)
