# Required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Boxplot Visualization"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of Bins:", min = 1, max = 30, value = 10),
      selectInput("species", "Filter by Species:", choices = c("", unique(iris$Species)))
    ),
    mainPanel(
      plotOutput("boxplot")
    )
  )
)

# Server
server <- function(input, output) {
  # Load the iris dataset
  dataset <- iris
  
  # Filter the dataset based on species
  filteredData <- reactive({
    if (input$species != "") {
      dataset %>% filter(Species == input$species)
    } else {
      dataset
    }
  })
  
  # Generate boxplot
  output$boxplot <- renderPlot({
    ggplot(filteredData(), aes(x = Species, y = Sepal.Length)) +
      geom_boxplot() +
      labs(title = "Boxplot", y = "Sepal Length")
  })
}

# Run the app
shinyApp(ui, server)
