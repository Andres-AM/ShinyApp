# Required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Density Plot Visualization"),
  sidebarLayout(
    sidebarPanel(
      div(
        h4("Group 1 Parameters"),
        numericInput("n1", "Sample Size:", value = 200),
        numericInput("mean1", "Mean Value:", value = 10),
        numericInput("sd1", "Standard Deviation:", value = 1)
      ),
      div(
        h4("Group 2 Parameters"),
        numericInput("n2", "Sample Size:", value = 200),
        numericInput("mean2", "Mean Value:", value = 12),
        numericInput("sd2", "Standard Deviation:", value = 1)
      ),
      actionButton("refresh", "Refresh")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Generate dataset 1 using rnorm
  dataset1 <- reactive({
    data <- rnorm(input$n1, mean = input$mean1, sd = input$sd1)
    data
  })
  
  # Generate dataset 2 using rnorm
  dataset2 <- reactive({
    data <- rnorm(input$n2, mean = input$mean2, sd = input$sd2)
    data
  })
  
# Generate density plot with colored groups
output$plot <- renderPlot({
  data <- data.frame(
    Group = rep(c("Group 1", "Group 2"), each = c(input$n1, input$n2)),
    Value = c(dataset1(), dataset2())
  )
  
  p <- ggplot(data, aes(x = Value, fill = Group)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot Visualization", x = "Value", y = "Density") +
    scale_fill_manual(values = c("Group 1" = "blue", "Group 2" = "red")) +
    theme_minimal()
  
  # Add mean lines for each group
  p <- p +
    geom_vline(
      data = data %>% group_by(Group) %>% summarise(mean_value = mean(Value)),
      aes(xintercept = mean_value, color = Group),
      linetype = "dashed",
      size = 1
    )
  
  p
})
  
  # Refresh button event
  observeEvent(input$refresh, {
    updateNumericInput(session, "n1", value = 200)
    updateNumericInput(session, "mean1", value = 10)
    updateNumericInput(session, "sd1", value = 1)
    updateNumericInput(session, "n2", value = 200)
    updateNumericInput(session, "mean2", value = 20)
    updateNumericInput(session, "sd2", value = 1)
    updateSliderInput(session, "bins", value = 0.2)
  })
}

# Run the app
shinyApp(ui, server)
