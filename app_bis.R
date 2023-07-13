# Required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Density Plot Visualization"),
  sidebarLayout(
    sidebarPanel(
      h4("Group 1 Parameters"),
      numericInput("mean1", "Mean Value:", value = 10),
      numericInput("sd1", "Standard Deviation:", value = 1),
      
      tags$hr(),
      
      h4("Group 2 Parameters"),
      numericInput("mean2", "Mean Value:", value = 12),
      numericInput("sd2", "Standard Deviation:", value = 1),
      
      tags$hr(),
      
      h4("Sample Size"),
      numericInput("n", "Size:", value = 200),
      
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
    data <- rnorm(input$n, mean = input$mean1, sd = input$sd1)
    data
  })
  
  # Generate dataset 2 using rnorm
  dataset2 <- reactive({
    data <- rnorm(input$n, mean = input$mean2, sd = input$sd2)
    data
  })
  
  # Generate density plot with colored groups and t-test results box
  output$plot <- renderPlot({
    data <- data.frame(
      Group = rep(c("Group 1", "Group 2"), each = input$n),
      Value = c(dataset1(), dataset2())
    )
    
    p <- ggplot(data, aes(x = Value, fill = Group)) +
      geom_density(alpha = 0.5) +
      labs(title = "Density Plot Visualization", x = "Value", y = "Density") +
      scale_fill_manual(values = c("Group 1" = "red", "Group 2" = "blue")) +
      theme_minimal()
    
    # Add mean lines for each group
    p <- p +
      geom_vline(
        data = data %>% group_by(Group) %>% summarise(mean_value = mean(Value)),
        aes(xintercept = mean_value, color = Group),
        linetype = "dashed",
        size = 1.5
      )
    
    # Perform t-test
    ttest_result <- t.test(Value ~ Group,
                           data = data,
                           var.equal = TRUE,
                           conf.level = 0.95,
                           alternative = "two.sided"
    )
    
    # Create text for t-test results box
    ttest_text <- paste(
      "T-test Results (Independent Samples):\n",
      "--------------------------------------\n",
      "Group 1 Mean: ", round(ttest_result$estimate[1], 2), "\n",
      "Confidence Interval: [", round(ttest_result$conf.int[1], 2), ", ", round(ttest_result$conf.int[2], 2), "]\n",
      "P-value: ", format.pval(ttest_result$p.value)
    )
    
    # Add t-test results box
    p <- p +
      annotate(
        "text",
        x = max(data$Value),
        y = max(density(data$Value)$y),
        label = ttest_text,
        hjust = 1,
        vjust = 1,
        size = 4
      )
    
    p
  })
  
  # Refresh button event
  observeEvent(input$refresh, {
    updateNumericInput(session, "mean1", value = 10)
    updateNumericInput(session, "sd1", value = 1)
    updateNumericInput(session, "mean2", value = 20)
    updateNumericInput(session, "sd2", value = 1)
    updateNumericInput(session, "n", value = 200)
  })
}

# Run the app
shinyApp(ui, server)
