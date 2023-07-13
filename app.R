# Required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Density Plot Visualization"),
  sidebarLayout(
    sidebarPanel(
      # Group 1 input parameters
      h4("Group 1 Parameters"),
      numericInput("mean1", "Mean Value:", value = 10),
      numericInput("sd1", "Standard Deviation:", value = 1),
      
      tags$hr(),
      
      # Group 2 input parameters
      h4("Group 2 Parameters"),
      numericInput("mean2", "Mean Value:", value = 12),
      numericInput("sd2", "Standard Deviation:", value = 1),
      
      tags$hr(),
      
      # Sample size input parameter
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
  
  # Generate density plot, QQ plots, and perform Shapiro-Wilk test
  output$plot <- renderPlot({
    # Combine the datasets
    data <- data.frame(
      Group = rep(c("Group 1", "Group 2"), each = input$n),
      Value = c(dataset1(), dataset2())
    )
    
    # Density plot
    p_density <- ggplot(data, aes(x = Value, fill = Group)) +
      geom_density(alpha = 0.5) +
      labs(title = "Density Plot Visualization", x = "Value", y = "Density") +
      scale_fill_manual(values = c("red", "blue")) +
      theme_minimal()
    
    # Add mean lines for each group
    p_density <- p_density +
      geom_vline(
        data = data %>% group_by(Group) %>% summarise(mean_value = mean(Value)),
        aes(xintercept = mean_value),
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
    
    # Create text for t-test results
    ttest_text <- paste(
      "T-test Results (Independent Samples):\n",
      "--------------------------------------\n",
      "Group 1 Mean: ", round(ttest_result$estimate[1], 2), "\n",
      "Confidence Interval: [", round(ttest_result$conf.int[1], 2), ", ", round(ttest_result$conf.int[2], 2), "]\n",
      "P-value: ", format.pval(ttest_result$p.value)
    )
    
    # Add t-test results to the plot
    p_density <- p_density +
      annotate(
        "text",
        x = max(data$Value),
        y = max(density(data$Value)$y),
        label = ttest_text,
        hjust = 1,
        vjust = 1,
        size = 4
      )
    
    # QQ plots
    p_qq <- ggplot(data, aes(sample = Value)) +
      geom_qq() +
      facet_wrap(~ Group, ncol = 2) +
      labs(title = "QQ Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
    
    # Combine density plot and QQ plots using cowplot
    p_density_qq <- cowplot::plot_grid(p_density, p_qq, ncol = 1, align = "v", rel_heights = c(3, 2))
    
    # Perform Shapiro-Wilk test for each group
    shapiro_test_group1 <- shapiro.test(data$Value[data$Group == "Group 1"])
    shapiro_test_group2 <- shapiro.test(data$Value[data$Group == "Group 2"])
    
    # Add Shapiro-Wilk test results to the plot
    p_density_qq <- p_density_qq +
      annotate(
        "text",
        x = max(data$Value),
        y = max(density(data$Value)$y),
        label = paste("Shapiro-Wilk Test (Group 1): p-value =", format.pval(shapiro_test_group1$p.value)),
        hjust = 1,
        vjust = 1,
        size = 4,
        color = "red"
      ) +
      annotate(
        "text",
        x = max(data$Value),
        y = min(density(data$Value)$y),
        label = paste("Shapiro-Wilk Test (Group 2): p-value =", format.pval(shapiro_test_group2$p.value)),
        hjust = 1,
        vjust = 0,
        size = 4,
        color = "blue"
      )
    
    # Return the combined plot
    p_density_qq
  })
  
  # Refresh button event
  observeEvent(input$refresh, {
    # Reset input values to default
    updateNumericInput(session, "mean1", value = 10)
    updateNumericInput(session, "sd1", value = 1)
    updateNumericInput(session, "mean2", value = 20)
    updateNumericInput(session, "sd2", value = 1)
    updateNumericInput(session, "n", value = 200)
  })
}

# Run the app
shinyApp(ui, server)
