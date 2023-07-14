

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
    
# Density plots
p_density <- ggplot(data, aes(x = Value, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot", x = "Value", y = "Density") +
  theme_minimal()

# Add a vertical line at the mean of the data
p_density <- p_density +
  geom_vline(aes(xintercept = mean(Value)), color = "red", linetype = "dashed")

# Add text for the mean value
p_density <- p_density +
  annotate(
    "text",
    x = mean(data$Value),
    y = max(density(data$Value)$y),
    label = paste0("Mean = ", round(mean(data$Value), 2)),
    hjust = 0,
    vjust = 1,
    size = 4
  )

# Add text for the t-test result
p_density <- p_density +
  annotate(
    "text",
    x = max(data$Value),
    y = max(density(data$Value)$y),
    label = "sdfs",
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

