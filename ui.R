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