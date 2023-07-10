

# Load libraries
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Monthly Expenses Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("rent", "Rent", value = 0),
      numericInput("utilities", "Utilities", value = 0),
      numericInput("groceries", "Groceries", value = 0),
      numericInput("transportation", "Transportation", value = 0),
      numericInput("entertainment", "Entertainment", value = 0)
    ),
    mainPanel(
      textOutput("total_expenses")
    )
  )
)

# Define server
server <- function(input, output) {
  # Calculate total expenses
  total_expenses <- reactive({
    input$rent + input$utilities + input$groceries + input$transportation + input$entertainment
  })

  # Render total expenses
  output$total_expenses <- renderText({
    paste("Total Monthly Expenses: $", total_expenses())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
