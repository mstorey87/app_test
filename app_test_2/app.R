library(shiny)

ui <- fluidPage(
  titlePanel("Simple R Shiny App"),
  sidebarLayout(
    sidebarPanel(
      numericInput("x", "Enter a number:", 2),
      numericInput("y", "Enter another number:", 3),
      actionButton("go", "Add numbers")
    ),
    mainPanel(
      h4("Result:"),
      verbatimTextOutput("result")
    )
  )
)

server <- function(input, output, session) {
  result <- eventReactive(input$go, {
    input$x + input$y
  })
  
  output$result <- renderText({
    result()
  })
}

shinyApp(ui, server)
