library(shiny)
ui <- fluidPage(
  # *Input(), *Output()
  passwordInput(inputId = "password","Password: "),
  actionButton("go", "Go"),
  verbatimTextOutput("value")
)

server <- function(input, output) {
  output$value <- renderText({
    req(input$go)
    isolate(input$password)
  })
}

shinyApp(ui, server)

