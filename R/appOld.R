library(shiny)

ui <- fluidPage(
  tags$div(
    tags$img(src = "logo.png", height = '50px', width = '100px'), # Correct path
    h1("My Shiny App")
  )
)

server <- function(input, output) {}
if (options()$browser == "") options(browser = "chromium")

shinyApp(ui = ui, server = server)
