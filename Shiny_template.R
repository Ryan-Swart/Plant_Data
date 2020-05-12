library(shiny)

ui <- fluidPage("hello world")

server <- function(input, output){}

shinyApp(ui = ui, server = server)