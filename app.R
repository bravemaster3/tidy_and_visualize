#inspiration from https://stackoverflow.com/questions/27080089/how-to-organize-large-shiny-apps
library(shiny)
ui <- shinyUI(navbarPage(
  "Tidy & Visualize",
  tabPanel("File upload"),
  tabPanel("Tidy it!"),
  tabPanel("Visualize it!")
  
))


server <- function(input, output, session) {

}


shinyApp(ui, server)
