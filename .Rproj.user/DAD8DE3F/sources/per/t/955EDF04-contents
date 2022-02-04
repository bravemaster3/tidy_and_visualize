library(shiny)
library(shinythemes)

#Sourcing module files
source("upload_modules.R")

ui <- shinyUI(navbarPage(
  "Tidy & Visualize",
    tabPanel("File upload",
             csvFileInput("datafile", "User data (.csv format)")
              ),
    tabPanel("Tidy it!",
             dataTableOutput("table2")),
    tabPanel("Visualize it!"),
  theme = shinytheme("darkly")
))


server <- function(input, output, session) {
  
  imported_table <- callModule(csvFile, "datafile",
                         stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    imported_table()
  })

}


shinyApp(ui, server)
