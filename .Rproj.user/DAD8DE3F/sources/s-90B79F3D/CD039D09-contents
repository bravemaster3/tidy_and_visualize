library(shiny)
library(shinythemes)

shinyUI(navbarPage(
  "Tidy & Visualize",
  tabPanel("File upload",
           csvFileInput("datafile", "User data (.csv format)")#this is a call to the UI part of the upload_modules.R code

  ),
  tabPanel("Tidy it!"#,
           #dataTableOutput("table2")
  ),
  tabPanel("Visualize it!",
           visualizeInput("plotcontrols") #this is a call to the UI part from the visualize_modules.R code
  ),
  theme = shinytheme("darkly"), #this can be changed with any of the predefined themes available when using shinytheme
  position = c("fixed-top"),
  tags$style(type="text/css", "body {padding-top: 70px;}"),
  collapsible = TRUE
))