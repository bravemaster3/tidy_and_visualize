library(shiny)
library(shinythemes)

shinyUI(navbarPage(
  "Tidy & Visualize",
  header=tags$style(HTML("body {padding-top: 50px;}")),
  tabPanel("File upload",
           csvFileUI("datafile", "User data (.csv format)")#this is a call to the UI part of the upload_modules.R code

  ),
  tabPanel("Tidy it!",
           tidyUI("tidyoptions")
  ),
  tabPanel("Visualize it!",
           visualizeUI("plotcontrols") #this is a call to the UI part from the visualize_modules.R code
  ),
  tabPanel("About the App",
           aboutUI("aboutoptions")
           ),
  
  theme = "myStyles.css",#shinytheme("united"), #this can be changed with any of the predefined themes available when using shinytheme
  position = c("fixed-top"),
  #tags$style(type="text/css", "body {padding-top: 70px;}"),
  collapsible = TRUE
))