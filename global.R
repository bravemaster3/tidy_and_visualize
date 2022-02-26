library(shiny)
library(shinythemes)
library(ggplot2)

#Sourcing module files
#Source here any additional modules created for the tidy.R tab. As long as the app is loaded as a project, relative paths work as below.
source("upload_modules.R")
source("visualize_modules.R")

#creating a global list of dataframes, availabe in the entire application, and updatable when a new dataframe is created either by uploading or after tidying...
#See example in the server.R after loading raw data
all_dfs <- list(raw=data.frame())
