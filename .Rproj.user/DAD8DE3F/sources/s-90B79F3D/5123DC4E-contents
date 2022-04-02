options(shiny.maxRequestSize=30*1024^2) #This is important, otherwise large tables (>6 Mb) won't open. Now, we pushed the limit to 30 Mb

function(input, output, session) {
  
  #This call to the upload csv module (csvFile) will return the raw dataframe imported
  callModule(csvFile, "datafile",
                               stringsAsFactors = FALSE)
  
  callModule(visualize, "plotcontrols") #this is simply calling the server part of the visualize_module.R
  
}