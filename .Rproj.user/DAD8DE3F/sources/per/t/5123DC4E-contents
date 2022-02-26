
function(input, output, session) {
  
  #This call to the upload csv module (csvFile) will return the raw dataframe imported
  imported_table <- callModule(csvFile, "datafile",
                               stringsAsFactors = FALSE)
  
  #This will add the imported table to the all_dfs dataframe list
  #This is also how you can add other dataframes to the list, calling them names other than "raw"; e.g. "tidied"
  all_dfs$raw <<- reactive( #very important the "<<", because that is how you update an existing global variable. after tidying, you can create a new reactive, adding e.g. $tidy1 to the list
    imported_table() 
  )

  
  output$table <- renderDataTable({#this will display the table "table". note that "table" is the ID of the table on the upload tab. See UI part of the module in upload_modules
    all_dfs$raw()
  })
  
  # output$table2 <- renderDataTable({
  #   imported_table()
  # })
  
  callModule(visualize, "plotcontrols") #this is simply calling the server part of the visualize_module.R
  
}