#https://stackoverflow.com/questions/27080089/how-to-organize-large-shiny-apps

#UI part of the CSV file reading module
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    column(4,
           fileInput(ns("file"), label="CSV file"),
           checkboxInput(ns("heading"), "Has heading"),
           selectInput(ns("quote"),
             "Quote",
             c("None" = "" , "Double quote" = "\"" , "Single quote" = "'")
           ),
           selectInput(ns("sep"),
                       "Separator",
                       c("Comma" = "," , "Semi-colon" = ";" , "Tab" = "\t"),
                       selected = ","
           ),
           checkboxInput(ns("scan"), "Scan header Row 1"),
           numericInput(ns("skip"),
                     "Number of lines to skip",
                     value=0)
    ),
    column(8,
           dataTableOutput(ns("table"))
    )
  )
}

# Module server function of the CSV file reading module
csvFile <- function(input, output, session, stringsAsFactors) {
  ns <- session$ns
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })

  header <- reactive({
    read.table(
      userFile()$datapath,
      nrows = 1,
      header = FALSE,
      quote = input$quote,
      stringsAsFactors = stringsAsFactors,
      sep=input$sep
    )
  })
  
  # The user's data, parsed into a data frame
  all_dfs$raw <<- reactive({ #very important the "<<", because that is how you update an existing global variable. after tidying, you can create a new reactive, adding e.g. $tidy1 to the list
    table1 <- read.table(
      userFile()$datapath,
      header = input$heading,
      quote = input$quote,
      stringsAsFactors = stringsAsFactors,
      sep=input$sep,
      skip = as.numeric(input$skip)#skip_value()#skip_checked
    )
    
    colnames(table1) <- unlist(header())
    colnames(table1) <- make.names(names(table1))
    table1
    
  })
  

  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  
  # all_dfs$raw <<- reactive( #very important the "<<", because that is how you update an existing global variable. after tidying, you can create a new reactive, adding e.g. $tidy1 to the list
  #   dataframe()
  # )
  
  output$table <- renderDataTable({#this will display the table "table". note that "table" is the ID of the table on the upload tab. See UI part of the module in upload_modules
    all_dfs$raw()
  })
}
