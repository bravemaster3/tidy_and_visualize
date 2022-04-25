#In this module, we will check available dataframes, and upon choice, we can list all columns to choose X & Y for the visualization

#UI part of the tidy it module
tidyInput <- function(id) {
  ns <- NS(id) #creates a namespace
  
  fluidPage(
    sidebarPanel(#the sidebarpanel will contain all controls of the graph, i.e. dataframe, x, y, graph type...
      tags$head(tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto;}" ))),
      wellPanel(
        checkboxInput(ns("check_time"), "Contains a datetime column?"),
        uiOutput(ns("slct_timecol")),
        actionButton(ns("BtnConvertTime"), "Apply")
        ),
     wellPanel(
       checkboxInput(inputId = ns("CheckNaRm"), label = "Create complete observations?"),
       actionButton(inputId = ns("BtnNaRm"), label = "Remove NAs")
    ),
    
    wellPanel(
      uiOutput(ns("df_select")),
      uiOutput(ns("slct_cols_out")),
      actionButton(ns("Btn_ColSubset"), label = "Create subset")
    ),
    
    wellPanel(
      uiOutput(ns("df_Rselect")),
      uiOutput(ns("Slider")),
      actionButton(ns("Btn_Filterrow"), label = "Filter Selected Rows")
    )
    ),
    
    mainPanel(
      dataTableOutput(ns("table2"))
              )

  )
  
}



#Server part of the visualization module
tidy <- function(input, output, session) {
  ns <- session$ns #Not sure how important it is, but this is to ensure that we are in the same namespace as in the UI part I guess
  
  #Programming what happens when a datetime column exists
  observeEvent(input$check_time,{
    if(input$check_time==TRUE){
      output$slct_timecol <- renderUI({#this will render a select input dynamically based on the selected dataframe in the first selectinput ("df_select) for selecting X variable to plot
        tagList(
          selectInput(inputId = ns('slct_timecol'),
                      label = "Select datetime column",
                      choices = colnames(all_dfs$raw())),
          textInput(inputId = ns('time_format'),
                    label="Input the time format",
                    value="%Y-%m-%d %H:%M:%S",
                    placeholder = "%Y-%m-%d %H:%M:%S")
        )
      })
    }
    
  })
  
  observeEvent(input$BtnConvertTime, {
    
    raw_df <- all_dfs$raw
    raw_df[,input$slct_timecol] <- as.POSIXct(raw_df[,input$slct_timecol], format=input$time_format, tz="UTC")
    all_dfs$raw_withTime <<- reactive( #Note that this is how you save a new dataframe to the list. here it is named raw2
      raw_df
    )
  })
  
  #Programming the logic for removing missing values and creating a new dataframe
  observeEvent(input$BtnNaRm, {
    raw_df <- all_dfs$raw
    
    all_dfs$raw_NaRm = raw_df[complete.cases(raw_df),]
    output$table2 <- renderDataTable({
      all_dfs$raw_NaRm
    })
  })
  
  #Programming the server logic for subsetting the dataframe by columns
  output$df_select <- renderUI({#this will render a select input dynamically, based on the global list of dataframes all_dfs
    selectInput(inputId = ns("df_select"),
                label = "Select a table",
                choices = names(all_dfs))
  })

  selected_df <- reactive({
    req(input$df_select)
    all_dfs[[input$df_select]]
  })

  output$slct_cols_out <- renderUI(
    selectInput(inputId = ns("slct_cols"),
                label = "select columns",
                multiple = T,
                choices = colnames(selected_df())
    )
  )

  observeEvent(input$Btn_ColSubset, {
    all_dfs$subset_df_cols = selected_df()[,input$slct_cols]
    
    output$table2 <- renderDataTable({
      all_dfs$subset_df_cols
    })
  }
  )
  
  output$df_Rselect <- renderUI({
    selectInput(inputId = ns("df_Rslct"),
                label = "Select table",
                choices = names(all_dfs))
  })
  
  selected_Rdf <- reactive({
    req(input$df_Rslct)
    all_dfs[[input$df_Rslct]]
  })
  
  output$Slider <- renderUI({
    sliderInput(inputId = ns("FilterRow"), label = h3("Range of rows"), min = 1, max = 1000, value = c(4,10), dragRange = T)
  })

  observeEvent(input$Btn_Filterrow, {
    all_dfs$filtered_rows = selected_Rdf()[input$FilterRow[1]:input$FilterRow[2],]
    output$table2 <- renderDataTable({
      all_dfs$filtered_rows
    })
  })
  
}


