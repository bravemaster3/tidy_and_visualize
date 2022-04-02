#In this module, we will check available dataframes, and upon choice, we can list all columns to choose X & Y for the visualization

#UI part of the tidy it module
tidyInput <- function(id) {
  ns <- NS(id) #creates a namespace
  
  fluidPage(
    sidebarPanel(#the sidebarpanel will contain all controls of the graph, i.e. dataframe, x, y, graph type...
     wellPanel(
        checkboxInput(ns("check_time"), "Contains a datetime column?"),
        uiOutput(ns("slct_timecol")),
        actionButton(ns("BtnConvertTime"), "Apply")
        ),
     wellPanel(
       checkboxInput(inputId = ns("CheckNaRm"), label = "Create complete observations?"),
       actionButton(inputId = ns("BtnNaRm"), label = "Remove NAs")
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
    
    raw_df <- all_dfs$raw()
    raw_df[,input$slct_timecol] <- as.POSIXct(raw_df[,input$slct_timecol], format=input$time_format, tz="UTC")
    all_dfs$raw_withTime <<- reactive( #Note that this is how you save a new dataframe to the list. here it is named raw2
      raw_df
    )
  })
  
  observeEvent(input$BtnNaRm, {
    raw_df <- all_dfs$raw()
    all_dfs$raw_NaRm <<- reactive({
      raw_df[complete.cases(raw_df),]
    })
    
    output$table2 <- renderDataTable({
      all_dfs$raw_NaRm()
    })
  })

}


