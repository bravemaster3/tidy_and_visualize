#In this module, we will check available dataframes, and upon choice, we can list all columns to choose X & Y for the visualization

#UI part of the tidy it module
tidyInput <- function(id) {
  ns <- NS(id) #creates a namespace
  
  fluidPage(
    sidebarPanel(#the sidebarpanel will contain all controls of the graph, i.e. dataframe, x, y, graph type...
      checkboxInput(ns("check_time"), "Contains a datetime column?"),
      uiOutput(ns("slct_timecol")),
      actionButton(ns("convert_time"), "Apply")
    ),
    
    mainPanel(
      dataTableOutput(ns("table"))
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
  
  observeEvent(input$convert_time, {
    
    raw_df <- all_dfs$raw()
    raw_df[,input$slct_timecol] <- as.POSIXct(raw_df[,input$slct_timecol], format=input$time_format, tz="UTC")
    all_dfs$raw2 <<- reactive( #Note that this is how you save a new dataframe to the list. here it is named raw2
      raw_df
    )
    #all_dfs$new <- reactive( #very important the "<<", because that is how you update an existing global variable. after tidying, you can create a new reactive, adding e.g. $tidy1 to the list
      #all_dfs$raw()[as.character(input$slct_timecol)] <- as.POSIXct(all_dfs$raw()[as.character(input$slct_timecol)], format="%Y-%m-%d %H:%M:%S", tz="UTC")
      #all_dfs$raw()
    #)
  })
  # 
  # output$mainpanelOutput <- renderUI({ #this will render the mainpanel dynamically, with the plot as well
  #   req(input$x_select, input$y_select) #ensures that these variables are available before running the rest of the block
  #   x <- input$x_select
  #   y <- input$y_select
  #   
  #   mainPanel(
  #     renderPlotly({
  #       p <- ggplot(selected_df()) +
  #         eval(parse(text = input$graph_type))+
  #         eval(parse(text = input$theme_type))
  #       ggplotly(p)
  #     })
  #   )
  #   
  #   
  # })
  
}


