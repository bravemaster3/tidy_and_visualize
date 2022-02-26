#In this module, we will check available dataframes, and upon choice, we can list all columns to choose X & Y for the visualization

#UI part of the visualization module
visualizeInput <- function(id) {
  ns <- NS(id) #creates a namespace
  
  fluidPage(
    sidebarPanel(#the sidebarpanel will contain all controls of the graph, i.e. dataframe, x, y, graph type...
      uiOutput(ns("df_select")),
      uiOutput(ns("x_select")),
      uiOutput(ns("y_select"))
    ),
    uiOutput(ns("mainpanelOutput")) #This one will be rendered dynamically in the server part of the module
  )
  
}


#Server part of the visualization module
visualize <- function(input, output, session) {
  ns <- session$ns #Not sure how important it is, but this is to ensure that we are in the same namespace as in the UI part I guess
  
  output$df_select <- renderUI({#this will render a select input dynamically, based on the global list of dataframes all_dfs
    selectInput(inputId = ns('df_select'),
                                 label = "Select a table",
                                 choices = names(all_dfs))
  })
  
  selected_df <- reactive({#in this reactive expression, we retrieve the dataframe from the selected name in the selectinput "df_select"
    req(input$df_select)#ensures that a selection has been made before running the rest of the block
    selected_df_name <- input$df_select
    all_dfs[[selected_df_name]]() #important here to use double square brakets because all_dfs is a list of dataframes
  })
   # 
  output$x_select <- renderUI({#this will render a select input dynamically based on the selected dataframe in the first selectinput ("df_select) for selecting X variable to plot
      selectInput(inputId = ns('x_select'),
                  label = "Select X column",
                  choices = colnames(selected_df()))
    })
  
  output$y_select <- renderUI({#this will render a select input dynamically based on the selected dataframe in the first selectinput ("df_select) for selecting y variable to plot
    selectInput(inputId = ns('y_select'),
                label = "Select Y column",
                choices = colnames(selected_df()))
  })
  
  output$mainpanelOutput <- renderUI({ #this will render the mainpanel dynamically, with the plot as well
    req(input$x_select, input$y_select) #ensures that these variables are available before running the rest of the block
    x <- input$x_select
    y <- input$y_select
    
    mainPanel(
      renderPlot({
        ggplot(selected_df(), aes_string(x=x,y=y))+
          geom_point()
      },height = 400,width = 600)
    )

  })

}
