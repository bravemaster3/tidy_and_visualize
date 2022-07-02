#In this module, we will check available dataframes, and upon choice, we can list all columns to choose X & Y for the visualization

#UI part of the visualization module
visualizeUI <- function(id) {
  ns <- NS(id) #creates a namespace
  
  # fluidPage(
  sidebarLayout(
    sidebarPanel(width = 3,
                 class="sidebarpanel",#the sidebarpanel will contain all controls of the graph, i.e. dataframe, x, y, graph type...
      wellPanel(
        h3("Main geometry"),
        uiOutput(ns("df_select")),
        selectInput(ns("graph_type"),
                    label="Select the type of plot",
                    choices=c("point"="geom_point(aes_string(x=x,y=y))",
                              "line"="geom_line(aes_string(x=x,y=y))",
                              "boxplot"="geom_boxplot(aes_string(x=x,y=y))",
                              "beanplot"="geom_violin(aes_string(x=x,y=y))",
                              "bar"="geom_bar(aes_string(x=x))")
        ),
        fluidRow(
          column(6,uiOutput(ns("x_select"))),
          column(6,uiOutput(ns("y_select")))
        )
      ),
      
      wellPanel(
        id=ns("wellpanAddGeom"),
        checkboxInput(ns("checkAddGeom"),label=h3("Additional geometries")),
        # uiOutput(ns("df_select2")),
        uiOutput(ns("addGeomControls")),
        # selectInput(ns("graph_type2"),
        #             label="Select the type of plot",
        #             choices=c("point"="geom_point(aes_string(x=x,y=y))",
        #                       "line"="geom_line(aes_string(x=x,y=y))",
        #                       "boxplot"="geom_boxplot(aes_string(x=x,y=y))",
        #                       "beanplot"="geom_violin(aes_string(x=x,y=y))",
        #                       "bar"="geom_bar(aes_string(x=x))")
        # ),
        fluidRow(
          column(6,uiOutput(ns("x_select2"))), # Not sure yet if this should be an option.
          column(6,uiOutput(ns("y_select2")))
        )
      ),
      
      wellPanel(
        h3("Additional settings"),
        selectInput(ns("theme_type"),
                    label="Select theme",
                    choices=c("bw"="theme_bw()",
                              "classic"="theme_classic()",
                              "economist"="theme_economist()",
                              "dark"="theme_dark()")
        ),
        uiOutput(ns("axis_titles"))
      ),
      actionButton(ns("plotBtn"), "Plot it")
      ),
    
    #shinycssloaders::withSpinner(
    mainPanel(width=9,
              class="mainpanel",
      withSpinner(plotlyOutput(ns("plotit")), type = 6)
    ),
    fluid = TRUE
    
  )
  
}



#Server part of the visualization module
visualizeServer <- function(id){
  moduleServer(
    id,
    function(input,output,session) {
      ns <- session$ns #Not sure how important it is, but this is to ensure that we are in the same namespace as in the UI part I guess
      
      output$df_select <- renderUI({#this will render a select input dynamically, based on the global list of dataframes all_dfs
        selectInput(inputId = ns("df_select"),
                    label = "Select a table",
                    choices = names(all_dfs))
      })
      
      selected_df <- reactive({#in this reactive expression, we retrieve the dataframe from the selected name in the selectinput "df_select"
        req(input$df_select)#ensures that a selection has been made before running the rest of the block
        selected_df_name <- input$df_select
        all_dfs[[selected_df_name]] #important here to use double square brakets because all_dfs is a list of dataframes
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
      
      #Programming the server logic for additional geometry
      observeEvent(input$checkAddGeom, {
        if(input$checkAddGeom == TRUE){
          output$addGeomControls <- renderUI({#this will render a select input dynamically, based on the global list of dataframes all_dfs
            tagList(
              selectInput(inputId = ns("df_select2"),
                          label = "Select a table",
                          choices = names(all_dfs)),
              selectInput(ns("graph_type2"),
                          label="Select the type of plot",
                          choices=c("point"="geom_point(aes_string(x=x,y=y))",
                                    "line"="geom_line(aes_string(x=x,y=y))",
                                    "boxplot"="geom_boxplot(aes_string(x=x,y=y))",
                                    "beanplot"="geom_violin(aes_string(x=x,y=y))",
                                    "bar"="geom_bar(aes_string(x=x))")
              )
            )
          })
          
        } 
        # else{
        #   print(paste0("#",{ns("df_select2")}))
        #   removeUI(selector = "div:has(> #df_select2)",
        #            immediate = TRUE
        #            )
        # }
      })
      
      
      #Programming the server logic for extra controls...
      
      output$axis_titles <- renderUI(
        tagList(textInput(inputId = ns("x_title"),
                          label = "Change X axis title",
                          value = input$x_select),
                textInput(inputId = ns("y_title"),
                          label = "Change Y axis title",
                          value = input$y_select))
      )
      
      output$heading_graph <- renderUI(
        textInput(inputId = ns("main_title"), label = "Give your Graph a Title", placeholder = "Provide your graph a title"))
      
      # observe({
      #   output$mainpanelOutput <- renderUI({ #this will render the mainpanel dynamically, with the plot as well
      #     mainPanel(class="mainpanel",
      #       withSpinner(plotlyOutput(ns("plotit")), type = 6)
      #     )
      #   })
      # })
      # 
      # observe({
      #   output$mainpanelOutput <- renderUI({ #this will render the mainpanel dynamically, with the plot as well
      #     mainPanel(class="mainpanel",
      #       withSpinner(plotlyOutput(ns("plotit")), type = 6)
      #     )
      #   })
      # })
      
      
      #let's get the input$graph_type2 reactively outside and add it only if the checkbutton is checked.
      additional_geom <- reactive({
        if(input$checkAddGeom==TRUE) input$graph_type2
        else "NULL"
      })
      
      #Let's finally do the plottings
      observeEvent(input$plotBtn,{
        req(input$x_select, input$y_select) #ensures that these variables are available before running the rest of the block
        x <- input$x_select
        y <- input$y_select
        
        # req(input$plotBtn)
        # Sys.sleep(5)
        # mainPanel(
        p <- ggplot(selected_df()) +
          eval(parse(text = input$graph_type))+
          eval(parse(text = additional_geom()))+
          eval(parse(text = input$theme_type))+
          xlab(input$x_title)+
          ylab(input$y_title)
        
        output$plotit <- renderPlotly({
          req(input$plotBtn)
          #Sys.sleep(5)
          ggplotly(p)
        })
        #)
        
        # })
      })
    }
  )
}