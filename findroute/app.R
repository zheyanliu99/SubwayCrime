#################################################
# author: "Zheyan Liu"
# date: "11/18/2021"
#################################################

# change working directory

# R packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(reticulate)

# use local python
use_python('/Users/jimmy/anaconda3/python.exe')

getroute = import("GetRoute")
source_python("GetRoute.py")

mygoogle_routes = google_routes()
mygoogle_routes$start_location = '168 st, New York'
mygoogle_routes$destination = 'Prospect Park, New York'
df = mygoogle_routes$get_directions() # it is a R dataframe
# rownames(df) = df %>% pull(route_num)

df %>% 
  group_by(route_num) %>% 
  # mutate(line = paste0(line, num_stops, collapse = '(')) %>% 
  summarise(time = mean(time),
            distance = mean(distance),
            walking_distance = mean(walking_distance),
            line = paste0(line, '[', as.character(num_stops), ']', collapse = " - ")) %>% 
  rename('line[stops]' = line)

df %>% 
  mutate(line = paste0(line, num_stops, collapse = '('))
####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("paper"),
                navbarPage("No crime Navigation:",
                           
                           tabPanel("Home",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      
                                      textInput("start_location", "Your Location:", "168 st"),
                                      textInput("destination", "Place of Interest:", "JFK"),
                                      
                                      actionButton("submitbutton", 
                                                   "Submit", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Routes')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      DT::dataTableOutput("tabledata"), # Results DT table
                                      tableOutput('tabledata2') # Results table
                                    ) # mainPanel()
                                    
                           )
                           
                ) # navbarPage()
) # fluidPage()


####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
  # Input Data
    
  datasetInput <- reactive({  
    
    mygoogle_routes = google_routes()
    mygoogle_routes$start_location = input$start_location
    mygoogle_routes$destination = input$destination
    df = mygoogle_routes$get_directions() # it is a R dataframe
    df = 
      df %>% 
      group_by(route_num) %>% 
      # mutate(line = paste0(line, num_stops, collapse = '(')) %>% 
      summarise(time = mean(time),
                distance = mean(distance),
                walking_distance = mean(walking_distance),
                line = paste0(line, '[', as.character(num_stops), ']', collapse = " - ")) %>% 
      distinct(line, .keep_all = TRUE) %>% 
      rename('line[stops]' = line)
    print(df)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Find results") 
    } else {
      return("Please enter your start location and destination")
    }
  })
  
  # Prediction results table
  # output$tabledata <- renderTable({
  #   if (input$submitbutton>0) { 
  #     datasetInput()
  #   } 
  # })
  
  output$tabledata <- DT::renderDataTable({
      if (input$submitbutton>0) {
        DT::datatable(datasetInput(),
                      options = list(scrollX = TRUE),
                      rownames = FALSE)
      }
    })

}


####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server) 

