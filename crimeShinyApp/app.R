#########################################
# author: "Youyuan Kong & Stephen Yuan" #
# date: "11/30/2021"                    #
#########################################

# data import

## R packages
library(flexdashboard)
library(tidyverse)
library(viridis)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(shinydashboard)
library(brochure)
##install.packages("shinythemes")
library(shinythemes)
#remotes::install_github("ColinFay/brochure")
##install.packages("shinydashboard")
library(flexdashboard)
library(tigris)
library(RColorBrewer)
library(data.table)
library(rvest)
library(httr)
library(lubridate)

## ui.R ##



## crime dataframe

crime_df = 
  read_csv("subwaycrime_with_station_new.csv")%>% 
  mutate(cmplnt_to_dt=as.Date(cmplnt_to_dt,format='%m/%d/%Y')) %>% 
  filter(distance<=0.0001)%>% 
  filter(cmplnt_to_dt>='2006-1-1')

crime_choice = 
  crime_df %>% 
  count(ofns_desc) %>% 
  ##  mutate(ofns_desc = fct_reorder(ofns_desc, n)) %>% 
  slice_max(n,n=10) %>% 
  distinct(ofns_desc) %>% 
  pull() 


## prepare for tab 2

crime_df2 = read_csv('crime_rate.csv') %>% 
  filter(postal_code!="None") %>% 
  janitor::clean_names()





options(tigris_use_cache = TRUE)


# get zip boundaries that start with 282
char_zips = zctas(cb = TRUE)
char_zips = 
  char_zips %>% 
  rename(postal_code = GEOID10)

####################
# User Interface   #
####################


ui = navbarPage("Dashboard", theme = shinytheme("flatly"),
                tabPanel("Crime",
                         sidebarLayout(
                           sidebarPanel(fluidPage(
                             
                             
                             selectInput(
                               "crime_choice",
                               label = h2("Select Crime"),
                               choices = c("All",crime_choice),
                               selected = "All"),
                             
                             sliderInput(
                               "year",
                               label = h2("Year Range"),
                               min = 2006,
                               max = 2021,
                               value = c(2007, 2021)
                             )
                             
                           )
                             
                           ),
                           mainPanel(
                             box(leafletOutput(outputId = "bubble_map"),
                                 width=12  ),
                             box(plotlyOutput(outputId = "hist_cluster"),
                                 width=6),
                             box(plotlyOutput(outputId = "hist_service"),
                                 width=6
                                 #                          )
                             )
                           )
                         )
                ),
                tabPanel("Crime rate",
                         sidebarLayout(
                           sidebarPanel(fluidPage(
                             
                             
                             selectInput(
                               "crime type",
                               label = h3("select crime type"),
                               choices = list("Felony" = 1,
                                              "Misdemeanor" = 2,
                                              "Violation" = 3,
                                              "All"=4),
                               selected = 1
                             ),

                             dateRangeInput(
                               "time",
                               label=h3("Choose time"),
                               start = "2018-12-29",
                               end = "2021-11-05",
                               min = "2018-12-29",
                               max = "2021-11-05",
                               format = "yyyy-mm-dd",
                               startview = "month",
                               weekstart = 0,
                               language = "en",
                             )
                             
                           )
                           
                           ),
                            mainPanel(
                             box(leafletOutput(outputId = "zip_code_map"),
                                 width=12),
                              box(plotlyOutput(outputId = "hist_cluster2"),
                                  width=6),
                              box(plotlyOutput(outputId = "hist_service2"),
                                 width=6 )
                           )
                         )
                )
)







# ui <-dashboardPage(
#   dashboardHeader(title = "My Dashboard"),
#   dashboardSidebar(fluidPage(
#     
#     
#     selectInput(
#       "crime_choice",
#       label = h2("Select Crime"),
#       choices = c("All",crime_choice),
#       selected = "All"),
#     
#     sliderInput(
#       "year",
#       label = h2("Year Range"),
#       min = 2006,
#       max = 2021,
#       value = c(2007, 2021)
#     )
#     
#   )),
#   
#   dashboardBody(     ## splitLayout(style = "border: 1px solid silver;",
#     ##             cellArgs = list(style = "padding: 6px"),
#     box(leafletOutput(outputId = "bubble_map"),
#         width=12  ),
#     box(plotlyOutput(outputId = "hist_cluster"),
#         width=6),
#     box(plotlyOutput(outputId = "hist_service"),
#         width=6
#         #                          )
#     ))
# )


# ui <- fluidPage(
#   
#   titlePanel("Crime Map"),
#   
#   # 定义包含输入与输出的侧边栏布局 ----
#   sidebarLayout(
#     
#     # 输入的侧边栏面板 ----
#     sidebarPanel(
#       
#       
#       selectInput(
#         "crime_choice",
#         label = h1("Select Crime"),
#         choices = c("All",crime_choice),
#         selected = "All"),
#       
#       sliderInput(
#         "year",
#         label = h2("Year Range"),
#         min = 2006,
#         max = 2021,
#         value = c(2007, 2021)
#       )
#       
#     ),
#     
#     # 显示输出的主面板 ----
#     mainPanel(
#       
#       # Output: 
#       
#       leafletOutput(outputId = "bubble_map"),
#        plotlyOutput(outputId = "hist_cluster")
#     )
#   )
# )


####################
# Server           #
####################

server <- function(input, output) {
  
  
  # d <- reactive({
  #   dist(input$crime_choice)
  #   
  #   dist(input$year)
  # })
  
  
  
  output$bubble_map <- ##reactive({
    
    renderLeaflet({ 
      
      
      
      
      if(input$crime_choice!="All") {
        crime_df_adjusted<-
          crime_df %>%
          filter(cmplnt_to_dt>=
                   str_c(as.character(round(input$year)),'-1-1')[1],
                 cmplnt_to_dt<=
                   str_c(as.character(round(input$year)),'-12-1')[2],
                 ofns_desc==input$crime_choice) }
      else {crime_df_adjusted<-
        crime_df %>%
        filter(cmplnt_to_dt>=
                 str_c(as.character(round(input$year)),'-1-1')[1],
               cmplnt_to_dt<=
                 str_c(as.character(round(input$year)),'-12-1')[2]
        ) }
      
      
      
      
      crime_df_adjusted %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addMarkers(lat = ~latitude, lng = ~longitude,
                   # popup = paste("Offense Description:", crime_df$ofns_desc,       "<br>",
                   #               "Suspect's age:",crime_df$susp_age_group,"<br>",
                   #               "Suspect's race:", crime_df$susp_race,"<br>",
                   #               "Suspect's gender:",crime_df$susp_sex,"<br>",
                   #               "Victim's age:",crime_df$vic_age_group,"<br>",
                   #               "Victim's race:", crime_df$vic_race,"<br>",
                   #               "Victim's gender:",crime_df$vic_sex,"<br>",
                   #               "Time:",crime_df$cmplnt_to_dt),
                   clusterOptions = markerClusterOptions())
      
    })
  
  output$hist_cluster <-     
    renderPlotly({
      if(input$crime_choice!="All") {
        crime_df_adjusted<-
          crime_df %>%
          filter(cmplnt_to_dt>=
                   str_c(as.character(round(input$year)),'-1-1')[1],
                 cmplnt_to_dt<=
                   str_c(as.character(round(input$year)),'-12-1')[2],
                 ofns_desc==input$crime_choice) }
      else {crime_df_adjusted<-
        crime_df %>%
        filter(cmplnt_to_dt>=
                 str_c(as.character(round(input$year)),'-1-1')[1],
               cmplnt_to_dt<=
                 str_c(as.character(round(input$year)),'-12-1')[2]
        ) }
      
      
      if(input[["crime_choice"]]!="All") {   
        crime_df_adjusted<-  
          crime_df %>% 
          filter(cmplnt_to_dt>=
                   str_c(as.character(round(input[["year"]])),'-1-1')[1],
                 cmplnt_to_dt<=
                   str_c(as.character(round(input[["year"]])),'-12-1')[2],
                 ofns_desc==input[["crime_choice"]]) }
      else {crime_df_adjusted<-
        crime_df %>% 
        filter(cmplnt_to_dt>=
                 str_c(as.character(round(input[["year"]])),'-1-1')[1],
               cmplnt_to_dt<=
                 str_c(as.character(round(input[["year"]])),'-12-1')[2]
        ) }
      
      crime_df_adjusted %>% 
        count(cluster) %>% 
        mutate(cluster = fct_reorder(cluster, n)) %>% 
        slice_max(n,n=20) %>% 
        plot_ly(x = ~cluster, y = ~n, color = ~cluster, type = "bar", colors = "viridis") %>% 
        layout(yaxis = list(title = 'Number of Compliants'),
               xaxis = list(title = 'Cluster'))  
      
      
    })
  
  
  output$hist_service <-     
    renderPlotly({
      if(input$crime_choice!="All") {
        crime_df_adjusted<-
          crime_df %>%
          filter(cmplnt_to_dt>=
                   str_c(as.character(round(input$year)),'-1-1')[1],
                 cmplnt_to_dt<=
                   str_c(as.character(round(input$year)),'-12-1')[2],
                 ofns_desc==input$crime_choice) }
      else {crime_df_adjusted<-
        crime_df %>%
        filter(cmplnt_to_dt>=
                 str_c(as.character(round(input$year)),'-1-1')[1],
               cmplnt_to_dt<=
                 str_c(as.character(round(input$year)),'-12-1')[2]
        ) }
      
      
      if(input[["crime_choice"]]!="All") {   
        crime_df_adjusted<-  
          crime_df %>% 
          filter(cmplnt_to_dt>=
                   str_c(as.character(round(input[["year"]])),'-1-1')[1],
                 cmplnt_to_dt<=
                   str_c(as.character(round(input[["year"]])),'-12-1')[2],
                 ofns_desc==input[["crime_choice"]]) }
      else {crime_df_adjusted<-
        crime_df %>% 
        filter(cmplnt_to_dt>=
                 str_c(as.character(round(input[["year"]])),'-1-1')[1],
               cmplnt_to_dt<=
                 str_c(as.character(round(input[["year"]])),'-12-1')[2]
        ) }
      
      crime_df_adjusted %>% 
        count(service) %>% 
        mutate(service = fct_reorder(service, n)) %>% 
        slice_max(n,n=20) %>% 
        plot_ly(x = ~service, y = ~n, color = ~service, type = "bar", colors = "viridis") %>% 
        layout(yaxis = list(title = 'Number of Compliants'),
               xaxis = list(title = 'Cluster'))  
      
      
    })
  
  output$zip_code_map <-     
    renderLeaflet({
      if(input[["crime type"]]==1)  
      {
        crime_rate_df<-crime_df2 %>% 
          filter(date>=input[["time"]][1],
                 date<=input[["time"]][2]) %>% 
          group_by(postal_code) %>% 
          summarise(crime_rate=sum(felony)/sum(flow))
      }
      else if(input[["crime type"]]==2)
      {
        crime_rate_df<-crime_df2 %>% 
          filter(date>=input[["time"]][1],
                 date<=input[["time"]][2]) %>% 
          group_by(postal_code) %>% 
          summarise(crime_rate=sum(misdemeanor)/sum(flow))
      }
      else if(input[["crime type"]]==3)
      {
        crime_rate_df<-crime_df2 %>% 
          filter(date>=input[["time"]][1],
                 date<=input[["time"]][2]) %>% 
          group_by(postal_code) %>% 
          summarise(crime_rate=sum(violation)/sum(flow))
      }
      else{
        crime_rate_df<-crime_df2 %>% 
          filter(date>=input[["time"]][1],
                 date<=input[["time"]][2]) %>% 
          group_by(postal_code) %>% 
          summarise(crime_rate=sum(violation+misdemeanor+felony)/sum(flow))
        
      }
      
      
      
      crime_rate_df<- merge(char_zips, crime_rate_df , 
                            by = "postal_code") 
      
      
      
      pal <- colorNumeric(
        palette = "Greens",
        domain = crime_rate_df$crime_rate,
        na.color = "white")
      
      
      labels <- 
        paste0(
          "Zip Code: ",
          crime_rate_df$postal_code, "<br/>",
          "Flow of Passengers: ",
          crime_rate_df$crime_rate) %>%
        lapply(htmltools::HTML)
      
      
      crime_rate_df %>%  
        mutate(postal_code_int = as.integer(postal_code)) %>% 
        filter(postal_code_int >= 10000 & postal_code_int < 14900) %>% 
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(
          fillColor = ~pal(crime_rate),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(weight = 2,
                                       color = "#666",
                                       dashArray = "",
                                       fillOpacity = 0.7,
                                       bringToFront = TRUE),
          label = labels) %>% 
        # addLegend(pal = pal, 
        #           values = ~crime_rate*100, 
        #           opacity = 0.01, 
        #           title = htmltools::HTML("Total Passengers 2021"),
        #           position = "bottomright") %>% 
        setView(-73.8399986, 40.746739, zoom = 10)
      
      
      
    })
  
  
  output$hist_cluster2<-renderPlotly({
    if(input[["crime type"]]==1)  
    {
      crime_rate_df<-crime_df2 %>% 
        filter(date>=input[["time"]][1],
               date<=input[["time"]][2]) %>% 
        group_by(service) %>% 
        summarise(crime_rate=sum(felony)/sum(flow))
    }
    else if(input[["crime type"]]==2)
    {
      crime_rate_df<-crime_df2 %>% 
        filter(date>=input[["time"]][1],
               date<=input[["time"]][2]) %>% 
        group_by(service) %>% 
        summarise(crime_rate=sum(misdemeanor)/sum(flow))
    }
    else if(input[["crime type"]]==3)
    {
      crime_rate_df<-crime_df2 %>% 
        filter(date>=input[["time"]][1],
               date<=input[["time"]][2]) %>% 
        group_by(service) %>% 
        summarise(crime_rate=sum(violation)/sum(flow))
    }
    else{
      crime_rate_df<-crime_df2 %>% 
        filter(date>=input[["time"]][1],
               date<=input[["time"]][2]) %>% 
        group_by(service) %>% 
        summarise(crime_rate=sum(violation+misdemeanor+felony)/sum(flow))
      
    }
    
    
    
    
    crime_rate_df %>%
      plot_ly(x = ~service, y = ~crime_rate, color = ~service, type = "bar", colors = "viridis") %>%
      layout(yaxis = list(title = 'Number of Compliants'),
             xaxis = list(title = 'service'))
    
    
  })
  
  
  output$hist_service2<-renderPlotly({
    if(input[["crime type"]]==1)  
    {
      crime_rate_df<-crime_df2 %>% 
        filter(date>=input[["time"]][1],
               date<=input[["time"]][2]) %>% 
        group_by(cluster) %>% 
        summarise(crime_rate=sum(felony)/sum(flow))
    }
    else if(input[["crime type"]]==2)
    {
      crime_rate_df<-crime_df2 %>% 
        filter(date>=input[["time"]][1],
               date<=input[["time"]][2]) %>% 
        group_by(cluster) %>% 
        summarise(crime_rate=sum(misdemeanor)/sum(flow))
    }
    else if(input[["crime type"]]==3)
    {
      crime_rate_df<-crime_df2 %>% 
        filter(date>=input[["time"]][1],
               date<=input[["time"]][2]) %>% 
        group_by(cluster) %>% 
        summarise(crime_rate=sum(violation)/sum(flow))
    }
    else{
      crime_rate_df<-crime_df2 %>% 
        filter(date>=input[["time"]][1],
               date<=input[["time"]][2]) %>% 
        group_by(cluster) %>% 
        summarise(crime_rate=sum(violation+misdemeanor+felony)/sum(flow))
      
    }
    
    
    
    
    crime_rate_df %>%
      plot_ly(x = ~cluster, y = ~crime_rate, color = ~cluster,
              type = "bar", colors = "viridis") %>%
      layout(yaxis = list(title = 'Number of Compliants'),
             xaxis = list(title = 'cluster'))
    
    
    
    
    
    
    
  })
  
  
  
  
  
  # output$summary <- renderPrint({
  #   summary(d())
  # })
  # 
  # # Generate an HTML table view of the data ----
  # output$table <- renderTable({
  #   d()
  # })
  
  
  
  
}

####################
# Create Shiny App #
####################

shinyApp(ui = ui, server = server) 



