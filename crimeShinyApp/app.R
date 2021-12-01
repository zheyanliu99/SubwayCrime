##########################################
# author: "Youyuan Kong & Stephen Yuan"  #
# date: "11/30/2021"                     #
##########################################
library(shiny)
library(leaflet)
library(tidyverse)

# data import
crime_df = read_csv('./data/subwaycrime_with_station_new.csv')
crime_df<-crime_df %>% 
  mutate(cmplnt_to_dt=as.Date(cmplnt_to_dt,format='%m/%d/%Y')) %>% 
  filter(distance<=0.0001)%>% 
  filter(cmplnt_to_dt>='2006-1-1')

crime_choices = 
  crime_df %>% 
  count(ofns_desc) %>% 
  ##  mutate(ofns_desc = fct_reorder(ofns_desc, n)) %>% 
  slice_max(n,n=10) %>% 
  distinct(ofns_desc) %>% 
  pull()

# user interface
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(

)

# server
server <- function(input, output){}

shinyApp(ui, server)