##########################################
# author: "Youyuan Kong & Stephen Yuan"  #
# date: "11/30/2021"                     #
##########################################
library(shiny)
library(leaflet)
library(tidyverse)



# user interface
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage( "hello world"

)

# server
server <- function(input, output){}

shinyApp(ui, server)