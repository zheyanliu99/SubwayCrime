library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% 
      addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)











mydf <- data.frame(Observation = c("A", "B"),
                   InitialLat = c(62.469722,48.0975),
                   InitialLong = c(6.187194, 16.3108),
                   NewLat = c(51.4749, 51.4882),
                   NewLong = c(-0.221619, -0.302621),
                   stringsAsFactors = FALSE)

mydf2 <- data.frame(group = c("A", "B"),
                    lat = c(mydf$InitialLat, mydf$NewLat),
                    long = c(mydf$InitialLong, mydf$NewLong))

#  group      lat      long
#1     A 62.46972  6.187194
#2     B 48.09750 16.310800
#3     A 51.47490 -0.221619
#4     B 51.48820 -0.302621

library(leaflet)
library(magrittr)

leaflet()%>%
  addTiles() %>%
  addPolylines(data = mydf2, lng = ~long, lat = ~lat, group = ~group)



library(tidyverse)

df = read_csv('findroute/directions.csv')
df %>% 
  pivot_longer(ends_with('lat'), names_to = 'journey', values_to = 'lat') %>% 
  pivot_longer(ends_with('lng'), names_to = 'journey2', values_to = 'lng') %>% 
  filter((journey == 'departure_stop_lat' & journey2 == 'departure_stop_lng')|(journey == 'arrival_stop_lat' & journey2 == 'arrival_stop_lng')) %>% 
  mutate(group = paste(as.character(route_num), line))













