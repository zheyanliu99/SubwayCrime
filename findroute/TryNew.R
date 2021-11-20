library(DT)
library(shiny)

library(dplyr)

items <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv'
  ) %>% select(-num_id,-sell_currency,-buy_currency,-games_id,-id_full) %>%
  unique() %>%
  head(100)  
villagers <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv'
  ) %>% 
  select(-row_n) %>% 
  unique()


datatable(villagers,
          options = list(paging = TRUE,    ## paginate the output
                         pageLength = 15,  ## number of rows to output for each page
                         scrollX = TRUE,   ## enable scrolling on X axis
                         scrollY = TRUE,   ## enable scrolling on Y axis
                         autoWidth = TRUE, ## use smart column width handling
                         server = FALSE,   ## use client-side processing
                         dom = 'Bfrtip',
                         buttons = c('csv', 'excel'),
                         columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                           list(targets = c(0, 8, 9), visible = FALSE))
          ),
          extensions = 'Buttons',
          selection = 'single', ## enable selection of a single row
          filter = 'bottom',              ## include column filters at the bottom
          rownames = FALSE                ## don't show row numbers/names
)

library(shiny)

ui <- fluidPage(titlePanel("DT table in Shiny"),
                mainPanel(width = 12,
                          DT::dataTableOutput("mytable")))

server <- function(input, output) {
  output$mytable <- DT::renderDataTable(villagers,
                                        options = list(scrollX = TRUE),
                                        rownames = FALSE)
}

library(formattable)

## Prepare the item data
pic_items <- items %>%
  mutate(picture = ifelse(!is.na(image_url), 
                          paste0("<img src=\"",image_url, "\" height=\"30\" data-toggle=\"tooltip\" data-placement=\"right\" title=\"", name, "\"></img>"), 
                          as.character(icon("question", lib = "font-awesome")))) %>%
  select(picture, name, category, orderable, sell_value, buy_value) %>%
  unique()

## A formatter function to format TRUE/FALSE values
true_false_formatter <-
  formatter("span",
            style = x ~ style(
              font.weight = "bold",
              color = ifelse(x == TRUE, "forestgreen", ifelse(x == FALSE, "red", "black"))
            ))

## Use formattable
formattable(
  head(pic_items),
  list(
    ## colour this column in a white to pink gradiant by value
    `sell_value` = color_tile("white", "pink"),
    
    ## a coloured bar with length proportional to value
    `buy_value` = color_bar("lightblue"),
    
    ## use custom formatter for TRUE/FALSE values
    orderable = true_false_formatter
  )
)
