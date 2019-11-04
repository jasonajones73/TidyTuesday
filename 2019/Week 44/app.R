# Load all the packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(leaflet)
library(shinycssloaders)
library(htmltools)

# Source external file
source(file = "url.R")

# Load universal data
nyc_squirrels <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

# Define user interface
ui <- navbarPage(title = "Rats in Cute Outfits",
                 collapsible = TRUE,
                 fluid = TRUE,
                 theme = shinytheme(theme = "flatly"),
                 header = list(tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
                               tags$head(HTML('<style>* {font-size: 100%; font-family: Roboto Mono;}</style>'))),
                 
                 tabPanel(title = "Main",
                   
                  fluidPage(
                    fluidRow(
                      column(width = 1),
                      column(width = 3,
                             tags$p("This Shiny application allows you to first select time of day and then select what you are
                                    interested in seeing or hearing. When you make your selection, squirrel clusters are computed
                                    and the centers of those clusters are displayed on the map as suggestions for where you should
                                    stand if you want to see or hear what you are interested in."),
                             selectInput(inputId = "time",
                                         label = "Please choose a time of day:",
                                         choices = c("AM", "PM"),
                                         multiple = FALSE,
                                         selectize = TRUE,
                                         selected = "AM",
                                         width = "100%"), 
                             selectInput(inputId = "interest",
                                         label = "What do you want to see or hear?",
                                         choices = c("Running", "Chasing",
                                                     "Climbing", "Eating",
                                                     "Foraging", "Kuks",
                                                     "Quaas"),
                                         selected = "Running",
                                         multiple = FALSE,
                                         selectize = TRUE,
                                         width = "100%")),
                      column(width = 7,
                             tags$h3("Here are five suggestions for where you should stand or sit:\n"),
                             tags$em(tags$p("You may need to zoom out to see all five markers!")),
                             leafletOutput(outputId = "map",
                                           height = 600) %>% withSpinner(type = 1)),
                      column(width = 1)
                    )
                    
                  ) #Closing fluidPage
                  ) #closing first tabPanel
                 ) #Closing navbarPage
  

# Define server logic
server <- function(input, output, session) {
  
  time_filt <- reactive({
    nyc_squirrels %>%
      filter(shift == input$time)
  })
    
  activity_filt <- reactive({
    if(input$interest == "Running") {
      time_filt() %>%
        filter(running == TRUE)
    } else if (input$interest == "Chasing") {
      time_filt() %>%
        filter(chasing == TRUE)
    } else if (input$interest == "Climbing") {
      time_filt() %>%
        filter(climbing == TRUE)
    } else if (input$interest == "Eating") {
      time_filt() %>%
        filter(eating == TRUE)
    } else if (input$interest == "Foraging") {
      time_filt() %>%
        filter(foraging == TRUE)
    } else if (input$interest == "Kuks") {
      time_filt() %>%
        filter(kuks == TRUE)
    } else
      time_filt() %>%
        filter(quaas == TRUE)
  })
  
  squirrel_clusters <- reactive({
    set.seed(1987)
    
    activity_filt() %>%
      select(long, lat) %>%
      kmeans(centers = 5) %>%
      .$centers %>%
      as_tibble() %>%
      mutate(cluster = row_number()) %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326)
  })
  
  output$map <- renderLeaflet({
    squirrel_clusters() %>%
      leaflet() %>%
      addTiles(urlTemplate = urltemplate,
               attribution = HTML("<a href='https://www.maptiler.com/copyright/' target='_blank'>© MapTiler</a> <a href='https://www.openstreetmap.org/copyright' target='_blank'>© OpenStreetMap contributors</a>")) %>%
      addMarkers()
  })
  
  
}


shinyApp(ui = ui, server = server)