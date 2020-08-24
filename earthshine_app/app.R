#
#

# Libraries ----
library(shinydashboard)
library(leaflet)
library(googlesheets4)
library(tidyverse)
# Google sheets authentication ----
gs4_auth(
  cache = ".secrets",
  email = "owen.bezick@nissaconsulting.com"
)

# Application UI -----
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  
  # Show a plot of the generated distribution
  dashboardBody(
    includeCSS("style.css")
    , fluidRow(
      box(width = 12, status = "danger", solidHeader = T,  title = "Your Basecamp For Adventure"
          , HTML('
            <div class="container" style="text-align:center;">
                <div class="row">
                    <div class="column" style = "position: relative;">
                        <img width="25%" src="logo.png">
                        <img width="10%" src="book_now.png" style= "position: absolute; right: 0;">
                    </div>
                </div>
            </div>
               ')
          )
      )
    , fluidRow(
      box(width = 3, title = NULL
          , div(img(src = "hiker_photo.png", height = "60%", width = "100%"), style = "text-align: center;")
          , checkboxGroupInput("types"
                               , ""
                               , choices = c("Hikes", "Mountain Bike", "Waterfalls", "White Water", "Blueridge Parkway Scenic Views")
                               , selected = c("Hikes", "Mountain Bike", "Waterfalls", "White Water", "Blueridge Parkway Scenic Views"))
          , sliderInput(inputId = "distance", label = "Distance", min = 0, max = 50, step = 1, value = 50)
          , sliderInput(inputId = "budget", label = "Budget", min = 0, max = 2000, step = 5, value = 2000)
      )
      , box(width = 9, status = "primary", title = NULL, solidHeader = T
            , leafletOutput("map", height = "800px")
      )
    )
  )
)
# Application Server -----
server <- function(input, output) {
  
  map_data <- read_sheet("https://docs.google.com/spreadsheets/d/1C1xjmxRPfIKKd6nZh_S4vi39xKQs3PyxjnrbCrmG8sw/edit#gid=0")
  
  r_map_data <- reactive({
    req(input$types, input$budget, input$distance)
    df <- map_data %>%
      filter(type %in% input$types
             , price <= input$budget
             , distance_from <= input$distance)
  })
  
  output$map <- renderLeaflet({
    earthshine_content <- paste(sep = "<br/>",
                                "<b><a href='https://earthshinenc.com/'>Earthshine Lodge</a></b>",
                                "Home Base")
    data <- r_map_data()
    
    leaflet(data = data) %>%
      addProviderTiles(provider = "Esri.WorldTopoMap") %>%
      addMarkers(label = data$name
                 , popup = data$description
      ) %>%
      addPopups(lat = 35.155076, lng = -82.898274, popup = earthshine_content)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
