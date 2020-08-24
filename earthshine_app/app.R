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
          , column(width = 4)
          , column(width = 4, align = "center"
                   , tags$a(
                     href="https://earthshinenc.com/", 
                     tags$img(src="logo.png", 
                              title="Earthshine Logo", 
                              width="50%",
                     )
                   )
          )
          , column(width = 4, align = "right"
                   , tags$a(
                     href="https://earthshinenc.com/reservations/", 
                     tags$img(src="book_now.png", 
                              title="nook now", 
                              width="20%",
                     )
                   )
          )
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
    earthshine_content <- '<a href="https://earthshinenc.com/"> <img src = "logo.png" style = "width:100%;" > </a>'
    data <- r_map_data()
    
    leaflet(data = data) %>%
      addProviderTiles(provider = "Esri.WorldTopoMap") %>%
      addMarkers(label = data$name
                 , popup = data$description
      ) %>%
      addPopups(lat = 35.155076, lng = -82.898274
                , popup = earthshine_content
                , options = popupOptions(keepInView = TRUE, closeButton = FALSE, closeOnClick = FALSE)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
