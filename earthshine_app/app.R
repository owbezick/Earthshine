#
#

# Libraries ----
library(shinydashboard)
library(leaflet)
library(googlesheets4)
library(tidyverse)
library(shinyWidgets)
library(htmltools)
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
                              width="50%"
                     )
                   )
          )
          , column(width = 4, align = "right"
                   , tags$a(
                     href="https://earthshinenc.com/reservations/", 
                     tags$img(src="book_now.png", 
                              title="nook now", 
                              width="20%"
                     )
                   )
          )
      )
    )
    , fluidRow(
      box(width = 3, title = NULL
          , div(img(src = "hiker_photo.png", height = "60%", width = "100%"), style = "text-align: center;")
          , uiOutput("typecheckbox")
          , uiOutput("distanceSlider")
          , uiOutput("budgetPicker")
          , div(textOutput("selection_number"), class = "selection-number")
      )
      , box(width = 9, status = "primary", title = NULL, solidHeader = T
            , leafletOutput("map", height = "800px")
      )
    )
  )
)
# Application Server -----
server <- function(input, output) {
  # Data -----
  map_data <- read_sheet("https://docs.google.com/spreadsheets/d/1C1xjmxRPfIKKd6nZh_S4vi39xKQs3PyxjnrbCrmG8sw/edit#gid=0")
  
  r_map_data <- reactive({
    req(input$types, input$budget, input$distance)
    df <- map_data %>%
      filter(type %in% input$types
             , price %in% input$budget
             , distance_from <= input$distance)
  })
  
  # Filters -----
  output$typecheckbox <- renderUI({
    types <- unique(map_data$type)
    checkboxGroupInput("types"
                       , div("Adventures", class = "picker-titles")
                       , choices = types
                       , selected = types)
  })
  
  output$distanceSlider <- renderUI({
    maxDistance <- max(map_data$distance_from)
    minDistance <- min(map_data$distance_from)
    sliderInput(inputId = "distance"
                , label = div("Distance (mi)", class = "picker-titles")
                , min = minDistance
                , max = maxDistance
                , step = 1
                , value = maxDistance)
  })
  
  output$budgetPicker <- renderUI({
    choices = list("Free" = 0, "$" = 1,"$$" = 2, "$$$" = 3)
    pickerInput(inputId = "budget", label = div("Budget", class = "picker-titles"), choices = choices, selected = choices, multiple = T)
  })
  
  r_selection_number <- reactive(
    nrow(r_map_data())
  )
  
  # UI Outputs ----
  output$selection_number <- renderText({
    number <- r_selection_number()
    if (number == 1){
      text = "result found"
    } else {
      text = "results found"
    }
    paste(number, text)
  })
  
  output$map <- renderLeaflet({
    logoIcon <- makeIcon(iconUrl = "www/logo.png", iconHeight = 40, iconWidth = 100, className = "logoIconClass")
    data <- r_map_data()
    data$popup_text <- paste0('<strong>', data$name, '</strong>'
                              , '<br>'
                              , data$description
                              , '<br>'
                              , data$address_line_one
                              , '<br>'
                              , data$address_line_two) %>%
      lapply(HTML)
    
    leaflet(data = data) %>%
      addProviderTiles(provider = "Esri.WorldTopoMap") %>%
      addMarkers(label = data$name
                 , popup = data$popup_text
      ) %>%
      addMarkers(lat = 35.155076, lng = -82.898274
                 , icon = logoIcon
                 , popup = HTML('<a href="https://earthshinenc.com/"> Website </a> <br> <a href="https://earthshinenc.com/reservations/"> Book Now </a>')
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
