# Libraries ----
library(shinydashboard)
library(leaflet)
library(googlesheets4)
library(tidyverse)
library(shinyWidgets)
library(htmltools)
library(googledrive)

# Application UI -----
ui <- dashboardPage(
  # Application title
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  
  # Show a plot of the generated distribution
  dashboardBody(
    includeCSS("style.css")
    , fluidRow(
      box(width = 12, status = "danger", solidHeader = T,  title = "Your Basecamp For Adventure", height = "12vh"
          , column(width = 4)
          , column(width = 4, align = "center"
                   , tags$a(
                     href="https://earthshinenc.com/", 
                     tags$img(src= "earthshine_logo.png", 
                              title= "Earthshine Logo"
                              , width = "50%"
                              , height = "55vh"
                              
                     )
                   )
          )
          , column(width = 4, align = "right"
                   , tags$a(
                     href="https://earthshinenc.com/reservations/", 
                     target = "_blank"
                     , tags$img(src="book_now.png", 
                                title="book now", 
                                width="20%"
                                
                     )
                   )
          )
      )
    )
    , fluidRow(
      box(width = 3, title = NULL, height = "80vh"
          , div(
            img(src = "hiker_photo.png"
                , height = "30%"
                , width = "50%%")
            , style = "text-align: center;"
          )
          , uiOutput("typecheckbox")
          , uiOutput("distanceSlider")
          # , uiOutput("budgetPicker")
          , div(textOutput("selection_number"), class = "selection-number")
      )
      , box(width = 9, status = "primary", title = NULL, solidHeader = T, height = "80vh"
            , leafletOutput("map", height = "77vh")
      )
    )
  )
)
# Application Server -----
server <- function(input, output) {
  options(gargle_oauth_cache = ".new_secrets")# designate project-specific cache
  app <- httr::oauth_app(appname = "earthshine_desktop_client",
                         key = "828707814267-k11guhpt2ge8s0hq2ipohu1q01bipinu.apps.googleusercontent.com"
                         , secret = "Q_L23RyvgDyY026uItksyGaY")
  
  gs4_auth_configure(app = app)
  
  gs4_auth(email = "owen.bezick@nissaconsulting.com"
           , cache = ".new_secrets")
  # Data -----
  map_data <- read_sheet("https://docs.google.com/spreadsheets/d/1C1xjmxRPfIKKd6nZh_S4vi39xKQs3PyxjnrbCrmG8sw/edit#gid=0")
  
  r_map_data <- reactive({
    req(input$types, input$distance)
    df <- map_data %>%
      filter(type %in% input$types
             # , price %in% input$budget
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
    ashevilleIcon <- makeIcon(iconUrl = "www/asheville.png", iconHeight = 40, iconWidth = 100, className = "logoIconClass")
    greenvilleIcon <- makeIcon(iconUrl = "www/greenville.jpg", iconHeight = 40, iconWidth = 100, className = "logoIconClass")
    data <- r_map_data()
    data$popup_text <- paste0('<center>'
                              , '<strong>', data$name, '</strong>'
                              , '<br>'
                              , '<img src = '
                              ,"'"
                              , data$thumbnail_link
                              , "' style='width:100%;height:100%; text-align:center;'"
                              , '>'
                              , '</center>'
                              , '<br>'
                              , '<strong>', "Address", '</strong>'
                              , '<br>'
                              , data$address_line_one
                              , '<br>'
                              , data$address_line_two
                              , '<br>'
                              ,'<strong>', "Description", '</strong>'
                              , '<br>'
                              , data$description
    ) %>%
      lapply(HTML)
    
    leaflet(data = data) %>%
      addProviderTiles(provider = "Esri.WorldTopoMap") %>%
      addMarkers(label = data$name
                 , popup = data$popup_text
      ) %>%
      addMarkers(lat = 35.155076, lng = -82.898274
                 , icon = logoIcon
                 , popup = HTML(
                   '<a href="https://earthshinenc.com/"> 
                   Website 
                   </a>
                   <br>
                   <a href="https://earthshinenc.com/reservations/">
                   Book Now
                   </a>')
      ) %>%
      addMarkers(
        lat = 35.5951
        , lng = -82.5515
        , icon = ashevilleIcon
        , popup = HTML("Asheville")
      ) %>%
      addMarkers(
        lat = 34.8526
        , lng = -82.3940
        , icon = greenvilleIcon
        , popup = HTML("Asheville")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
