## This is an alpha version of Warehouse CITY school, a community mapping tool for warehouse impacts
## Authored by Mike McCarthy, Radical Research LLC
## Thanks to Sean Raffuse at UC Davis AQRC for help with the nearby intersection code for great circles 
## First created September, 2022
## Last modified September, 2022
#

library(shiny)
library(leaflet)
library(htmltools)
#library(gghighlight)
#library(spatstat)
library(sf)
#library(gstat)
#library(spdep)
#library(raster)
#library(rgdal)
library(tidyverse)
library(DT)
library(markdown)
#library(automap)
#library(gstat)

## Define UI for application that displays warehouses
# Show app name and logos
ui <- fluidPage(title = 'Warehouse CITY school',
                tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
                titlePanel(
                  fluidRow(column(1),
                           column(3,
                                  div(style = 'height:60px; font-size: 30px;',
                                      'Warehouse CITY School')),
                           column(2, shiny::img(height = 60, src = 'Logo_Redford.jpg')),
                           column(2, shiny::img(height = 38, src = 'Logo.png')))
                ),
                ##Create a tabset display to have a readme file and main warehouse page
                  tabPanel('Dashboard',
                           # Display slider bar selections, checkbox, and summary text
                          # fluidRow(column(2),
                          #          column(6, align = 'center', 
                          #              sliderInput('radius', 'Selection radius (feet)', 
                          #             min = 0, max = 5000, value = 1000, step =100)
                          #          )
                                    #column(2, textOutput('text2'))
                          # ),
                          # fluidRow(column(1),
                          #          column(8, align = 'center', dataTableOutput('Summary'))
                          # ),
                           # Display map and table
                           fluidRow(
                             column(1),
                             column(8, align = 'center', leafletOutput("map", height = 600))
                           ),
                           fluidRow(column(2),
                                    column(6, align = 'center', dataTableOutput('schoolsNearWH'))),
                           fluidRow(column(9),
                                    column(3, paste('Warehouse CITY School v1.02, last updated', Sys.Date())
                                    )
                           )
                  )
)

server <- function(input, output) {
  
  
  #Create leaflet map with legend and layers control
  
  output$map <- renderLeaflet({
    map1 <- leaflet() %>%
      addTiles() %>%
      setView(lat = 34, lng = -117.60, zoom = 9) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') %>%
      addLayersControl(baseGroups = c('Basemap', 'Imagery'),
                       overlayGroups = c('Warehouses', 'Schools near warehouses', 
                       '1,000 Foot Buffer', 'all schools') , 
                       options = layersControlOptions(collapsed = FALSE)
      )  
    map1 %>% hideGroup(c('all schools'))
  })
  
  #Warehouse size bins
  observe({
    leafletProxy("map", data = warehouses) %>%
      clearGroup(group = 'Warehouses') %>%
      addPolygons(color =  '#A94915',
                  fillColor = '#A94915',
                  weight = 2,
                  fillOpacity = 0.4,
                  group = 'Warehouses',
                  label = ~htmlEscape(paste(round(shape_area,0), 'sq.ft.')))
  })
  #Circle select
  #observe({
  #  leafletProxy("map", data = circle()) %>%
  #    clearGroup(group = 'Circle') %>%
  #    addPolygons(color = 'grey50',
  #                group = 'Circle')
  #})
  observe({
    leafletProxy("map", data = schools) %>%
      clearGroup(group = 'all schools') %>%
      addPolygons(color = 'purple', weight = 0.6, 
                  fillOpacity = 0.2,
                  group = 'all schools',
      )
  })
  
  observe({
    leafletProxy("map", data = SchoolsNearWH1000) %>%
      clearGroup(group = '1,000 Foot Buffer') %>%
      addPolygons(color = 'grey', weight = 1, 
                  fillOpacity = 0.3,
                  group = '1,000 Foot Buffer',
      )
  })

  observe({
    leafletProxy("map", data = SchoolsNearWH2) %>%
      clearGroup(group = 'Schools near warehouses') %>%
      addPolygons(color = 'purple', weight = 3, fillOpacity = 0.8,
                  label = ~htmlEscape(paste(School, ' in ', District)),
                  group = 'Schools near warehouses',
                  )
  })


  
  ## Generate a data table of warehouses in selected reactive data
  output$schoolsNearWH <- DT::renderDataTable(
    schoolsNearWH, 
    server = FALSE,
    caption  = 'Schools with a warehouse within 1,000 feet',
    rownames = FALSE, 
    options = list(dom = 'Btp',
                   pageLength = 15,
                   buttons = c('csv','excel')),
    extensions = c('Buttons'),
    filter = list(position = 'top', clear = FALSE)
  )
  ## Reactive data selection logic
  # First select parcels based on checkbox and year range input
  filteredParcels <- reactive({
      selectedYears <- inland_parcels %>%
        #dplyr::filter(year_chr != 'unknown') %>%
      #  dplyr::filter(year_built >= input$year_slider[1] & year_built <= input$year_slider[2]) %>%
        mutate(shape_area = round(shape_area, 0))
    
    return(selectedYears)
  })

  ##Calculate circle around a selected point
  circle <- reactive({
    req(input$map_click)
    #FIXME - make this user defined
    distance <- input$radius*0.3048
    lat1 <- round(input$map_click$lat, 8)
    lng1 <- round(input$map_click$lng, 8)
    clickedCoords <- data.frame(lng1, lat1)
    
    dat_point <- st_as_sf(clickedCoords, coords = c('lng1', 'lat1'), crs = 4326) %>%
      st_transform(3857)
    circle_sf <- st_buffer(dat_point, distance) %>%
      st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84")
    return(circle_sf)
  })
  
  ##Code to select nearby warehouse polygons
  nearby_warehouses <- reactive({
    req(circle())
    nearby <- st_intersects(circle(), warehouses)
    nearby2 <- warehouses[nearby[[1]],] %>%
      as.data.frame() %>%
     # rename(parcel.number = apn) %>%
      mutate(acreage = round(shape_area/43560, 0)) %>%
      dplyr::select(acreage) %>%
      arrange(desc(acreage))
    
    return(nearby2)
  })
  
  ##Select between data without selection radius or with
  parcelDF_circle <- reactive({
    if (is.null(input$map_click)) {
      warehouse2 <- warehouses %>%
        as.data.frame() %>%
        #rename(parcel.number = apn) %>%
        mutate(acreage = round(shape_area/43560, 0)) %>%
        dplyr::select(acreage) %>%
        arrange(desc(acreage))
      
    }
    else {
      warehouse2 <- nearby_warehouses() %>%
        as.data.frame()
    }
    return(warehouse2)
  })
  
  ##Add variables for Heavy-duty diesel truck calculations
  

  
  #output$text2 <- renderText({
  #  req(input$map_click)
  #  paste('You clicked on', round(input$map_click$lat,5), round(input$map_click$lng,5))
  #})
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
