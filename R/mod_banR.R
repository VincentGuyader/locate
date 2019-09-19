#' @importFrom shinyalert useShinyalert shinyalert
#' @importFrom leaflet leafletOutput
#' @importFrom magrittr %>% 
#' @import leaflet
#' @importFrom  dplyr filter

mod_banRUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(useShinyalert(),
    tags$head(tags$style("
/* Change cursor when over entire map */
.leaflet-container {
  cursor: crosshair !important;
}")),
    # useShinyalert(),
    
     br(),
    fluidRow(
      # Render de la carte
      leafletOutput(ns("mymap")
                    # ),
      , height = 600, width = "100%"),
      tags$br()
    )
  )
}

#' @importFrom leaflet leaflet renderLeaflet addTiles setView addMarkers addControl leafletOptions
#' @importFrom sp SpatialPoints over

mod_banR <- function(input, output, session, r, polyfr) {
  data(city,package = "locate")

  ville <- reactive({
    "Paris"
    
  })
  
  observe({
    r$geoloc_map <- leaflet(
      options = 
        leafletOptions(zoomControl = TRUE,
                       dragging = TRUE,
                       maxZoom=15,
                       minZoom=6,
                       trackResize=FALSE, 
                       boxZoom = FALSE,
                       doubleClickZoom = TRUE )
      
    ) %>%
      # addProviderTiles("cartoDB") %>%
      # addTiles() %>% 
      addTiles("http://tile.stamen.com/terrain-background/{z}/{x}/{y}.jpg") %>%
      setView(lng =  1.322813,
              lat = 47.34429,
              zoom = 6)     %>%
      leaflet.extras::removeSearchOSM() %>% 
      leaflet.extras::addReverseSearchOSM(showSearchLocation = TRUE,
                                          displayText = FALSE,
                                          showBounds = FALSE,
                                          showFeature = FALSE, 
                                          fitBounds = FALSE, 
                                          group = NULL) %>% 
      
      addControl(tags$html("Pouvez vous localiser",
                           br(), em(ville())),
                 position = "topright")
 
     })
  
  
  # Render de la map
  output$mymap <- renderLeaflet({
    r$geoloc_map
  })

  observeEvent(input$mymap_reverse_search_feature_found, {

lon <- as.numeric(input$mymap_reverse_search_feature_found$result$lon)
lat <- as.numeric(input$mymap_reverse_search_feature_found$result$lat)

# message(lon)
# message(lat)
   
v<-city %>% filter(name==ville())
dist <- distance(lon1 = lon,lat1 = lat,
         lon2 = v$long,lat2 = v$lat)
message(dist)
  shinyalert(
    # imageHeight = 50,imageWidth = 50,
    title = "",
    text = glue::glue("Vous êtes à {print(dist)} de {ville()}.")
    # ,
    # type = "info"
  )

  })
  
  
}
