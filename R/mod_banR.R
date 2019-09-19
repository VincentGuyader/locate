#' @importFrom shinyalert useShinyalert
#' @importFrom leaflet leafletOutput
#' @importFrom shinyBS bsButton
#' @importFrom magrittr %>% 
#' @import leaflet

mod_banRUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
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
#' @importFrom banR geocode reverse_geocode
#' @importFrom sp SpatialPoints over

mod_banR <- function(input, output, session, r, polyfr) {
  data(city,package = "locate")

  # > terrain = "http://tile.stamen.com/terrain-background/{z}/{x}/{y}.jpg"
  # > leaflet(quakes) %>% addTiles(terrain)
  # Map par défaut
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
                                          group = NULL
                                                                                    )
 
     })
  
  
  # Render de la map
  output$mymap <- renderLeaflet({
    r$geoloc_map
  })

  observeEvent(input$mymap_reverse_search_feature_found, {

lon <- as.numeric(input$mymap_reverse_search_feature_found$result$lon)
lat <- as.numeric(input$mymap_reverse_search_feature_found$result$lat)

message(lon)
message(lat)
   
    
  })
  
  
}
