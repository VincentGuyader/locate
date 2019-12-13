#' @importFrom shinyalert useShinyalert shinyalert
#' @importFrom leaflet leafletOutput
#' @importFrom magrittr %>% 
#' @import leaflet
#' @importFrom  dplyr filter slice top_frac arrange mutate
#' @importFrom  tibble tibble

mod_locateUI <- function(id) {
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
      checkboxInput(inputId = ns("soluce"),label = "Hide answer",value = FALSE),
      column(width = 6,sliderInput(inputId = ns("difficulte"),
                                   label = "difficulté",min = 1,max = 5,value = 2,step = 1
      )),
      column(width = 6,gaugeOutput(ns("score"), width = "100%", height = "200px")),
      DT::DTOutput(ns("resultat")),
      actionButton(ns("restart"),label = "Restart"),
      tags$br()
    )
  )
}

#' @importFrom leaflet leaflet renderLeaflet addTiles setView addMarkers addControl leafletOptions
#' @importFrom sp SpatialPoints over
#' @importFrom dplyr sample_n pull
#' @import flexdashboard

mod_locate <- function(input, output, session, r, polyfr) {
  data(city,package = "locate")
  city2 <- city %>% dplyr::filter(lat> 40,lat <56,long> -10,long<20) %>% 
   dplyr::filter( !(lat> 40 &lat <56 &long> 8.5&long<10))
  
difficulte <- c("1" =  100/36700,
                "2"=  500/36700,
                "3" =  5000/36700,
                "4" =  15000/36700,
                "5" =  36700/36700)

    coffret <- reactiveValues(old_point =NULL,
                              resultat =list(),
                            ville = "Paris",
                            score = 1000,
                            onjoue = TRUE
                            )
  
  # icone pour solution
  ic <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "red"
  )
  # effet de la difficulté

  
  
  
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
      addTiles("http://tile.stamen.com/terrain-background/{z}/{x}/{y}.jpg") %>%
      setView(lng =  1.322813,
              lat = 47.34429,
              zoom = 6)     %>%
      leaflet.extras::removeSearchOSM() %>%
      addControl(tags$html("Pouvez vous localiser",
                           br(), em(coffret$ville)),
                 position = "topright")
 

    
     })
  
  
  # Render de la map
  output$mymap <- renderLeaflet({
    r$geoloc_map
  })
  

  
  observeEvent( input$mymap_click , {
    req(coffret$onjoue == TRUE)
    lon <- as.numeric(input$mymap_click$lng)
    lat <- as.numeric(input$mymap_click$lat)
    v <- city %>% filter(name==coffret$ville) %>% arrange(desc(pop)) %>% slice(1)
    message(coffret$ville)
    message("lon1 = ",lon)
    message("lat1 = ",lat)
    message("lon2 = ",v$long)
    message("lat2 = ",v$lat)
    d <- distance(lon1 = lon,lat1 = lat,
                     lon2 = v$long,lat2 = v$lat)
    
    dist <- d$affichage
    dist_v <-d$valeur
    pop <- v$pop
    
    
    leafletProxy('mymap') %>% 
      removeMarker("proposition") %>% 
      removeMarker("solution") %>% 
      addMarkers(lng=lon, lat=lat,layerId="proposition")
    
    
    if ( !input$soluce){
    leafletProxy('mymap') %>% 
      addAwesomeMarkers(lng = v$long, lat = v$lat, layerId = "solution",icon = ic)
    }
    
    
    coffret$old_point <- c(lng=lon, lat=lat)
    
    shinyalert(
      title = "",
      text = glue::glue("Vous êtes à {print(dist)} de {coffret$ville}.")
    )
    coffret$resultat <- append(coffret$resultat,
                              list(
                               tibble(ville = coffret$ville,dist = dist_v,
                                          population = pop,
                                          difficulte = input$difficulte
                                          )
                              )
                               
                               )
    coffret$score <- coffret$score - round(dist_v/1000)
    
    coffret$ville <- city %>%
      top_frac(difficulte[as.character(input$difficulte)],pop) %>% 
      sample_n(1) %>%
      pull(name)
    # coffret$ville <- "Paris"
        
  })
  
  output$score <- renderGauge({
      gauge(coffret$score, 
            min = 0, 
            max = 1000, 
            sectors = gaugeSectors(success = c(700,1000), 
                                   warning = c(450, 700),
                                   danger = c(0, 450)))
    
    
  })
  
  
  observeEvent( coffret$score , {
  if (coffret$score <=0){
    coffret$onjoue <- FALSE
    shinyalert(
      title = "Fin de partie",type = "error",
      text = glue::glue("Vous avez positioné {length(coffret$resultat)} villes en difficulté {round(mean(resultat()$difficulte),1)}.")
    )
    
    
  }
  })
  
  resultat <- eventReactive(coffret$resultat,{
    req(length(coffret$resultat)>1)
    dplyr::bind_rows(coffret$resultat) %>% 
      mutate(dist=round(dist))
    
  })
  
  output$resultat <- DT::renderDT({
resultat()
  })
  
  
  observeEvent( input$restart , {
  
                coffret$resultat =list()
                coffret$ville = "Paris"
                coffret$score = 1000
                coffret$onjoue = TRUE
                leafletProxy('mymap') %>% 
                  removeMarker("proposition") %>% 
                  removeMarker("solution") 
                
    
  })
  
}
