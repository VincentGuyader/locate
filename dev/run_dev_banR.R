# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
rm(list=ls())
# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

library(shiny)
if (interactive()) {
  ui <- fluidPage(
    # useShinyalert(),
    mod_banRUI("locate")
  )
  
  server <- function(input, output, session) {
    r <- reactiveValues()
    callModule(mod_banR, "locate", r)
  }
  
  shinyApp(ui, server)
}
