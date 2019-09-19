#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  r <- reactiveValues()
  callModule(mod_banR, "locate", r)
}
