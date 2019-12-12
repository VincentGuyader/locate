#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
locate <- function(...) {
  with_golem_options <-   function(app, golem_opts){
    app$appOptions$golem_options <- golem_opts
    app
  }
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server), 
    golem_opts = list(...)
  )
}
