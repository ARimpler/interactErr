#' uncertainty UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_uncertainty_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' uncertainty Server Functions
#'
#' @noRd 
mod_uncertainty_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_uncertainty_ui("uncertainty_1")
    
## To be copied in the server
# mod_uncertainty_server("uncertainty_1")
