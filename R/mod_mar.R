#' mar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mar_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mar Server Functions
#'
#' @noRd 
mod_mar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mar_ui("mar_1")
    
## To be copied in the server
# mod_mar_server("mar_1")
