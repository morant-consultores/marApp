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
    fluidRow(
      col_6(
        actionButton(inputId = ns("correr"),label =  "Graficar")
      ),
      col_6(
        selectInput(ns("grupos"), "Grupo", choices = c("Seleccione" = ""))
      )
    ),
    fluidRow(
      box(width = 6,
          visNetwork::visNetworkOutput(ns("red"))
      ),
      box(width = 6,
          plotOutput(ns("barras"))
      )
    )
  )
}

#' mar Server Functions
#'
#' @noRd
mod_mar_server <- function(id, bd){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    comparaciones <- eventReactive(input$correr,{
      bd$comparaciones %>%
        semi_join(bd$combinaciones %>% filter(comparada == 1), by = "id_combinacion") %>% collect()
    })

    resumen <- eventReactive(input$correr,{

      estimar_influencia(comparativo = comparaciones(), actores_tb = bd$actores)

    })

    output$red <- visNetwork::renderVisNetwork({
      graficar_red(comparaciones(), resumen(), base_actores = bd$actores)
    })

    barras <- reactive({
      graficar_influencia(resumen())
    })

    observeEvent(barras(),{
      opciones <- names(barras())
      updateSelectInput(inputId = "grupos",choices = opciones, selected = opciones[[1]])
    })

    output$barras <- renderPlot({
      req(input$grupos)
      barras() %>% purrr::pluck(input$grupos)
    })
  })
}

## To be copied in the UI
# mod_mar_ui("mar_1")

## To be copied in the server
# mod_mar_server("mar_1")
