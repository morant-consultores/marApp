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
      )
    ),
    fluidRow(
      box(width = 6,
          visNetwork::visNetworkOutput(ns("red"))
      ),
      box(width = 6,
          fluidRow(
            plotOutput(ns("barras"))
          ),
          fluidRow(
            shinyjs::hidden(actionButton(inputId = ns("siguiente"), "Siguiente"))
          )

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
      if(length(opciones) > 1){
        shinyjs::show("siguiente")
      }
    })

    pagina <- reactiveVal(1)

    observeEvent(input$siguiente,{
      pagina(pagina() + 1)
    })

    output$barras <- renderPlot({

      pag <- pagina() %% (length(barras())+1)
      if(pag == 0) {
        pagina(pagina() + 1)
        pag <- pagina() %% (length(barras())+1)
      }
      barras() %>% purrr::pluck(pag)
    })
  })
}

## To be copied in the UI
# mod_mar_ui("mar_1")

## To be copied in the server
# mod_mar_server("mar_1")
