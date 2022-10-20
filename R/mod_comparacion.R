#' comparacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comparacion_ui <- function(id){
  ns <- NS(id)
  tagList(
    # actionBttn(inputId = ns("borrar"), "Reiniciar todo"),
    shinyWidgets::progressBar(id = ns("progreso"), value = 0, display_pct = T, status = "warning",size = "sm",commas = T),
    shinyjs::hidden(
      div(id = ns("nada"),
          bs4Quote("Nada por clasificar", color = "info")
      )),
    shinyjs::hidden(
      div(id = ns("formulario"),
          tagList(
            bs4Quote("¿Quién de estos dos personajes tiene mayor influencia o poder?", color = "info"),
            fluidRow(
              userBox(
                id = ns("actor_1"),
                title = userDescription(
                  title = "",
                  subtitle = "",
                  type = 1,
                  image = NULL
                ),
                status = "info",
                gradient = TRUE,
                background = "gray",
                boxToolSize = "xl",
                collapsible = F,
                footer = prettyCheckbox(
                  inputId = ns("ele_actor_1"),
                  label = "Más influyente",
                  value = F,
                  status = "info",
                  icon = icon("thumbs-up"),
                  plain = TRUE,
                  outline = TRUE
                ),
                textOutput(ns("cargo_1")),
                textOutput(ns("descripcion_1"))
              ),
              userBox(
                id = ns("actor_2"),
                title = userDescription(
                  title = "",
                  subtitle = "",
                  type = 2,
                  image = NULL
                ),
                status = "info",
                gradient = TRUE,
                background = "gray",
                boxToolSize = "xl",
                collapsible = F,
                footer = prettyCheckbox(
                  inputId = ns("ele_actor_2"),
                  label = "Más influyente",
                  value = F,
                  status = "info",
                  icon = icon("thumbs-up"),
                  plain = TRUE,
                  outline = TRUE
                ),
                textOutput(ns("cargo_2")),
                textOutput(ns("descripcion_2"))
              )
            ),
            fluidRow(
              checkboxInput(ns("empate"), "Mismo nivel de influencia")
            ),
            hr(),
            bs4Quote("¿Qué tan cercanos son entre ellos estos dos personajes?", color = "success"),
            fluidRow(
              radioGroupButtons(
                inputId = ns("relacion"),
                label = "",
                choices = c("No sé","No tienen relación", "Muy alejados", "Alejados", "Neutral", "Algo cercanos", "Muy cercanos"),
                justified = TRUE,
                selected = "",status = "success",
                width = "100%",
                checkIcon = list(
                  yes = icon("ok",
                             lib = "glyphicon"))
              )
            ),
            fluidRow(
              col_6(
                actionBttn(
                  inputId = ns("saltar"),
                  label = "Saltar",
                  style = "fill",
                  block = T
                )
              ),
              col_6(
                actionBttn(
                  inputId = ns("siguiente"),
                  label = "Siguiente",
                  style = "fill",
                  block = T
                )
              )
            )
          )
      )
    )
  )
}

#' comparacion Server Functions
#'
#' @noRd
mod_comparacion_server <- function(id, bd, usuario){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(gargoyle::watch("actualizar_combinaciones"),{
      valor <- bd$combinaciones %>% count(comparada) %>% collect()
      shinyWidgets::updateProgressBar(id = "progreso", value = valor %>% filter(comparada == 1) %>% pull(n),total = sum(valor$n),commas = T)
    })

    observeEvent(input$ele_actor_1,{
      updateBox(id = "actor_1", action = "update",
                options = list(
                  background = if_else(input$ele_actor_1, "info", "gray")
                )
      )
    }, ignoreInit = T)

    observeEvent(input$ele_actor_2,{
      updateBox(id = "actor_2", action = "update",
                options = list(
                  background = if_else(input$ele_actor_2, "info", "gray")
                )
      )
    }, ignoreInit = T)

    # observeEvent(c(input$ele_actor_1, input$ele_actor_2),{
    #   if(input$ele_actor_1 + input$ele_actor_2 > 1){
    #     shinyalert::shinyalert("¡Atención!", "Sólo puede seleccionar un actor", type = "warning",
    #                            closeOnEsc = F,closeOnClickOutside = F,
    #                            callbackR = function(x){
    #                              updatePrettyCheckbox(inputId = "ele_actor_1",value = F)
    #                              updatePrettyCheckbox(inputId = "ele_actor_2",value = F)
    #                            })
    #   }
    # })

    observeEvent(input$empate,{
      updatePrettyCheckbox(inputId = "ele_actor_1", value = input$empate)
      updatePrettyCheckbox(inputId = "ele_actor_2", value = input$empate)
    },ignoreInit = T)
    #
    #     observeEvent(c(input$ele_actor_1, input$ele_actor_2),{
    #
    #       if((input$ele_actor_1 + input$ele_actor_2) < 2) updateCheckboxInput(inputId = "empate", value = F) else{
    #         updateCheckboxInput(inputId = "empate", value = T)
    #       }
    #
    #     },ignoreInit = T)

    contador <- reactiveVal(1)

    por_clasificar <- eventReactive(gargoyle::watch("actualizar_combinaciones"),{
      req(usuario)
      combinaciones <- bd$combinaciones %>% filter(comparada <= 0) %>% arrange(desc(comparada)) %>% collect()

      aux <- combinaciones %>%
        select(id_combinacion, id_actor_1, id_actor_2) %>%
        left_join(bd$actores %>%
                    transmute(id_actor_1 = id_actor,
                              nombre_1 = nombre,
                              apellidos_1 = glue::glue("{apellido_paterno} {apellido_materno}"),
                              foto_1 = foto,
                              cargo_1 = cargo,
                              descripcion_1 = descripcion)) %>%
        left_join(bd$actores %>%
                    transmute(id_actor_2 = id_actor, nombre_2 = nombre,
                              apellidos_2 = glue::glue("{apellido_paterno} {apellido_materno}"),
                              foto_2 = foto,
                              cargo_2 = cargo,
                              descripcion_2 = descripcion))

      contador(1)
      return(aux)
    })

    renglon <- eventReactive(c(contador(), por_clasificar()),{
      por_clasificar() %>% slice(contador())
    })

    observeEvent(input$siguiente,{

      if((input$ele_actor_1 + input$ele_actor_2 == 0) | is.null(input$relacion)){
        shinyalert::shinyalert("¡Atención!", "Favor de contestar ambas preguntas", type = "warning")
      } else{
        #guardar resultado
        renglon() %>% select(starts_with("id_")) %>%
          mutate(id_actor_1_mas_influyente = input$ele_actor_1,
                 id_actor_2_mas_influyente = input$ele_actor_2,
                 relacion = input$relacion, creado = lubridate::now(tz = "America/Mexico_City")) %>%
          DBI::dbWriteTable(pool, tbl_comparaciones, value = .,append = T)

        #quitar de la cola
        DBI::dbExecute(pool, glue::glue("UPDATE {tbl_combinaciones} set comparada = '1' WHERE id_combinacion = '{renglon()$id_combinacion}'"))

        updatePrettyCheckbox(inputId = "ele_actor_1",value = F)
        updatePrettyCheckbox(inputId = "ele_actor_2",value = F)
        updateCheckboxInput(inputId = "empate", value = F)
        updateRadioGroupButtons(inputId = "relacion",selected = character(0))

        contador(contador()+ 1)
      }

    })

    observeEvent(input$saltar,{
      glue::glue("UPDATE {tbl_combinaciones} SET comparada = comparada - 1 WHERE id_combinacion = '{renglon()$id_combinacion}'") %>%
        DBI::dbExecute(pool,.)
      contador(contador() + 1)
    })

    output$cargo_1 <- renderText({
      if(is.na(renglon()$cargo_1)) "Sin cargo" else glue::glue("Cargo: {renglon()$cargo_1}")
    })

    output$cargo_2 <- renderText({
      if(is.na(renglon()$cargo_2)) "Sin cargo" else glue::glue("Cargo: {renglon()$cargo_2}")
    })

    output$descripcion_1 <- renderText({
      if(is.na(renglon()$descripcion_1)) "Sin descripción" else renglon()$descripcion_1
    })

    output$descripcion_2 <- renderText({
      if(is.na(renglon()$descripcion_2)) "Sin descripción" else renglon()$descripcion_2
    })

    observeEvent(renglon(),{

      updateBox(id = "actor_1",action = "update",
                options = list(title = userDescription(
                  title = renglon()$nombre_1, subtitle = renglon()$apellidos_1, type = 2,
                  image = if_else(!is.na(renglon()$foto_1) & (renglon()$foto_1 != "") & (substr(renglon()$foto_1,nchar(renglon()$foto_1)-3, nchar(renglon()$foto_1)) %in% c(".png",".jpg",".svg","jpeg", "JPEG")), renglon()$foto_1, "https://upload.wikimedia.org/wikipedia/commons/5/50/User_icon-cp.svg")
                ))
      )



      updateBox(id = "actor_2",action = "update",
                options = list(title = userDescription(
                  title = renglon()$nombre_2, subtitle = renglon()$apellidos_2, type = 2,
                  image = if_else(!is.na(renglon()$foto_2) & (renglon()$foto_2 != "" & (substr(renglon()$foto_2,nchar(renglon()$foto_2)-3, nchar(renglon()$foto_2)) %in% c(".png",".jpg",".svg","jpeg", "JPEG"))), renglon()$foto_2, "https://upload.wikimedia.org/wikipedia/commons/5/50/User_icon-cp.svg")
                )))

      waiter::waiter_hide()
    })

    observeEvent(contador(),{
      shinyjs::toggle(id = "formulario", condition = contador() <= nrow(por_clasificar()))
      shinyjs::toggle(id = "nada", condition = contador() > nrow(por_clasificar()))
    })

    observeEvent(input$borrar,{
      glue::glue("UPDATE {tbl_combinaciones} set comparada = '0'") %>% DBI::dbExecute(pool,.)
      DBI::dbRemoveTable(pool, "mar_comparaciones")
      DBI::dbExecute(pool, "CREATE TABLE mar_comparaciones (
  id_comparacion INT AUTO_INCREMENT PRIMARY KEY,
  id_combinacion INT,
  id_actor_1 INT,
  id_actor_2 INT,
  id_actor_mas_influyente INT,
  relacion VARCHAR(20),
  creado DATETIME
);" )

      if(usuario != "stecnico"){
        bd$combinaciones <- tbl(pool, tbl_combinaciones) %>% left_join(bd$usuarios %>% select(usuario, id_usuario)) %>%
          filter(usuario == !!usuario, comparada <= 0) %>% collect()
      } else{
        bd$combinaciones <- tbl(pool, tbl_combinaciones) %>% left_join(bd$usuarios %>% select(usuario, id_usuario)) %>%
          filter(comparada <= 0) %>% collect()
      }


      contador(1)
    })
  })
}

## To be copied in the UI
# mod_comparacion_ui("comparacion_1")

## To be copied in the server
# mod_comparacion_server("comparacion_1")
