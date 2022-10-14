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
    actionBttn(inputId = ns("borrar"), "Reiniciar todo"),
    shinyjs::hidden(
      div(id = ns("nada"),
          bs4Quote("Nada por clasificar", color = "info")
      )),
    shinyjs::hidden(
      div(id = ns("formulario"),
          tagList(
            bs4Quote("Elija al actor más influyente", color = "info"),
            # actionButton(ns("update_box"), "Update"),
            fluidRow(
              userBox(
                id = ns("actor_1"),
                title = userDescription(
                  title = "",
                  subtitle = "",
                  type = 1,
                  image = "https://upload.wikimedia.org/wikipedia/commons/5/50/User_icon-cp.svg",
                ),
                status = "info",
                gradient = TRUE,
                background = "gray",
                boxToolSize = "xl",
                footer = prettyCheckbox(
                  inputId = ns("ele_actor_1"),
                  label = "Más influyente",
                  value = F,
                  status = "info",
                  icon = icon("thumbs-up"),
                  plain = TRUE,
                  outline = TRUE
                )
              ),
              userBox(
                id = ns("actor_2"),
                title = userDescription(
                  title = "",
                  subtitle = "",
                  type = 1,
                  image = "https://upload.wikimedia.org/wikipedia/commons/5/50/User_icon-cp.svg",
                ),
                status = "info",
                gradient = TRUE,
                background = "gray",
                boxToolSize = "xl",
                footer = prettyCheckbox(
                  inputId = ns("ele_actor_2"),
                  label = "Más influyente",
                  value = F,
                  status = "info",
                  icon = icon("thumbs-up"),
                  plain = TRUE,
                  outline = TRUE
                )
              )
            ),
            hr(),
            bs4Quote("¿Cómo describe su relación?", color = "success"),
            fluidRow(
              radioGroupButtons(
                inputId = ns("relacion"),
                label = "",
                choices = c("Negativa", "Neutral", "Positiva"),
                justified = TRUE,
                selected = "",status = "success",
                width = "100%",
                checkIcon = list(
                  yes = icon("ok",
                             lib = "glyphicon"))
              )
            ),
            fluidRow(
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
}

#' comparacion Server Functions
#'
#' @noRd
mod_comparacion_server <- function(id, bd, usuario){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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

    observeEvent(c(input$ele_actor_1, input$ele_actor_2),{
      if(input$ele_actor_1 + input$ele_actor_2 > 1){
        shinyalert::shinyalert("¡Atención!", "Sólo puede seleccionar un actor", type = "warning",
                               closeOnEsc = F,closeOnClickOutside = F,
                               callbackR = function(x){
                                 updatePrettyCheckbox(inputId = "ele_actor_1",value = F)
                                 updatePrettyCheckbox(inputId = "ele_actor_2",value = F)
                               })
      }
    })


    contador <- reactiveVal(1)

    por_clasificar <- eventReactive(bd$combinaciones,{
      aux <- bd$combinaciones %>%
        # slice(contador()) %>%
        select(id_combinacion, id_actor_1, id_actor_2) %>%
        left_join(bd$actores %>%
                    transmute(id_actor_1 = id_actor,
                              nombre_1 = nombre,
                              apellidos_1 = glue::glue("{apellido_paterno} {apellido_materno}"),
                              foto_1 = foto)) %>%
        left_join(bd$actores %>%
                    transmute(id_actor_2 = id_actor, nombre_2 = nombre,
                              apellidos_2 = glue::glue("{apellido_paterno} {apellido_materno}"),
                              foto_2 = foto))
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
          mutate(id_actor_mas_influyente = if_else(input$ele_actor_1, renglon()$id_actor_1, renglon()$id_actor_2),
                 relacion = input$relacion, creado = lubridate::now(tz = "America/Mexico_City")) %>%
          DBI::dbWriteTable(pool, tbl_comparaciones, value = .,append = T)

        #quitar de la cola
        DBI::dbExecute(pool, glue::glue("UPDATE {tbl_combinaciones} set comparada = '1' WHERE id_combinacion = '{renglon()$id_combinacion}'"))

        updatePrettyCheckbox(inputId = "ele_actor_1",value = F)
        updatePrettyCheckbox(inputId = "ele_actor_2",value = F)
        updateRadioGroupButtons(inputId = "relacion",selected = character(0))

        contador(contador()+ 1)
      }

    })

    observeEvent(renglon(),{
      # tmp_1 <- tempfile(fileext = ".jpeg")
      # googledrive::drive_download(file = renglon()$foto_1, tmp_1 )
      # tmp_2 <- tempfile(fileext = ".jpeg")
      # googledrive::drive_download(file = renglon()$foto_2, tmp_2)

      updateBox(id = "actor_1",action = "update",
                options = list(title = userDescription(
                  title = renglon()$nombre_1, subtitle = renglon()$apellidos_1,
                  image = "https://upload.wikimedia.org/wikipedia/commons/5/50/User_icon-cp.svg"
                )))

      updateBox(id = "actor_2",action = "update",
                options = list(title = userDescription(
                  title = renglon()$nombre_2, subtitle = renglon()$apellidos_2,
                  image = "https://upload.wikimedia.org/wikipedia/commons/5/50/User_icon-cp.svg"
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

      bd$combinaciones <- tbl(pool, tbl_combinaciones) %>% filter(id_usuario == !!usuario, comparada == 0) %>% collect()
      contador(1)
    })
  })
}

## To be copied in the UI
# mod_comparacion_ui("comparacion_1")

## To be copied in the server
# mod_comparacion_server("comparacion_1")
