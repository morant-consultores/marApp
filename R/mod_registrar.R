#' registrar UI Function
#'#' reg#' registrar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_registrar_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        textInput(ns("nombre"), "Nombre(s)")
      ),
      col_4(
        textInput(ns("apellido_p"), "Apellido paterno")
      ),
      col_4(
        textInput(ns("apellido_m"), "Apellido materno")
      )
    ),
    fluidRow(
      col_12(
        textInput(ns("foto"), label = "Link a fotografía")
      )
    ),
    fluidRow(
      col_4(
        dateInput(ns("nacimiento"), label = "Fecha de nacimiento")
      ),
      col_4(
        textInput(ns("telefono"), "Teléfono")
      ),
      col_4(
        selectInput(ns("influencia"), "Tipo de influencia",
                    choices = c("Seleccione una opción" = "", c("Política", "Religiosa", "Económica", "Social")))
      )
    ),
    fluidRow(
      col_6(
        shinyWidgets::radioGroupButtons(ns("zona"),
                                        checkIcon = list(
                                          yes = tags$i(class = "fa fa-circle",
                                                       style = "color: steelblue"),
                                          no = tags$i(class = "fa fa-circle-o",
                                                      style = "color: steelblue")),
                                        label = "Zona de influencia", choices = c("Estatal", "Municipal"),
                                        selected = character(0))
      ),
      col_6(
        shinyjs::hidden(selectInput(ns("municipio"), label = "Municipio",multiple = T,
                                    choices = c("Elija un municipio" = "", municipios)))
      )
    ),
    fluidRow(
      col_6(
        checkboxInput(ns("asociado"), label = "Asociado a un partido político",value = F)
      ),
      col_6(
        shinyjs::hidden(selectInput(ns("partido"), label = "Partido",
                                    choices = c("Elija un partido" = "",
                                                c("MORENA","PVEM", "PRI", "PAN", "PRD", "PT", "MC", "PPCH", "RSP","Chiapas Unido", "Podemos Mover a Chiapas"))))
      )
    ),
    fluidRow(
      actionButton(ns("guardar"), "Guardar")
    )
  )
}

#' registrar Server Functions
#'
#' @noRd
mod_registrar_server <- function(id, bd, usuario){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      if((length(input$zona)>0)){
        shinyjs::toggle(id = "municipio", condition = (input$zona == "Municipal"))
      } else{
        shinyjs::hide(id = "municipio")
      }

    })

    observe({
      shinyjs::toggle(id = "partido", condition = input$asociado)
    })

    observeEvent(input$guardar,{
      mandatory <- c("nombre", "apellido_p", "apellido_m","telefono", "zona","influencia") %>%
        purrr::map(~(!is.null(input[[.x]])) &&
                     ((input[[.x]] %>%  stringr::str_squish()) != "") &&
                     (as.character(input[[.x]]) != 0)) %>% all()

      if(mandatory){
        withProgress(message = 'Espere un momento', value = 0, {
          shinyjs::disable(id = "guardar")

          c("nombre", "apellido_p", "apellido_m","foto","nacimiento","telefono", "influencia", "municipio","asociado", "partido") %>%
            purrr::walk(~shinyjs::reset(id = .x))

          shinyWidgets::updateRadioGroupButtons(inputId = "zona", selected = character(0))

          shiny::incProgress(amount = .4, detail = "Registrando")

          usuario <- bd$usuarios %>% select(usuario, id_usuario) %>% filter(usuario == !!usuario) %>% pull(id_usuario)

          tibble(
            id_usuario = !!usuario,
            nombre = input$nombre,
            apellido_paterno = input$apellido_p,
            apellido_materno = input$apellido_m,
            foto = input$foto,
            fecha_de_nacimiento = lubridate::ymd(input$nacimiento),
            telefono = input$telefono,
            cual_es_el_tipo_de_influencia = input$influencia,
            zona_de_influencia = input$zona,
            municipio = if_else(input$zona == "Municipal", paste(input$municipio, collapse = ", "), NA_character_),
            partido = if_else(input$asociado, input$partido, NA_character_),
            creado = lubridate::now(tz = "America/Mexico_City"),
            activo = 1
          ) %>% DBI::dbAppendTable(pool, tbl_actores, value = .)

          a <- tbl(pool, tbl_actores) %>%
            filter(id_usuario == !!usuario) %>% collect() %>% sample_frac()

          nuevo <- a %>% filter(id_actor == max(id_actor)) %>% pull(id_actor)

          shiny::incProgress(amount = .5, detail = "Actualizando datos")

          combn(a$id_actor,2) %>% t() %>% as_tibble %>% purrr::set_names(c("id_actor_1", "id_actor_2")) %>%
            filter(if_any(everything(), ~.x == !!nuevo)) %>%
            mutate(id_usuario = !!usuario, comparada = 0, creado = lubridate::now(tz = "America/Mexico_City")) %>%
            sample_frac() %>% DBI::dbWriteTable(pool, "mar_combinaciones",., append = T)

          bd$combinaciones <- tbl(pool, tbl_combinaciones) %>%
            left_join(bd$usuarios %>% select(usuario, id_usuario)) %>%
            filter(id_usuario == !!usuario, comparada == 0) %>% collect()

          bd$actores <- tbl(pool, tbl_actores) %>% filter(activo == 1) %>% collect()

          gargoyle::trigger("actualizar_combinaciones")

          shinyjs::enable(id = "guardar")
        })
      } else{
        shinyalert::shinyalert(title = "¡Alerta!", text = "Llene todos los campos")
      }


    })

  })
}

## To be copied in the UI
# mod_registrar_ui("registrar_1")

## To be copied in the server
# mod_registrar_server("registrar_1")
