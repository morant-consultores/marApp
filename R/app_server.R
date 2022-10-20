#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr dbplyr
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  gargoyle::init("actualizar_combinaciones")

  output$usuario <- renderText({
    res_auth$nombre
  })

  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(db = app_sys("app/data/credenciales.sqlite"),
                                                        passphrase = "morantconsultores")
  )

  bd <- reactiveValues(
    actores = tbl(pool, tbl_actores) %>% filter(activo == 1) %>% collect(),
    combinaciones = tbl(pool, tbl_combinaciones),
    usuarios = tbl(pool, tbl_usuarios)
  )

  observeEvent(c(res_auth$user),{
    if(res_auth$user != "stecnico"){
      bd$combinaciones <- bd$combinaciones %>% left_join(bd$usuarios %>% select(usuario, id_usuario)) %>%
        filter(usuario == !!res_auth$user)
    } else{
      bd$combinaciones <- bd$combinaciones %>% left_join(bd$usuarios %>% select(usuario, id_usuario))
    }
    gargoyle::trigger("actualizar_combinaciones")
  })




  mod_comparacion_server("comparacion_1", bd = bd, usuario = res_auth$user)
  mod_registrar_server("registrar_1", bd = bd, usuario = res_auth$user)
}
