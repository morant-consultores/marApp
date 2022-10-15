#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr dbplyr
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(db = app_sys("app/data/credenciales.sqlite"),
                                                        passphrase = "morantconsultores")
  )

  bd <- reactiveValues(
    actores = tbl(pool, tbl_actores) %>% filter(activo == 1) %>% collect(),
    combinaciones = tbl(pool, tbl_combinaciones)
  )

  observeEvent(res_auth$user,{
    bd$combinaciones <- bd$combinaciones %>% filter(id_usuario == !!res_auth$user, comparada == 0) %>% collect()
  })




  mod_comparacion_server("comparacion_1", bd = bd, usuario = res_auth$user)
}
