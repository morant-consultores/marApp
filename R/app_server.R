#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr dbplyr
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  usuario <- 1
  bd <- reactiveValues(usuarios = tbl(pool, tbl_usuarios) %>% filter(activo == 1) %>% collect(),
                 actores = tbl(pool, tbl_actores) %>% filter(activo == 1) %>% collect(),
                 combinaciones = tbl(pool, tbl_combinaciones) %>% filter(id_usuario == !!usuario, comparada == 0) %>% collect()
                 # comparaciones = tbl(pool, tbl_comparaciones) %>% collect()
                 )

  mod_comparacion_server("comparacion_1", bd = bd, usuario = usuario)
}
