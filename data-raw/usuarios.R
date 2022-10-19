## code to prepare `usuarios` dataset goes here
library(shinymanager)

credentials <- #tibble(id_usuario = 1, user = "stecnico", password = "1", nombre = "Soporte técnico", admin = T) %>%
  # bind_rows(
    tbl(pool, tbl_usuarios) %>% filter(activo == 1) %>% collect() %>%
      rename(user = usuario, password = contrasena) %>% mutate(admin = F) %>% select(-activo)
  # )

shinymanager::create_db(
  credentials_data = credentials,
  sqlite_path = "inst/app/data/credenciales.sqlite", # will be created
  passphrase = "morantconsultores"
)
