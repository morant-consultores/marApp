## code to prepare `aws` dataset goes here
library(googlesheets4)
library(janitor)
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)
library(purrr)
# pool --------------------------------------------------------------------

pool <- pool::dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "db_gp",
  host = 'mysql.cduzrgqgkfht.us-east-2.rds.amazonaws.com',
  username = 'root',
  password = '9Blw33caY',
  port = 3306
)

# googlehseet -------------------------------------------------------------

actores <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KEStCRTHKbHH_zNrl8XAlhPFxN4kXch07uh5wE4IpXA/edit#gid=305471201") %>% clean_names()
1
distrito <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/14IWajVLgeeR4KmLYo6_sZKwIvMXtP13JM_SZ_v-jWAY/edit#gid=0") %>% clean_names()

# usuarios ----------------------------------------------------------------
# DBI::dbRemoveTable(pool, "mar_usuarios")
DBI::dbExecute(pool, "CREATE TABLE mar_usuarios (
  id_usuario INT AUTO_INCREMENT PRIMARY KEY,
  usuario VARCHAR(100),
  nombre VARCHAR(100),
  contrasena VARCHAR(100),
  distrito INT,
  creado DATETIME,
  activo TINYINT
);" )

# set.seed(1991)
# tibble(id_usuario = 0, usuario = "stecnico", contrasena = 1, nombre = "Soporte técnico", distrito = 0, activo = 1, creado = now()) %>%
#   bind_rows(distrito %>% mutate(telefono = as.character(.data$telefono)) %>%
#               transmute(usuario = str_replace_all(telefono," ",""),
#                         nombre = titular,
#                         contrasena = round(runif(nrow(.),1000,9999)),
#                         distrito = readr::parse_number(distrito),
#                         creado = now(tz = "America/Mexico_City"),
#                         activo = 1
#               )) %>% DBI::dbWriteTable(pool, "mar_usuarios", ., append = T)

tbl(pool, tbl_usuarios)
# actores -----------------------------------------------------------------
# googlesheets4::gs4_auth(email = "emiliomorones@gmail.com")
# DBI::dbRemoveTable(pool, "mar_actores")
DBI::dbExecute(pool, "CREATE TABLE mar_actores (
  id_actor INT AUTO_INCREMENT PRIMARY KEY,
  id_usuario INT,
  nombre VARCHAR(50),
  apellido_paterno VARCHAR(50),
  apellido_materno VARCHAR(50),
  foto VARCHAR(200),
  fecha_de_nacimiento DATE,
  telefono INT,
  cual_es_el_tipo_de_influencia VARCHAR(100),
  zona_de_influencia VARCHAR(100),
  municipio VARCHAR(100),
  partido VARCHAR(50),
  creado DATETIME,
  activo TINYINT
);" )


usu <- tbl(pool, "mar_usuarios") %>% select(id_usuario, nombre) %>% collect()
actores %>%
  select(-foto) %>%
  mutate(telefono = actores$telefono %>% as.character()) %>%
  filter(cual_es_tu_nombre != "DPP") %>%
  rename(partido = esta_asociada_a_un_partido_politico,
         foto = liga_web_de_la_foto) %>%
  left_join(usu, by = c("cual_es_tu_nombre" = "nombre")) %>%
  select(-cual_es_tu_nombre) %>%
  rename(creado = marca_temporal) %>%
  mutate(activo = 1) %>%
  DBI::dbWriteTable(conn = pool, name = "mar_actores", append = T)

#prueba
# tibble(a = 1:20) %>% mutate(id_usuario = 1,
#                             nombre = glue::glue("Nombre {a}"),
#                             apellido_paterno = glue::glue("Apellido paterno {a}"),
#                             apellido_materno = glue::glue("Apellido materno {a}"),
#                             activo = 1) %>%
#   select(-a) %>%
#   DBI::dbWriteTable(conn = pool, name = "mar_actores", append = T)

# combinaciones -----------------------------------------------------------
# DBI::dbRemoveTable(pool, "mar_combinaciones")
DBI::dbExecute(pool, "CREATE TABLE mar_combinaciones (
  id_combinacion INT AUTO_INCREMENT PRIMARY KEY,
  id_usuario INT,
  id_actor_1 INT,
  id_actor_2 INT,
  comparada TINYINT,
  creado DATETIME
);" )


act <- tbl(pool, "mar_actores") %>% collect()
act %>% sample_frac() %>% group_by(id_usuario) %>% filter(n()>1) %>% ungroup %>%
  split(.$id_usuario) %>% imap(~{
  combn(.x$id_actor,2) %>% t() %>% as_tibble %>% set_names(c("id_actor_1", "id_actor_2")) %>%
      mutate(id_usuario = .y, comparada = 0, creado = now(tz = "America/Mexico_City")) %>%
    sample_frac() %>% DBI::dbWriteTable(pool, "mar_combinaciones",., append = T)
})


tbl(pool, "mar_combinaciones")
# comparaciones -----------------------------------------------------------
# DBI::dbRemoveTable(pool, "mar_comparaciones")
DBI::dbExecute(pool, "CREATE TABLE mar_comparaciones (
  id_comparacion INT AUTO_INCREMENT PRIMARY KEY,
  id_combinacion INT,
  id_actor_1 INT,
  id_actor_2 INT,
  id_actor_1_mas_influyente INT,
  id_actor_2_mas_influyente INT,
  relacion VARCHAR(20),
  creado DATETIME
);" )
