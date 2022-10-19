## code to prepare `municipios` dataset goes here
library(dplyr)
municipios <- readr::read_csv("data-raw/catálogo_mpos_dttos.csv")
municipios <- municipios %>% arrange(nombre_municipio) %>% distinct(nombre_municipio) %>% pull(1)
usethis::use_data(municipios, overwrite = TRUE)
