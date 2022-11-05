resta_abs <- function(x) abs(x[[1]]-x[[2]])

estimar_influencia <- function(comparativo, actores_tb){
  # Modelo
  model <- bpcs::bpc(comparativo,
                     player0 = 'id_actor_1',
                     player1 = 'id_actor_2',
                     player0_score = 'id_actor_1_mas_influyente',
                     player1_score = 'id_actor_2_mas_influyente',
                     model_type = 'davidson',
                     solve_ties = 'none',
                     priors = list(prior_lambda_std=2.0), # making a more informative prior to improve convergence
                     iter = 3000)
  # Muestreo de la distribución posterior
  posterior_draws <- bpcs::get_sample_posterior(model) %>%
    as_tibble() %>%
    tidyr::pivot_longer(everything())
  # Resumen
  sum_pd <- posterior_draws %>% group_by(name) %>%
    summarise(influencia=median(value)) %>%
    mutate(name=readr::parse_number(name)) %>%
    inner_join(actores_tb, by = c("name"="id_actor")) %>%
    mutate(nombre_completo=paste(nombre, apellido_paterno, apellido_materno))
  return(sum_pd)
}

reordenar <- function(usuarios_id){
  usuarios_id %>% map(~{
    reacomodo <- combinaciones %>% filter(id_usuario == !!.x, comparada == 0)
    calif <- combinaciones %>% filter(id_usuario == !!.x, comparada == 1)

    if(nrow(calif) >0){
      # Nuevo orden de combinaciones faltantes de personajes ya califica --------

      nvo <- nvoOrden_combFalt_yaCalif(combinaciones, comparaciones, actores, .x)

      # Combinaciones de actores faltantes con más y menos influyentes ----------

      extremos <- nvo[[2]] %>% filter(influencia %in% range(influencia)) %>% pull(name)
      reacomodo_2 <- reacomodo %>% anti_join(nvo[[1]], by = "id_combinacion") %>%
        filter((id_actor_1 %in% extremos) | (id_actor_2 %in% extremos))


      # Resto de combinaciones --------------------------------------------------

      reacomodo_3 <- reacomodo %>% anti_join(nvo[[1]], by = "id_combinacion") %>% anti_join(
        reacomodo_2, by = "id_combinacion"
      )


      # Final -------------------------------------------------------------------

      orden_final <- bind_rows(reacomodo_2, nvo[[1]], reacomodo_3)

      # Subir nuevas combinaciones ----------------------------------------------

      orden_final %>% select(-id_combinacion) %>% mutate(creado = now(tzone = "America/Mexico_City"),
                                                         activo = 1) %>%
        DBI::dbAppendTable(pool, value = ., tbl_combinaciones)

      # Desactivar combinaciones pasadas ----------------------------------------

      desactivar <- orden_final$id_combinacion %>% paste(collapse = ", ")
      glue::glue("UPDATE {tbl_combinaciones} set activo = 0 WHERE id_combinacion in ({desactivar})") %>% DBI::dbExecute(pool,.)



    } else{
      orden_final <- NULL
    }
    return(orden_final)
  })
}

nvoOrden_combFalt_yaCalif <- function(comb, comp, act, usu){
  comb_select <- comb %>%
    filter(comparada == 1, id_usuario == !!usu)

  est <- estimar_influencia(comparativo = comp %>%
                              semi_join(comb_select,
                                        by = "id_combinacion"),
                            actores_tb = act)
  combs <- combn(est$name, 2) %>% t() %>% as_tibble %>%
    anti_join(comb_select, by = c("V1" = "id_actor_1",
                                  "V2" = "id_actor_2")) %>%
    anti_join(comb_select, by = c("V2" = "id_actor_1",
                                  "V1" = "id_actor_2")) %>%
    left_join(comb %>% filter(id_usuario == !!usu) %>%
                select(id_actor_1, id_actor_2, id_combinacion), by = c("V1" = "id_actor_1",
                                                                       "V2" = "id_actor_2")) %>%
    left_join(comb %>% filter(id_usuario == !!usu) %>%
                select(id_actor_1, id_actor_2, id_combinacion), by = c("V2" = "id_actor_1",
                                                                       "V1" = "id_actor_2")) %>%
    mutate(id_combinacion = if_else(is.na(id_combinacion.x), id_combinacion.y, id_combinacion.x)) %>%
    select(-id_combinacion.x, -id_combinacion.y) %>%
    as.matrix()

  aux <- NULL
  for(i in seq_len(nrow(combs))){
    aux <- aux %>% append(
      resta_abs(
        est$influencia[est$name %in% combs[i,1:2]]
      ))
  }

  nuevo_orden <- combs %>% as_tibble() %>% mutate(dif = aux) %>% arrange(dif) %>%
    inner_join(combinaciones) %>% select(-V1, -V2,-dif)

  return(list(nuevo_orden, est))
}
