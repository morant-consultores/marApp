#' modelo
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

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


# Graficar red
graficar_red <- function(comparaciones,
                         resumen,
                         base_actores){
  edges <- comparaciones %>%
    filter(relacion!= "No sé") %>%
    mutate(relacion=factor(x = relacion,
                           levels = c("Muy alejados", "Alejados", "Neutral",
                                      "Algo cercanos", "Muy cercanos")),
           width=as.numeric(relacion)-3,
           color=c("blue", "black", "grey", "orange","red")[width+3]) %>%
    rename(from=id_actor_1, to=id_actor_2)
  # Se queda con aquellos en los que hay relaciones
  base_actores <- base_actores %>%
    inner_join(resumen %>% select(id_actor=name, influencia, nombre_completo))

  #
  nodes <-   base_actores %>%
    transmute(id=id_actor,
              shape=case_when(cual_es_el_tipo_de_influencia=="Política"~"circle",
                              cual_es_el_tipo_de_influencia=="Religiosa"~"star",
                              cual_es_el_tipo_de_influencia=="Económica"~"diamond",
                              cual_es_el_tipo_de_influencia=="Social"~"square",
              ),
              color=case_when(partido=="MORENA"~"brown",
                              partido=="PVEM"~"green",
                              partido=="Chiapas Unido"~"turquoise",
                              partido=="Podemos Mover a Chiapas"~"purple"),
              nombre_completo=nombre_completo,
              value=influencia,
              title=nombre_completo)

  red <- visNetwork::visNetwork(edges = edges,
                                nodes =  nodes) %>%
    visNetwork::visOptions(highlightNearest = TRUE,
                           selectedBy = "nombre_completo")

  return(red)

}

# Graficar influencia
graficar_influencia <- function(resumen){
  resumen_por <- resumen %>%
    arrange(desc(influencia)) %>%
    mutate(orden=row_number(),
           grupo=ceiling(orden/10)) %>%
    split(f = .$grupo)

  graficas <- resumen_por %>%
    purrr::map(~{
      .x %>%
        ggplot2::ggplot(ggplot2::aes(x=reorder(stringr::str_trunc(nombre_completo,25),
                             influencia),
                   y=100*pnorm(influencia)))+
        ggplot2::geom_bar(ggplot2::aes(fill=100*pnorm(influencia)),
                 stat="identity") +
        ggfittext::geom_bar_text(contrast=T,
                                 ggplot2::aes(label=round(100*pnorm(influencia))))+
        ggplot2::coord_flip() +
        ggplot2::labs(y="Influencia", x="",
             title = "Influencia de los actores relevantes",
             subtitle = glue::glue("Del {min(.x$orden)} al {max(.x$orden)}"))+
        ggplot2::scale_fill_gradient2(midpoint = 50,
                             name="Influencia",
                             limits=c(0,100))+
        ggplot2::theme_minimal() +
        ggplot2::guides(fill="none")
    })
  return(graficas)
}
