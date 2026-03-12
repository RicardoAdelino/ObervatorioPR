#' Estimador de espécies prioritarias
#'
#' Esta função usa os registros de ocorrencia para estimar a expectativas de distribuição espacial das espécies em uma célula espacial de área pré-definida.
#' A razão entre o numero de células observadas e o número de celulas esperadas determina a área de ocupação das espécies.
#'    
#' @param area `numerico` determina o tamnaho da área da célula espacial de referencia em kilometros quadrados
#' @param filtro numerico`determina o número minimo de ocorrências por espécies para considerar na analise. Padrão filto = 3 
#' 
#' @return data frame contendo o nome das espécies e as razão entre a distribuição observada e esperada
#' 
#' @examples 


# Linha temporaria
#load("data/db_splist.rda")
#load("data/db_oco.rda")
#load("data/geometrias.rda")

opree_spatCover <- function(area, filtro = 2){
    # Filtra lista inicial
    sp_ <- db_splist %>% 
        dplyr::filter(nivel_classificacao == 'especie' & manter == "SIM") %>% 
        dplyr::pull(especie_ajustado) 

    # Aplica filtro nas ocorrencias
    pr_obs <- db_oco %>%     
        sf::st_as_sf(
            ., 
            coords = c("long_dec","lat_dec"), 
            remove = FALSE,
            crs = 4326
        ) %>% 
        sf::st_transform(32722) %>% 
    dplyr::filter(species_ajustado %in% sp_) 

    # ⤷ Carrega poligono do Paraná
    pr_ <- geometrias %>%
        filter(class == 'OpenStreeMap') %>% 
        # UTM 22J (recomendado para o PR)
    sf::st_transform(32722)

    # ⤷ Carrega poligono do Paraná
    mun_ <- geometrias %>% 
        filter(class == 'IBGE') %>% 
         # UTM 22J (recomendado para o PR)
    sf::st_transform(32722)

    # ⤷ Cria hexagonos ao longo do poligono do Parana
    grid_ <- sf::st_make_grid(
        # Sf object
        pr_,
        # control grid area (https://github.com/r-spatial/sf/issues/1505)
        cellsize = units::as_units(area, "km^2"), 
        # Return class
        what = 'polygons',
        # Hexagonal
        square = TRUE
        )  %>% 
        # sf object
        sf::st_as_sf() %>% 
        # crop hexagons to territorial limmits
        sf::st_intersection(
            ., 
            pr_
        )  %>% 
        # Create ID for each hexagon
        tibble::rownames_to_column(
            ., 
            var = "hex_id"
        ) %>% 
        dplyr::mutate(
            hex_id = as.numeric(hex_id)
    )

    # Redefine os dados
    data_ <- pr_obs

    cob_ <- data_ %>%  
            dplyr::group_by(species_ajustado) %>% 
            st_difference() %>% 
        {setNames(group_split(.), group_keys(.)[[1]])} %>% 
    purrr::keep(., ~ nrow(.x) > filtro)


    # Passo 2: Cria polígono nos registros de ocorrência
    # O poligono pode ser obtido varias maneiras, aqui preferi manter a abordagem de poligono concavo
    # Usado para estimar a área potencial que pode ser ocupada pela espécies
    # Esse valor representa a medida de peso dos indices municipais
    hull_list <- pbapply::pblapply(
        cob_, 
        function(x){
            x %>% 
            terra::vect() %>% 
            terra::hull(., type="concave_length", param = .01) %>% 
            sf::st_as_sf()
        }
    )

    # Cria células dentro do polígono estimado
    in_pol <- list()
    for(i in 1:length(cob_)){
        in_pol[[i]] <- grid_ %>% 
            dplyr::mutate(range_cells = sf::st_intersects(.,  hull_list[[i]]) %>% lengths(),.after = "categoria") %>% 
            dplyr::mutate(point_cells = sf::st_intersects(.,  cob_[[i]]) %>% lengths(),.after = "range_cells")
    }
    names(in_pol) <- names(cob_)

    # Passo 3: relação entre Observado e esperado para o estado
    # Conta numero de celulas da distribuição estimada
    range_ncell <- lapply(in_pol, function(x){x %>% filter(range_cells == 1) %>% nrow()}) %>% 
        unlist() %>% 
        tibble::enframe(name = "especie", value = "distr_esperada")

    # Conta numero de celulas obsevadas
    obs_ncell <- lapply(in_pol, function(x){x %>% filter(point_cells == 1) %>% nrow()}) %>% 
        unlist() %>% 
        tibble::enframe(name = "especie", value = "distr_observada")

    # Monta data frame
    cbtra_spacial <- dplyr::left_join(
        range_ncell, 
        obs_ncell, 
        by = "especie") %>% 
        dplyr::mutate(
            Peso = distr_observada/distr_esperada, 
            EOO_km2 = distr_esperada * 50,
            AOO_km2 = distr_observada * 50
            ) %>% 
        dplyr::arrange(
            dplyr::desc(Peso)   
    )
    return(cbtra_spacial)
}


