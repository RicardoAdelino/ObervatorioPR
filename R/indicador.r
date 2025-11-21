#' Estimador de vulnerabilidade
#'
#' Esta função usa os registros de ocorrencia para estimar a expectativas de distribuição espacial das espécies em uma célula espacial de área pré-definida.
#' A razão entre o numero de células observadas e o número de celulas esperadas determina a área de ocupação das espécies.
#'    
#' @param dados `data frame` contendo os registros de ocorrência, nome das espécies e grupos de interesse
#' @param grupo `string`determina o grupo de interesse para analise. Padrão não considera grupos específicos `group = "all"`  
#' @param grid `numerico` determina a área da célula em quilometros quadrados
#' @param geom `string` determina o polígono de interesse no qual a metrica vai ocorrer. Padrão ajustado para o munícípios do estado do Paraná obtidos da base de dados do IBGE `geom = "IBGE`
#' Podem ser também: "OpenStreeMap","Litoral","Continente","IBGE","EcoRegions","IBGE_BIOME","MMA","IAT"

#' @return `data frame` contendo o valor do indicador estimado para cada poligono existente no paramentro `geom`
#' 
#' @examples 

ivm <- function(dados = NULL, grupo = 'all', grid = 50, geom = "IBGE"){
    # Passo 1: Cria células dentro da geometria da área de interesse
    # Por padrão a geometria utilizada é o limite admnistrativo do Paraná sem considerar áreas marinhas
    pr_ <- geometrias %>%
        dplyr::filter(class == 'Continente') %>% 
        # UTM 22J (recomendado para o PR)
        sf::st_transform(32722)
    
    # Passo 2: Cria células de área específica dentro dos limites admnistrativos do estado do Paraná
    cel_ <- sf::st_make_grid(
        pr_,        
        cellsize = units::as_units(grid, "km^2"), 
        what = 'polygons',
        square = TRUE
        )  %>% 
        sf::st_as_sf() %>% 
        sf::st_intersection(
            ., 
            pr_
        )  %>% 
        tibble::rownames_to_column(
            ., 
            var = "hex_id"
        ) %>% 
        dplyr::mutate(
            cel_id = as.numeric(cel_id)
    )
    
    # Passo 3: determina as condições de fluxo
    # Condição 1: Para todas as ocorrências independentemnte de agrupamentos especificos
    if(grupo == "all"){
        cob_ <- dados %>%  
            # Agrupa por espécie
            dplyr::group_by(especies_ajustado) %>% 
            # Faz a diferença de políggonos
            sf::st_difference() %>% 
            # Separa os dados em lista de espécies
            {setNames(group_split(.), group_keys(.)[[1]])} %>% 
            # Mantém apenas espécies com mais do que dois registros de ocorrência
            purrr::keep(., ~ nrow(.x) >2)
    } else {
    # Condição 2: Para todas as ocorrências dentro de agrupamentos especificos    
        cob_ <- dados %>% 
            # Filtra a categoria de interesse
            dplyr::filter(form == grupo) %>% 
            # Agrupa por espécies
            dplyr::group_by(especies_ajustado) %>%
            #Faz a diferença de polígonos 
            sf::st_difference() %>%
            # Separa os dados em lista de espécies 
            {setNames(group_split(.), group_keys(.)[[1]])} %>%
            # Mantém apenas espécies com mais do que dois registros de ocorrência
            purrr::keep(., ~ nrow(.x) >2)
        }

    # Passo 4: Cria polígono para os registros de ocorrência
    print("Computando polígonos para as espécies")
    hull_list <- pbapply::pblapply(
        cob_, 
        function(x){
            x %>% 
            terra::vect() %>% 
            terra::hull(
                ., 
                type="concave_length", 
                param = .01
            ) %>% 
            sf::st_as_sf()
        }
    )

    # Passo 5: Cria células dentro do polígono estimado
    in_pol <- list()
    for(i in 1:length(cob_)){
        in_pol[[i]] <- cel_ %>% 
            dplyr::mutate(
                range_cells = sf::st_intersects(
                    .,  
                    hull_list[[i]]
                ) %>% 
                lengths()
            ) %>% 
            dplyr::mutate(
                point_cells = sf::st_intersects(
                    ., 
                    cob_[[i]]
                ) %>% 
            l   engths(),
            .after = "range_cells"
        )
    }
    names(in_pol) <- names(cob_)

    # Passo 5: Conta numero de celulas da distribuição estimada (i.e., soma das células dentro de cada polígono)
    range_ncell <- lapply(
        in_pol, 
        function(x){
            x %>% 
            dplyr::filter(range_cells == 1) %>% 
            base::nrow()
            }
        ) %>% 
        base::unlist() %>% 
        tibble::enframe(
            name = "especie", 
            value = "distr_esperada"
        )

    # Passo 6: Conta numero de celulas obsevadas
    obs_ncell <- lapply(
        in_pol, 
        function(x){
            x %>% 
            dplyr::filter(point_cells == 1) %>% 
            base::nrow()
            }
        ) %>% 
        base::unlist() %>% 
        tibble::enframe(
            name = "especie", 
            value = "distr_observada"
        )

    # Passo 7: Monta data frame dos resultados
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
            dplyr::desc(AOO_km2)
    )

    #if(rank == TRUE){
    #    return(cbtra_spacial)
    #}else {
    #   return(
    #    "Estimando medidas dentro de cada grupo de polígono"
    #   )
    #}

    # Passo 8: relação entre observado e esperado para o municipio
    mun_ <- geometrias %>% 
        dplyr::filter(class == geom) %>% 
        # UTM 22J (recomendado para o PR)
        sf::st_transform(32722)
    
    # Cria hexagonos ao longo do poligono do Parana
    cel_mun <- sf::st_make_grid(
        mun_,
        cellsize = units::as_units(grid, "km^2"), 
        what = 'polygons',
        square = TRUE
        )  %>% 
        sf::st_as_sf() %>% 
        sf::st_intersection(
            ., 
            mun_
        )  %>% 
        tibble::rownames_to_column(
            ., 
            var = "cel_id"
        ) %>% 
        dplyr::mutate(
            cel_id = as.numeric(cel_id)
    )

    # Passo 9: Intersecção das celulas do area estimada com municio
    in_pol_mun <- list()
        for(i in 1:length(cob_)){
            in_pol_mun[[i]] <- cel_mun %>% 
                dplyr::mutate(
                    range_cells = sf::st_intersects(
                        .,
                        hull_list[[i]]
                    ) %>% 
                    lengths()
                ) %>% 
                dplyr::mutate(
                    point_cells = sf::st_intersects(
                        .,
                        cob_[[i]]
                    ) %>% 
                    lengths(),
                    .after = "range_cells"
        )
    }
    names(in_pol_mun) <- names(cob_)

    # Passo 10: Conta numero de celulas da distribuição estimada
    print("Computando polígonos das espécies nos municipios")
    range_ncell_mun <- pbapply::pblapply(
        in_pol_mun, 
        function(x){
            x %>% 
            group_by(nome) %>% #<-- ISSO AQUI PODE GERAR PROBLEMA SE O POLIGONO FOR DE FONTE EXTERNA
            summarise(
                distr_esperada_mun = sum(range_cells != 0)
            ) %>% 
            sf::st_drop_geometry()
            }
        ) %>% 
    dplyr::bind_rows(., .id = 'especie') 

    # Paso 11: Conta numero de celulas obsevadas
    obs_ncell_mun <- pbapply::pblapply(
        in_pol_mun, 
        function(x){
            x %>% 
            group_by(nome) %>% 
            summarise(
                distr_observada_mun = sum(point_cells != 0)
            ) %>% 
            sf::st_drop_geometry()
            }
        ) %>% 
    dplyr::bind_rows(., .id = 'especie') 

    # Passo 12: Combina dados
    print("Computando indicador")
    mun_xpobs <- dplyr::left_join(range_ncell_mun, obs_ncell_mun) %>% 
        dplyr::mutate(
            Cobertura = distr_observada_mun/distr_esperada_mun, 
            Cobertura = replace_na(Cobertura, 0)
    ) 

    ind_ <- mun_xpobs %>% 
        dplyr::left_join(
            ., 
            cbtra_spacial %>% 
            dplyr::select(especie, Peso), 
            by = "especie"
    ) 
    #range_std <- function(x){(x-min(x))/(max(x)-min(x))}
    ssi <- ind_ %>% 
        filter(Cobertura != 0) %>% 
        dplyr::group_by(
            nome
        ) %>% 
        # Cobertura = Ncell_observado / Ncell esperado
        dplyr::mutate(Distr_indicador = Cobertura * Peso) %>% 
        summarise(
            Ind_vulnerabilidade = sum(Cobertura)/n_distinct(especie)
            #Mun_resp = sum(SSI_mun)/sum(Peso)
    ) 
    return(ssi)
}
