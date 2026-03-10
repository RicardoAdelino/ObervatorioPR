#' Classes de vulnerabilidade
#'
#' Visualização do indicador e da classificação dos polígonos
#'    
#' @param dados `data frame` contendo os registros de ocorrência, nome das espécies e grupos de interesse
#' @param grid `numerico` determina a área da célula em quilometros quadrados
#' @param geom `string` determina o polígono de interesse no qual a metrica vai ocorrer. Padrão ajustado para o munícípios do estado do Paraná obtidos da base de dados do IBGE `geom = "IBGE`
#' Podem ser também: "OpenStreeMap","Litoral","Continente","IBGE","EcoRegions","IBGE_BIOME","MMA","IAT"
#' @param plot `boleano` determina se retorna imagem na forma de mapa. 
#' 
#' @return Se plot = TRUE retorna mapa, se plot = FALSE retorna lista de `data frame` contendo a proporção das classes classificadas e o raqueamento dos polígonos

#' @examples 


opr_classes <- function(dados = NULL, grid = 50, geom = "IBGE", plot = c(TRUE,FALSE)){
    if(is.null(dados)){
        return("Planilha de indicador ausente! \n Execute a função ivm para estimar os indicadores")
        stop()
    } else {
        # Carrega dados da geometria com os mesmos parametros utilizado para os indicadores
        pr_ <- geometrias %>%
            dplyr::filter(class == 'Continente') %>% 
            # UTM 22J (recomendado para o PR)
            sf::st_transform(32722)
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

        completo_ <- suppressWarnings(
            left_join(mun_,dados) %>% 
                dplyr::mutate(
                    quantile_class = cut(
                        Ind_vulnerabilidade,
                        breaks = quantile(Ind_vulnerabilidade, probs = seq(0, 1, 0.25), na.rm = TRUE) ,
                        labels = c(
                            "Atenção",
                            "Preocupante", 
                            "Vulneravel", 
                            "Muito Vulneravel"),
                        include.lowest = TRUE,
                        right = TRUE),
                    quantile_class = as.character(quantile_class),
                    quantile_class = tidyr::replace_na(quantile_class, "Deficiente de dados") %>% as.factor(), 
                    quantile_class =  fct_relevel(
                        quantile_class, 
                        "Deficiente de dados",
                        "Pouco preocupante", 
                        "Atenção", 
                        "Preocupante", 
                        "Vulneravel", 
                        "Muito Vulneravel"
                        ),
                    color_code = as.factor(
                        case_when(
                            quantile_class == "Pouco preocupante" ~ "#4575B4",
                            quantile_class == "Atenção" ~ "#91BFDB", 
                            quantile_class == "Preocupante" ~ "#FEE090", 
                            quantile_class == "Vulneravel" ~ "#FC8D59", 
                            quantile_class == "Muito Vulneravel" ~ "#D73027",
                            quantile_class == "Deficiente de dados" ~ "#D1D1C6")
                        ), 
                    color_code = fct_relevel(color_code, "#D1D1C6", "#4575B4", "#91BFDB", "#FEE090","#FC8D59","#D73027")
                ) %>% 
            dplyr::relocate("geometry", .after = "color_code")
        )

        # Proporção das classes
        rank_class_geral <- completo_ %>% 
            st_drop_geometry() %>% 
            group_by(quantile_class) %>% 
            summarise(
                n_muni = n(), 
                ) %>% 
            mutate(
                prop = n_muni/sum(n_muni)
            ) %>% 
            arrange(desc(quantile_class)
        )

        # Top_n municipios
        rank_vals <- completo_ %>% 
            st_drop_geometry() %>% 
            select(nome,Ind_vulnerabilidade,quantile_class) %>% 
            mutate(
                rank = dense_rank(desc(Ind_vulnerabilidade))
                ) %>% 
        arrange(rank) 
    }
    if(plot == TRUE){
        imagem <- pr_ %>% 
            ggplot() + 
            geom_sf(
                fill = "transparent",
                colour = "black") +
            geom_sf(
                data = mun_, 
                fill = "transparent",
                colour = "grey") +
            geom_sf(
                data = completo_, 
                aes(fill = quantile_class),
                colour = alpha("black", .75), linewidth = 0.2) + 
                labs(
                    title = " Todos os grupos") +
            scale_fill_manual(
                    values = c(table(completo_$color_code) %>% names()),
                     guide = guide_legend(
                        title = "",
                        title.position = "top")
                    ) +
            theme_bw() +
            theme(
                    legend.position="bottom"
        )
    return(imagem)
    } else {
        lista_rank = list(
            prop_classe = rank_class_geral,
            rank = rank_vals
            )
        return(lista_rank)
    }
}
