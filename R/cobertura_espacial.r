#load("data/geometrias.rda")

opree_prep_data <- function(data_, shape_,long_, lat_, area_, hex_ = c(TRUE,FALSE)){
    if(is.null(data_)){
        stop("Insira os dados de entrada!")    
    } else if (is.null(shape_)) {
       stop("Insira os dados espaciais!")
    } else {
       obs_ <- data_ %>%     
        sf::st_as_sf(
            ., 
            coords = c(long_,lat_), 
            remove = FALSE,
            crs = 4326
        ) %>% 
        sf::st_transform(32722)
        
        if(inherits(shape_,"SpatVector")){
            poly_ <- st_as_sf(shape_) %>% 
            sf::st_transform(32722)
        } else {
            poly_ <- shape_ %>% 
            sf::st_transform(32722)
        }
        
        # ⤷ Cria hexagonos ao longo do poligono do Parana
        print("Criando malha espacial na área de interesse, isso pode levar alguns minutos!")
        hex_ <- sf::st_make_grid(
            poly_,
            cellsize = units::as_units(area_, "km^2"), 
            what = 'polygons',
            square = hex_
            )  %>% 
            sf::st_as_sf() %>% 
            sf::st_intersection(
                ., 
                poly_
            )  %>% 
            tibble::rownames_to_column(
                ., 
                var = "hex_id"
            ) %>% 
            dplyr::mutate(
                hex_id = as.numeric(hex_id)
        )

        print("Calculando frequencia de registros por unidade espacial")

        count_tbl <- hex_ %>% 
            dplyr::mutate(
                n_ocs = sf::st_intersects(., obs_) %>% 
                    base::lengths(),
                    .before = x
                ) %>% 
            dplyr::group_by(hex_id) %>% 
            dplyr::summarise(
                n_ocs = base::sum(n_ocs)
            ) %>% 
            dplyr::mutate(
                area = st_area(
                    .[[attr(., "sf_column")]] 
                    ) %>% 
                    units::set_units(., "km^2") ,
                .after = "n_ocs"
        )
    }
    return(count_tbl)
}

#teste <- opree_prep_data(
#    data_ = db_oco, 
#    shape_ = geometrias %>% filter(class == "OpenStreeMap") %>% terra::vect(),
#    long_ = "long_dec", 
#    lat_ = "lat_dec", 
#    area_ = 100, 
#    hex_ = FALSE
#)
#

opree_prep_grp <- function(data_1 = NULL, data_2, sp_, long_, lat_){
    if(is.null(data_1)){
        oco <- opree_dB()
    } else {
       oco = data_2
    }

    oco <- oco %>% 
        dplyr::select(
            sp_, 
            long_,
            lat_
        ) %>% 
        dplyr::left_join(
            .,
            opree_ecoevo(), 
            by = c("especie_ajustado" = "especie")
        ) %>%   
        sf::st_as_sf(
            ., 
            coords = c(long_, lat_), 
            remove = FALSE,
            crs = 4326
        ) %>% 
        sf::st_transform(32722)
    
    pts_cells <- sf::st_join(
            oco, 
            data_2, 
            join = sf::st_within, 
            lefft = FALSE
        ) %>% 
        sf::st_drop_geometry() %>% 
        left_join(data_2, .)
    return(pts_cells)
}

#teste2 <- opree_prep_grp(
#    data_2 = teste, 
#    sp_ = "especie_ajustado", 
#    long_ = "long_dec", 
#    lat_ = "lat_dec"
#)



opree_map <- function(data_ = NULL, pallete_ = "Spectral", lgd_break){
    return(
        if(is.null(data_)){
            stop("Insira os dados de entrada!")    
        } else if (!inherits(data_, "sf")){
           stop("dados de entrada não padronizados, prepare os dados usando a função opree_prep_data")
        }else {
            data_ %>% 
            ggplot2::ggplot() + 
            ggplot2::geom_sf(
                fill = "transparent",
                colour = "black") +
            ggplot2::geom_sf(
                data = hex_, 
                fill = "transparent",
                colour = "grey") +
            ggplot2::geom_sf(
                data = data_ %>% dplyr::filter(n_ocs > 0), 
                aes(fill = n_ocs),
                colour = alpha("grey", .75), linewidth = 0.2) +
            ggplot2::scale_fill_gradientn(
                colors = rev(RColorBrewer::brewer.pal(11, pallete_)),
                na.value = "transparent", 
                limits = c(min(data_$n_ocs), max(data_$n_ocs)),
                breaks = function(x) {
                    std_breaks <- scales::extended_breaks(n = lgd_break)(x)
                    # Remove any breaks within 7% of the edges to prevent overlap
                    buffer <- 0.07 * (max(x) - min(x))
                    filtered <- std_breaks[std_breaks > (min(x) + buffer) & 
                                           std_breaks < (max(x) - buffer)]
                    sort(unique(c(min(x), filtered, max(x))))
                },
                expand = c(0, 0)
            ) +
            ggplot2::labs(
                fill = "Acumulo de espécies"
            ) +
            ggplot2::theme_bw(base_size = 15) +
            ggplot2::theme(
                axis.text = element_text(size = 10),
                legend.text = element_text(size = 10),
                legend.position = "bottom",
                legend.title = element_text(size = 15),
                legend.justification = "center",
                plot.tag = element_text(size = 15, face = "bold"),
                panel.border = element_rect(fill = NA, color = "black"),
            ) +
            ggplot2::guides(
                fill = guide_colorbar(
                    barwidth = 15, 
                    barheight = 1.25,
                    title.position = "top",    
                    title.hjust = 0.5, 
                    frame.colour = "black",
                    ticks.colour = "black", 
                    draw.llim = TRUE,
                    draw.ulim = TRUE,
                )  
            )
        }
    )
}

opree_map(teste,lgd_break = 5)

