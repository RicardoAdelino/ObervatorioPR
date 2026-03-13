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

