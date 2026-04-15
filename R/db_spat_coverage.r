#' @title Estimate extent of occurrences using controlled grid cell 
#' @description mapping occurrence extent using controled grid cell
#' @param data_ `data.frame` Data frame or tible containing longitude and latitude in decimals
#' @param shape_ `polygon` spatial polygon of extent of interest.
#' @param long_ `string` name of longitude variable
#' @param lat_ `string` name of latitude variable
#' @param area_ `numeric` number with the size of ares of interest. 
#' @param hex_ `boolean` If `TRUE` grid cells in hexagon.
#' 
#' @return `data.frame` containg the number of occurrences per grid cell
#' 
#' @import dplyr
#' @import units
#' @import tibble
#' @import sf   
#' @import stringr
#' 
#' @export
opree_spat_grid <- function(data_, shape_,long_, lat_, area_, square_grid = c(TRUE,FALSE)){
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
            poly_ <- sf::st_as_sf(shape_) %>% 
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
            square = square_grid
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
                area = sf::st_area(
                    .[[attr(., "sf_column")]] 
                    ) %>% 
                    units::set_units(., "km^2") ,
                .after = "n_ocs"
        )
    }
    return(count_tbl)
}

#' @export
opree_grid_grp <- function(data_1 = NULL, data_2, sp_, long_, lat_){
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
            # Subset by life form
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
        dplyr::left_join(data_2, .)
    return(pts_cells)
}

#teste2 <- opree_prep_grp(
#    data_2 = teste, 
#    sp_ = "especie_ajustado", 
#    long_ = "long_dec", 
#    lat_ = "lat_dec"
#)

