# Retorna nome das bases de dados da coleção
opree_collection <- function(refs_ = NULL){
    if(is.null(refs_)){
        return(
            db_oco %>% 
            dplyr::distinct(dB) %>% 
            dplyr::pull()
        )
    } else {
        return(
            db_oco %>% 
            dplyr::distinct(dB, reference) 
        )
    } 
}

opree_dB <- function(simpl_ = NULL){
     if(is.null(simpl_)){
        db_oco %>% 
        dplyr::group_by(dB) %>%
        dplyr::select(
            -dB,
            especie_ajustado, 
            long_dec, 
            lat_dec,
            ano_inicial, 
            ano_final
        )
        } else {
        db_oco %>% 
        dplyr::group_by(dB) %>%
        dplyr::select(
            -dB,
            especie_ajustado, 
            long_dec, 
            lat_dec,
            ano_inicial, 
            ano_final) %>% 
        {setNames(group_split(.), group_keys(.)[[1]])}    
    }
}

opree_exo_tbl <- function(filter_ = NULL){
    if(is.null(filter_)){
        return(
            db_splist %>% 
            dplyr::select(
                -registro, 
                - manter, 
                - motivo
            )
        )
    } else {
        return(
            db_splist %>% 
            dplyr::filter(identificacao == "especie") %>% 
            dplyr::select(
                -registro, 
                - manter, 
                - motivo
            )
        )
    }
}

opree_exo_lst <- function(clean_ = NULL){
    if(is.null(clean_)){
        return(
            list(
                especie = db_splist %>% 
                    dplyr::filter(identificacao == "especie") %>% 
                    dplyr::pull(especie) %>% 
                    base::sort(),
                genero = db_splist %>% 
                    dplyr::filter(identificacao == "gênero") %>% 
                    dplyr::pull(especie) %>% 
                    base::sort(),
                familia = db_splist %>% 
                    dplyr::filter(identificacao == "família") %>% 
                    dplyr::pull(especie) %>% 
                    base::sort(),
                hibridos = db_splist %>% 
                    dplyr::filter(identificacao == "hibrido") %>% 
                    dplyr::pull(especie) %>% 
                    base::sort()
            )
        )
    } else {
        return(
            db_splist %>% 
            dplyr::filter(identificacao == "especie" & manter == "sim") %>% 
            dplyr::pull(especie) %>% 
            base::sort()
        )
    }
}


