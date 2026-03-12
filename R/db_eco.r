opree_lifeform <- function(clean_ = NULL){
    if(is.null(clean_)){
        return(
            db_splist %>% select(especie, forma_vida)
        )
    } else {
       db_splist %>% 
       dplyr::filter(identificacao == "especie" & manter == "sim") %>% 
       select(especie, forma_vida)
    }
}

opree_ecoevo <- function(clean_ = NULL){
    if(is.null(clean_)){
        return(
            db_splist %>% select(especie, eco_evo_class)
        )
    } else {
       db_splist %>% 
       dplyr::filter(identificacao == "especie" & manter == "sim") %>% 
       select(especie, eco_evo_class)
    }
}

opree_ecos <- function(clean_ = NULL){
    if(is.null(clean_)){
        return(
            db_splist %>% select(especie, ecossistema)
        )
    } else {
       db_splist %>% 
       dplyr::filter(identificacao == "especie" & manter == "sim") %>% 
       select(especie, ecossistema)
    }
}

opree_class <- function(clean_ = NULL){
    if(is.null(clean_)){
        return(
            db_splist %>% select(especie, status)
        )
    } else {
       db_splist %>% 
       dplyr::filter(identificacao == "especie" & manter == "sim") %>% 
       select(especie, status)
    }
}

opree_class(clean_ = 1)
