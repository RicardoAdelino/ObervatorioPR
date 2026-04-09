#' @title Access ecological information from opree
#' @description Access biological information from opree databases
#' @param clean_ `boolean` If TRUE, return speceis with high confidence of invasisve status. If `NULL` retunr all the data
#' @import dplyr

#' @export
opree_lifeform <- function(clean_ = NULL){
    if(is.null(clean_)){
        return(
            db_splist %>% 
            dplyr::select(species, life_form)
        )
    } else {
       db_splist %>% 
       dplyr::filter(identification == "species" & keep == "yes") %>% 
       dplyr::select(species, life_form)
    }
}

#' @export
opree_ecoevo <- function(clean_ = NULL){
    if(is.null(clean_)){
        return(
            db_splist %>% 
            dplyr::select(species, eco_evo_class)
        )
    } else {
       db_splist %>% 
       dplyr::filter(identification == "species" & keep == "yes") %>% 
       dplyr::select(species, eco_evo_class)
    }
}

#' @export
opree_ecos <- function(clean_ = NULL){
    if(is.null(clean_)){
        return(
            db_splist %>%
            dplyr::select(species, ecossistem)
        )
    } else {
       db_splist %>% 
       dplyr::filter(identification == "species" & keep == "yes") %>% 
       dplyr::select(species, ecossistem)
    }
}

#' @export
opree_class <- function(clean_ = NULL){
    if(is.null(clean_)){
        return(
            db_splist %>% 
            dplyr::select(species, status)
        )
    } else {
       db_splist %>% 
       dplyr::filter(identification == "species" & keep == "yes") %>% 
       dplyr::select(species, status)
    }
}