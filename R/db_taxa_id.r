#' @title opree taxa assessment
#' @description This function allow to access the groups of taxa in the raw file of database
#' @param list_lv `bolean` determine the level of identification in raw data 
#' @return Default return a list of vector containing different levels of identification. 
#' If `list_lv = FALSE` return a list of vector with non-valid classificatopm level
#' @import dplyr' 
#' @export 

opree_exo_lst <- function(list_lv = TRUE){
    if(list_lv == TRUE){
        return(
            list(
                species = db_splist %>% 
                    dplyr::filter(
                        identification == "species" 
                    ) %>% 
                    dplyr::pull(species) %>% 
                    base::sort(),
                genus = db_splist %>% 
                    dplyr::filter(
                        identification == "genus" 
                    ) %>% 
                    dplyr::pull(species) %>% 
                    base::sort(),
                family = db_splist %>% 
                    dplyr::filter(
                        identification == "family" 
                    ) %>% 
                    dplyr::pull(species) %>% 
                    base::sort(),
                hybrid = db_splist %>% 
                    dplyr::filter(
                        identification == "hybrid" 
                    ) %>% 
                    dplyr::pull(species) %>% 
                    base::sort()
            )
        )
    } else {
        return(
            list(
                unknow = db_splist %>% 
                        dplyr::filter(
                            status == "unknow"
                        )%>% 
                    dplyr::pull(species),
                native_pr = db_splist %>% 
                        dplyr::filter(
                            status == "nat_pr"
                        )%>% 
                    dplyr::pull(species),
                native_br = db_splist %>% 
                        dplyr::filter(
                            status == "nat_br"
                    ) %>% 
                    dplyr::pull(species)
            )
        )
    }
}
