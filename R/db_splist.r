#' @title opree species list
#' @description This function allow to access the up-to date non-native species list
#' @param sp_val `bolean` determine the level of confidence of species in species list 
#' @return Default is `TRUE` and return a tibble containing the species with high confidence in their non-native status and prefered environment.
#' @import dplyr' 
#' @export 

opree_exo_tbl <- function(sp_val = TRUE){
    if(sp_val == FALSE){
        return(
            db_splist %>% 
            dplyr::select(
                species, 
                identification
            )
        )
    } else {
        return(
            db_splist %>% 
            dplyr::filter(
                identification == "species" &
                keep == "yes" & 
                status  %in% c("exo_br", "exo_pr")
                ) %>% 
            dplyr::select(
                species,  
                ecossistem
            )
        )
    }
}