#' @title Database collection
#' @description This function allow to access the sources of databases and their references
#' @param refs_ `bolean` parameter to determine if return references associated with databases  
#' @return Default return a vector with database identification code. If refs_ = TRUE return a tibble with database identification code and the references associated.
#' @import dplyr 
#' @export 
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