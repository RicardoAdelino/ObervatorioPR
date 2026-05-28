#' @title opree species list
#' @description This function allow to access the up-to date non-native species list
#' @param summary_ `bolean` Deafult is `TRUE` and determine filtered conditions 
#' @param subset_ `vector` with the condition to be filtered
#' @param key_ `vector` with the variable to the key variable to be filtered or summarized
#' @return Default is `TRUE` and return a tibble containing the species with high confidence in their non-native status and prefered environment.
#' @import dplyr 
#' @examples 
#' \dontrun{
#' # Get species list
#' opree_exo_tbl(summary_ = FALSE)
#' 
#' # Summary of group of interest
#' opree_exo_tbl(summary_ = TRUE, key_ = "eco_evo_class")
#' 
#' # Filter by of group of interest
#' opree_exo_tbl(subset_ = "aquatic_vertebrate", key_ = "eco_evo_class") 
#' }
#' @export 

opree_exo_tbl <- function(summary_ = TRUE, subset_ = NULL, key_ = NULL){
    if (!is.null(subset_) && !is.null(key_)) {
        cat(">>>> Filter list of species by key elements")
        return(
            db_splist %>% 
            dplyr::filter(keep == "yes") %>% 
            dplyr::filter(.data[[key_]] %in% subset_)
        )
    } 
    if(isTRUE(summary_)){
        cat("Returning summary of specis by group")
        return(
            db_splist %>% 
            dplyr::filter(keep == "yes") %>% 
            dplyr::group_by(.data[[key_]]) %>% 
            dplyr::summarise(
                n_sp = n()
            )
        )
        
    } else {
        cat("Retuning opree non-native species list \n")
        return(
            db_splist %>% 
            dplyr::filter(keep == "yes") %>% 
            dplyr::select(-keep)
        )
    }
}