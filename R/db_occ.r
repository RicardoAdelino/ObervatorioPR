#' @title occurrences database
#' 
#' @description Main database of opree containing occurrence records from the collection of databases  
#' 
#' @param simplify_ `boleano` If `TRUE` return a list of data.frame for each database contributor 
#' 
#' @return return data.frame with species name, longitude, latitude and available year of observation
#' @import dplyr
#' @export
opree_dB <- function(simplify_ = NULL){
     if(is.null(simplify_)){
        db_oco %>% 
        #dplyr::group_by(dB) %>%
        dplyr::select(
            species_adj, 
            #biolevel,
            long_dec, 
            lat_dec,
            start_year, 
            final_year
        )
        } else {
        db_oco %>% 
        dplyr::group_by(dB) %>%
        dplyr::select(
            species_adj, 
            #biolevel,
            long_dec, 
            lat_dec,
            start_year, 
            final_year) %>% 
        {setNames(group_split(.), group_keys(.)[[1]])}    
    }
}
