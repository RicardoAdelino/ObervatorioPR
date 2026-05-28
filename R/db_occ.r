#' @title occurrences database
#' 
#' @description Main file containing occurrence records from the collection of databases  
#' @param raw_ `boleano` Default is `TRUE` return a data.frame with species occurrences 
#' @param subset_ `vector` with the condition to be filtered
#' @param key_ `vector` with the variable to the key variable to be filtered
#' @return return data.frame with species name, longitude, latitude and available year of observation
#' @import dplyr
#' @examples
#' \dontrun{
#' opree_dB(raw = FALSE,subset_ = "PELD", key_ = "dB")
#' }
#' @export

opree_dB <- function(raw_ = TRUE, subset_ = NULL, key_ = NULL){
    if (!is.null(subset_) && !is.null(key_)) {
        cat("Filter by subset_ in function of key_")
        return(
            db_oco %>% 
            dplyr::filter(.data[[key_]] %in% subset_)
        )
    } 
    if (isTRUE(raw_)) {
        cat("Return raw data with 18 variables\n")
        return(db_oco)
    } else {
        cat("Return simplified data frame containing 5 key variables\n")
        return(
        db_oco %>%
            dplyr::select(
                species_adj,
                long_dec,
                lat_dec,
                start_year,
                final_year
            )
        )
    }
}

