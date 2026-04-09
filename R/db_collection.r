#' @title Database collection
#' @description Group of four key functions to access opree database.
#' `opree_collection:` retorna uma lista com os nomes das bases de dados utilizadas
#' `opree_dB:` retorna dados dos registros de ocorrência combinados ou em lista
#' `opree_exo_tbl:` lista de espécies
#' `opree_exo_lst:` lista de espécies
#  
#' #' @param refs_ `boleano` Se TRUE retorna referencias. FALSE retorna nome das bases de dados
#' @param simpl_ `boleano` Se TRUE retorna lista de data frames para cada dB
#' @param filter_ `boleano` Se TRUE retorna lista com taxa identificados a nível de especie
#' @param clean_ `boleano` 
#' 
#' @return 
#' `opree_collection:` return specific data for database references, database occurrences
#' `opree_exo_tbl:` return list of taxa detected at species level
#' `opree_exo_lst:` return list of taxa detected at multiple taxonomic levels
#' 
#' @import dplyr
#' 
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

#' @export
opree_exo_tbl <- function(filter_ = NULL){
    if(is.null(filter_)){
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
            dplyr::filter(identification == "species") %>% 
            dplyr::select(
                species, 
                identification
            )
        )
    }
}

#' @export
opree_exo_lst <- function(clean_ = NULL){
    if(is.null(clean_)){
        return(
            list(
                species = db_splist %>% 
                    dplyr::filter(identification == "species") %>% 
                    dplyr::pull(species) %>% 
                    base::sort(),
                genus = db_splist %>% 
                    dplyr::filter(identification == "genus") %>% 
                    dplyr::pull(species) %>% 
                    base::sort(),
                familt = db_splist %>% 
                    dplyr::filter(identification == "family") %>% 
                    dplyr::pull(species) %>% 
                    base::sort(),
                hybrid = db_splist %>% 
                    dplyr::filter(identification == "hybrid") %>% 
                    dplyr::pull(species) %>% 
                    base::sort()
            )
        )
    } else {
        return(
            db_splist %>% 
            dplyr::filter(identification == "species" & keep == "yes") %>% 
            dplyr::pull(species) %>% 
            base::sort()
        )
    }
}
