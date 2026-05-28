#' @title Prepare data for occurrences from spLink
#' @description This function allows you to download occurrence records from the Brazilian spLink database in `.csv` format for one or multiple species.
#' @param splist `vector`species binomial (i.e., c("Lepus saxatilis","Puma concolor"))
#' @param token `vector` personal token obtained in spLink plataform
#' @return `data.frame` occurrences obtained from spLink
#' @import dplyr
#' @import httr2
#' @import stringr
#' @import purrr
#' @import tidyr
#' @examples 
#' \dontrun{
#' spList <- data_prep(    
#' splist = c("Perna perna","Perna viridis","Abramites hypselonotus","Callithrix jacchus"), 
#' token = "adicine seu token"
#' )
#' opree_spLink(api_list = spList, lst = TRUE)
#' }
#' @export 
data_prep <- function(splist, token = NULL){
    if(is.null(token)){
        stop("Token not detected! Enter with your security key!")
    } else {
        # Set species API list
        dados <- splist %>% 
            base::tolower() %>%
            stringr::str_replace_all(., "[ _]", "+") %>% 
            unique()
        # Add API token
        api_ <- paste0("https://specieslink.net/ws/1.0/search?scientificname=invaders&country=Brazil&offset=0&limit=50000&apikey=", token)    
        # Iterates over the list of species to add each species as a search parameter. 
        api_path <- list()
        for (i in 1:length(dados)) {
            api_path[[i]] <- stringr::str_replace(api_, "invaders", dados[i])
        }
        names(api_path) <- stringr::str_replace(dados,"\\+","_")

        }
    return(api_path)
}

#'@title Download occurrences from spLink
#' @description This function allows you to download occurrence records from the Brazilian spLink database in `.csv` format for one or multiple species.
#' @param api_list `list` of species API paths obtained from `data_prep` function
#' @param lst `boolen` indicating if output should return a list of data frame (lst = TRUE) or a single long data.frame (lst = FALSE)
#' @export 
opree_spLink <- function(api_list, lst = c(TRUE,FALSE)){
    spLink_list <- pbapply::pblapply(
    api_list, 
    function(x) {
        spLink <- x %>%
            # Request api acess
            httr2::request() %>%
            # Allowing request
            httr2::req_perform() %>%
            # Unstructured to json list
            httr2::resp_body_json()

        # Get features of json list
        spLinkFeat <- spLink$features %>%
            # Drop one level of list elements
            purrr::list_flatten() %>%
            # Keep list elements greather than 2
            purrr::keep(~ length(.x) > 2)

        # Combine all data in a new tibble
        spLink_structured <- spLinkFeat %>%
            dplyr::bind_rows()
            }
        )

        # Get points in area of interest
        spLink_ListToDf <- spLink_list %>%
            dplyr::bind_rows() %>%
            dplyr::mutate(
                decimallongitude = as.numeric(decimallongitude),
                decimallatitude = as.numeric(decimallatitude)
                ) %>%
            dplyr::filter(
                decimallongitude != 0,
                decimallatitude != 0
                ) %>%
            dplyr::relocate(decimallatitude, .after = decimallongitude) %>%
            tidyr::drop_na(decimallongitude, decimallatitude) %>%
            dplyr::mutate(
                on_land = lengths(
                    sf::st_within(
                        sf::st_as_sf(
                            ., 
                            coords = c("decimallongitude", "decimallatitude"), 
                            remove = FALSE, 
                            crs = st_crs(4326)
                        ),
                    geobr::read_state(code_state = "PR", showProgress = FALSE) %>% 
                    sf::st_transform(4326) %>% 
                    suppressMessages()
                )
            )
        ) %>% 
        dplyr::filter(on_land == 1) %>% 
        dplyr::select(-on_land)
        
        if(lst == TRUE){
            spLink_lst <- spLink_ListToDf %>% 
                dplyr::group_by(scientificname) %>% 
                {
                    stats::setNames(
                        dplyr::group_split(.), 
                        dplyr::group_keys(.)[[1]]
                    )
                }
        return(spLink_lst)
    } else {  
        return(spLink_ListToDf)
    }
}


