#' Registros de ocorrência do spLink
#'
#' Esta função permite fazer o download dos registros de ocorrência da base de dados brasileira spLink em formato `.csv` para uma ou múltiplas espécies.
#'    
#' @param splist `vetor`com a nomenclatura biniminal das espécies (i.e., c("Lepus saxatilis","Puma concolor"))
#' @param token `vetor`com sua chave de segurança gerada pessoal (i.e., token) na plataforma spLink
#' @param api_list `lista` dos caminhos da API gerados para cada espécies
#' @param lst `boolenao` indicando se o resultado deve ser no formato lista de data frame (lst = TRUE) ou condensado em um único data frame (lst = FALSE)
#' 
#' @return `data_prep` retorna uma lista com o caminho especifico de cada espécies para busca na API. `opr_spLink` retorna arquivo único do tipo data frame para todas as espécies de interesse da lista. Retona lista de dataframes se o paramentro `lst == TRUE`. 
#' 
#' @examples 
#' NOT RUN
#' spList <- data_prep(    
#'    splist = c("Perna perna","Perna viridis","Abramites hypselonotus","Callithrix jacchus"), 
#'    token = "adicine seu token"
#' )
#'
#'opr_spLink(api_list = spList, lst = TRUE)

# Prepara os dados
data_prep <- function(splist, token = NULL){
    if(is.null(token)){
        return(
            "Token não detectado! Insira sua chave de segurança"
        )
    } else {
        # Padroniza lista de espécies para busca na API
        dados <- splist %>% 
        base::tolower() %>%
        stringr::str_replace_all(., "[ _]", "+") %>% 
        unique()

        # Adiciona chanve token na API
        api_ <- paste0("https://specieslink.net/ws/1.0/search?scientificname=invaders&country=Brazil&offset=0&limit=50000&apikey=", token)    
        
        # Itera sobre a lista de espécies para adidionar cada espécies como um parametro de busca 
        api_path <- list()
        for (i in 1:length(dados)) {
            api_path[[i]] <- str_replace(api_, "invaders", dados[i])
        }
        names(api_path) <- str_replace(dados,"\\+","_")

        }
    return(api_path)
}

opr_spLink <- function(api_list, lst = c(TRUE,FALSE)){
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


