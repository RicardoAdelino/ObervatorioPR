#' Visao Veral
#'
#' Esta função permite gerar tabelas de síntese descritiva dos dados do obsevatório paranaense de espécies exóticas.
#' As categorias utilizadas até o momento são: `reino`,`classe`,`ordem`,`família`, `ecossistema`,`forma_de_vida`.
#'   
#' @param categoria argumento do tipo `string` usado para selecionar uma classe de descrição específica  
#' @param all argumento `boolenao` indicando se todas as caracteristicas devem ser computadas. Esse parametro sobrepõe qualquer arumento do paramentro categoria, que passa ser obsoleto se all = TRUE.
#'
#' @return Se all = TRUE retorna uma lista de data frames com a contagem de espécies por variável de interesse. Se a categoria for unica e específica, retorna apenas o data frame específico para a categoria. 
#'
#' @examples 
#' # NOT RUN
#' opr_sumario_(categoria = NULL, all = TRUE)

opr_sumario <- function(categoria = NULL, all = NULL){
    if(all == TRUE){
        grupos <- c("reino","classe","ordem","família","ecossistema","forma_de_vida")
        # Prepara objeto para armazenar os dados
        out_lista <- NULL
        # Define rótulo das listas
        out_lista[grupos] <- list(NULL)
        # Iterador para resumir todas as principais variaveis
        for(i in 1:6){
            out_lista[[i]] <- db_splist %>% 
            dplyr::filter(manter == "SIM") %>% 
            dplyr::group_by(!!!syms(grupos[i])) %>% 
            dplyr::count(., name = "contagem") %>%
            #dplyr::rename()  
            arrange(
                desc(contagem)
            )
        }
        return(out_lista)
    } else {
        out_categoria <- db_splist %>% 
            dplyr::filter(manter == "SIM") %>% 
            dplyr::group_by(!!!syms(categoria)) %>% 
            dplyr::count(., name = "contagem") %>% 
            arrange(
                desc(contagem)
            )
        return(out_categoria)
    }     
}


