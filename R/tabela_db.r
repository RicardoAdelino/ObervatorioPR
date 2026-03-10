#' Lista de espécies (Tabela)
#'
#' Acessa a tabela com o levantamento das lista de espécies para cada uma das seguintes bases de dados:
#' **Instituto Água e Terra**, **Instituto Hórus**, **Intituto Chico Mendes de Biodiversidade**, 
#' **Associação de Portos de Paranaguá** e **Rivisão da Literatura Científica**.
#'    
#' @param data Tabela de presença e ausência de espécies por base dados
#' @param scale_ `caractere` seleciona o nível taxonomico necessario para amostrar a tabela.
#' `genero:` amostra apenas amostras classificadas a nivel de genero, `especie:` amostra apenas amostras classificadas a nível de espécie.
#' Padrão `scale_ = NULL` retorna tabela de presença e ausência bruta.
#' 
#' @return `tibble` Tabela com a lista de espécies amostradas, com nomenclatura cienífica padronizada encontrada nas bases de dados e na revisão da literatura. 
#' 
#' @examples 
#' 

opre_table <- function(data, scale_ = NULL){
    if(is.null(scale_)){
        print(
            "Nenhum critério taxonomico foi escolhido \n 
            Retornando tabela bruta!"
        )
        return(data)      
    } else {
       return(
            data %>% 
            dplyr::filter(ident == {{scale_}}) %>% 
            dplyr::select(-ident)
        )
    }
}