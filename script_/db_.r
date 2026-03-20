gc()
library(tidyverse)

# Carrega dados da lista de espécies
db_splist <- readODS::read_ods("/media/ricardo/Backup1/Observatorio/Pacote/db_up/171125_spdb.ods") %>% 
    dplyr::select(
        -"distribuicao_nativa",	
        - "distribuicao_classe",	
        - "fonte",	
        - "elegibilidade_modelo"
    )

# Cria database interno
# usethis::use_data(db_splist, overwrite = TRUE)


# Carrega dados de ocorrência
db_oco <- data.table::fread("/media/ricardo/Backup1/Observatorio/Pacote/db_up/coords.csv") %>% 
    as_tibble() %>% 
    dplyr::filter(inside == 1) %>% 
    dplyr::rename("especies_ajustado" = "species_ajustado") %>% 
    dplyr::select(
        -sinonimo, 
        -inside,
        -sp_ctrl
    )

# usethis::use_data(db_oco, overwrite = TRUE)
