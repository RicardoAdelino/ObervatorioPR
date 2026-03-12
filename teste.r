gc()
library(tidyverse)
library(sf)
library(terra)
library(patchwork)
library(smoothr)

load("data/db_splist.rda")
load("data/db_oco.rda")
load("data/geometrias.rda")
load("data/db_oco.rda")

# Filtra lista inicial
    sp_ <- db_splist %>% 
        dplyr::filter(nivel_classificacao == 'especie' & manter == "SIM") %>% 
        dplyr::pull(especie_ajustado) 

    # Aplica filtro nas ocorrencias
    pr_obs <- db_oco %>%     
        sf::st_as_sf(
            ., 
            coords = c("long_dec","lat_dec"), 
            remove = FALSE,
            crs = 4326
        ) %>% 
        sf::st_transform(32722) #%>% 
    #dplyr::filter(especie %in% sp_) 

    # ⤷ Carrega poligono do Paraná
    pr_ <- geometrias %>%
        filter(class == 'OpenStreeMap') %>% 
        # UTM 22J (recomendado para o PR)
    sf::st_transform(32722)

    # ⤷ Carrega poligono do Paraná
    # if parametro shape_ == NULL usar dados internos
    # else, usar o fornecido (garantir transformação em classe sf)
    mun_ <- geometrias %>% 
        filter(class == 'IBGE') %>% 
         # UTM 22J (recomendado para o PR)
    sf::st_transform(32722)

    # ⤷ Cria hexagonos ao longo do poligono do Parana
    grid_ <- sf::st_make_grid(
        # Sf object
        pr_,
        # control grid area (https://github.com/r-spatial/sf/issues/1505)
        cellsize = units::as_units(100, "km^2"), 
        # Return class
        what = 'polygons',
        # Hexagonal
        square = FALSE
        )  %>% 
        # sf object
        sf::st_as_sf() %>% 
        # crop hexagons to territorial limmits
        sf::st_intersection(
            ., 
            pr_
        )  %>% 
        # Create ID for each hexagon
        tibble::rownames_to_column(
            ., 
            var = "hex_id"
        ) %>% 
        dplyr::mutate(
            hex_id = as.numeric(hex_id)
    )

    # Redefine os dados
    data_ <- pr_obs

    cob_ <- data_ %>%  
            dplyr::group_by(especie_ajustado) %>% 
            st_difference() %>% 
        {setNames(group_split(.), group_keys(.)[[1]])} %>% 
    purrr::keep(., ~ nrow(.x) > 2)


    # Passo 2: Cria polígono nos registros de ocorrência
    # O poligono pode ser obtido varias maneiras, aqui preferi manter a abordagem de poligono concavo
    # Usado para estimar a área potencial que pode ser ocupada pela espécies
    # Esse valor representa a medida de peso dos indices municipais
    hull_list <- pbapply::pblapply(
        cob_, 
        function(x){
            x %>% 
            terra::vect() %>% 
            terra::hull(., type="concave_length", param = .01) %>% 
            sf::st_as_sf()
        }
    )

    # Cria células dentro do polígono estimado
    in_pol <- list()
    for(i in 1:length(cob_)){
        in_pol[[i]] <- grid_ %>% 
            dplyr::mutate(range_cells = sf::st_intersects(.,  hull_list[[i]]) %>% lengths(),.after = "categoria") %>% 
            dplyr::mutate(point_cells = sf::st_intersects(.,  cob_[[i]]) %>% lengths(),.after = "range_cells")
    }
    names(in_pol) <- names(cob_)

    # Passo 3: relação entre Observado e esperado para o estado
    # Conta numero de celulas da distribuição estimada
    range_ncell <- lapply(in_pol, function(x){x %>% filter(range_cells == 1) %>% nrow()}) %>% 
        unlist() %>% 
        tibble::enframe(name = "especie", value = "distr_esperada")

    # Conta numero de celulas obsevadas
    obs_ncell <- lapply(in_pol, function(x){x %>% filter(point_cells == 1) %>% nrow()}) %>% 
        unlist() %>% 
        tibble::enframe(name = "especie", value = "distr_observada")

    # Monta data frame
    cbtra_spacial <- dplyr::left_join(
        range_ncell, 
        obs_ncell, 
        by = "especie") %>% 
        dplyr::mutate(
            Peso = distr_observada/distr_esperada, 
            EOO_km2 = distr_esperada * 50,
            AOO_km2 = distr_observada * 50
            ) %>% 
        dplyr::arrange(
            dplyr::desc(Peso)   
    )

    cbtra_spacial














oco <- db_oco %>% 
    sf::st_as_sf(
        ., 
        coords = c("long_dec","lat_dec"), 
        remove = FALSE,
        crs = 4326
    ) %>% 
sf::st_transform(32722) 

oco  %>%  names()
teste <- ivm(dados = oco)

###################################################
# testando plot
###################################################

opr_classes(
    dados = teste, 
    plot = FALSE
)
apree_spatdb <- tibble::tibble(
    nome = c(
        "TEOW",
        "FEOW",
        "MEOW",
        "FITO",
        "BAC_HIDRO",
        "UNI_HIDRO"
    ),
    source = c(
        "https://storage.googleapis.com/teow2016/Ecoregions2017.zip", 
        "https://www.feow.org/files/downloads/GIS_hs_snapped.zip",
        "https://wri-public-data.s3.amazonaws.com/resourcewatch/bio_018_marine_ecoregions.zip",
        "https://metadados.snirh.gov.br/geonetwork/srv/api/records/d6f207dc-4298-4ff2-9c1e-1cb9f343cd9e/attachments/GEOFT_COBERTURA_VEGETAL_NATIVA.zip",
        "http://www.iat.pr.gov.br/sites/agua-terra/arquivos_restritos/files/documento/2020-07/bacias_hidrograficas_parana.zip",
        "http://www.iat.pr.gov.br/sites/agua-terra/arquivos_restritos/files/documento/2020-07/unidades_hidrograficas_parana.zip"
    )    
)
