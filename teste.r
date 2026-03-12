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
