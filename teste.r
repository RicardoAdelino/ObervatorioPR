gc()
library(tidyverse)
library(sf)
library(terra)
library(patchwork)
library(smoothr)

load("data/db_splist.rda")
load("data/db_oco.rda")
load("data/geometrias.rda")

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
