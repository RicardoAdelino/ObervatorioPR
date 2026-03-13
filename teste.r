gc()
library(tidyverse)
library(sf)
library(terra)

load("data/db_splist.rda")
load("data/db_oco.rda")
load("data/geometrias.rda")

vt <- db_splist %>% filter(eco_evo_class == "vertebrado_terrestre") %>% pull(especie)
sp <- db_oco %>% filter(especie_ajustado %in% vt) 
#municipios <- geometrias %>% filter(class == "IAT") %>% vect()
#bacia <- geometrias %>% filter(class == "EcoRegions") %>% vect()
ecor <- geometrias %>% filter(class == "EcoRegions") %>% vect()
ecor$nome
