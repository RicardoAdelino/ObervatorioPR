gc()
library(tidyverse)
library(sf)
library(terra)

db_oco <- readRDS("/media/ricardo/Backup2/Observatorio/Pacote/db_/ocorrencias.rds")
usethis::use_data(db, overwrite = TRUE)

#save(db,file = "data/db_oco.rda")
db_splist <- readODS::read_ods("data/sp_list.ods")
usethis::use_data(db_splist, overwrite = TRUE)


load("data/db_splist.rda")
load("data/db_oco.rda")
load("data/geometrias.rda")

vt <- db_splist %>% filter(eco_evo_class == "vertebrado_terrestre") %>% pull(especie)
sp <- db_oco %>% filter(especie_ajustado %in% vt) 
#municipios <- geometrias %>% filter(class == "IAT") %>% vect()
#bacia <- geometrias %>% filter(class == "EcoRegions") %>% vect()
ecor <- geometrias %>% filter(class == "IBGE") %>% vect()

ind <- opree_cp(
    data_ = sp, 
    long = "long_dec", 
    lat = "lat_dec", 
    shp_ = ecor,
    shape_var = "nome", 
    data_var = "especie_ajustado"
)

opree_map_cp(
    data_ = ind,
    shp = ecor,
    lgd_break = 6,
    name_ = "teste"
)
