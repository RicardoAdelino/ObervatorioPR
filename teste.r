gc()
library(tidyverse)
library(sf)
library(terra)
library(patchwork)
library(smoothr)

opree_collection()



load("data/db_splist.rda")
load("data/db_oco.rda")
load("data/geometrias.rda")
load("data/db_oco.rda")


grid_ <- sf::st_make_grid(
    # Sf object
    geometrias %>%
    filter(class == 'OpenStreeMap') %>% 
    # UTM 22J (recomendado para o PR)
    sf::st_transform(32722),
    # control grid area (https://github.com/r-spatial/sf/issues/1505)
    cellsize = units::as_units(2000, "km^2"), 
    # Return class
    what = 'polygons',
    # Hexagonal
    square = TRUE
    )  %>% 
    # sf object
    sf::st_as_sf() %>% 
    # crop hexagons to territorial limmits
    sf::st_intersection(
        ., 
        geometrias %>%
        filter(class == 'OpenStreeMap') %>% 
        st_bbox() %>% 
        st_as_sfc(, crs = 4326) %>% 
        sf::st_transform(32722)
    )  %>% 
    # Create ID for each hexagon
    tibble::rownames_to_column(
        ., 
        var = "hex_id"
    ) %>% 
    dplyr::mutate(
        hex_id = as.numeric(hex_id)
)

ggplot() +
    geom_sf(
        data = st_geometry(geometrias %>%
                filter(class == 'OpenStreeMap') %>% 
                # UTM 22J (recomendado para o PR)
                sf::st_transform(32722))
        ) + 
    geom_sf(
        data = grid_, 
        fill = "transparent"
        )   +
        annotate(
            "text",
            x = st_coordinates(st_centroid(grid_))[,1],
            y = st_coordinates(st_centroid(grid_))[,2], 
            label = grid_$hex_id
) +
theme_bw()    

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
