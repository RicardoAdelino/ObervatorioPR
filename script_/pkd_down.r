gc()
library(tidyverse)
library(hexSticker)

#db_oco <- readRDS("data/ocorrencias.rds")
#db_splist <- readODS::read_ods("data/sp_list.ods")
#save(db_oco, file = "data/db_oco.rda")
#save(db_splist, file = "data/db_splist.rda")

#
#pkgdown::build_site()
#pkgdown::clean_site()

sticker(subplot = "image_/Logo.png", # Endereço da onde a imagem está
        package = "opree",  # Nome que vai ser exibido na imagem
        p_size = 20, # Tamanho da Fonte do nome do pacote
        s_x = 1, 
        s_y = .95, # Posição da imagem, eixo X e Y
        s_width = .8, # Tamanho da imagem
        h_fill = "#0a0a0a", # Cor do fundo
        h_color = "#2e9123", # Cor da borda,
        filename = "image_/hexstick.png" 
) 
