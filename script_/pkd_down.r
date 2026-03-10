pkgdown::build_site()
pkgdown::clean_site()

install.packages("hexSticker")
library(hexSticker)

sticker(subplot = "image_/Logo.png", # Endereço da onde a imagem está
        package = "spatdB",  # Nome que vai ser exibido na imagem
        p_size = 20, # Tamanho da Fonte do nome do pacote
        s_x = 1, 
        s_y = .95, # Posição da imagem, eixo X e Y
        s_width = .8, # Tamanho da imagem
        h_fill = "#0a0a0a", # Cor do fundo
        h_color = "#2e9123", # Cor da borda,
        filename = "image_/hexstick.png" 
) 
