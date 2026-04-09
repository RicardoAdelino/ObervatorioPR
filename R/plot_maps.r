#' @title Plot maps derived from `opree_cp`
#' @description This function uses occurrence records to estimate colonization pressure measured by the number of exotic species per square kilometer of the unit of interest.#' @param data_ `data frame ou tibble` contendo os registros de ocorrência, nome das espécies ou grupos de interesse
#' @param data_`data.frame` from `opree_cp` function
#' @param shp_ `spatvect` geometry
#' @param lgd_break `string` scale of color values
#' @param name_ `string` plot title
#' @param pallete_ `string` any color sequence derived from `RColorBrewer`
#' @import dplyr
#' @import ggplot2
#' @import sf 
#' @import RColorBrewer
#' @examples 
#' \dontrun{
#' vt <- db_splist %>% filter(eco_evo_class == "vertebrado_terrestre") %>% pull(especie)
#' sp <- db_oco %>% filter(especie_ajustado %in% vt) 
#' ecor <- geometrias %>% filter(class == "IBGE") %>% vect()
#' ind <- opree_cp(
#' data_ = sp, 
#' long = "long_dec", 
#' lat = "lat_dec", 
#' shp_ = ecor,
#' shape_var = "nome", 
#' data_var = "especie_ajustado"
#' )
#' opree_map_cp(
#' data_ = ind,
#' shp = ecor,
#' lgd_break = 6,
#' name_ = "teste"
#' )
#'}
#' @export 
opree_map_cp <- function(data_, shp_, lgd_break, name_, pallete_ = "Spectral"){
    data_ <- suppressMessages(dplyr::left_join(shp_, data_)) 
    data_ <- sf::st_as_sf(data_)
    return(
      data_ %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_sf(
          aes(fill = perc),
          colour = "black") +
      ggplot2::scale_fill_gradientn(
          colors = rev(RColorBrewer::brewer.pal(11, pallete_)),
          na.value = "transparent", 
          limits = c(min(data_$perc), max(data_$perc)),
          breaks = function(x) {
              std_breaks <- scales::extended_breaks(n = lgd_break)(x)
              # Remove any breaks within 7% of the edges to prevent overlap
              buffer <- 0.07 * (max(x) - min(x))
              filtered <- std_breaks[std_breaks > (min(x) + buffer) & 
                                     std_breaks < (max(x) - buffer)]
              sort(unique(c(min(x), filtered, max(x))))
          },
          expand = c(0, 0)
      ) +
       ggplot2::labs(
        subtitle = name_,
        fill = ""
      )+
      ggplot2::theme_bw(base_size = 15) +
      ggplot2::theme(
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_text(size = 15),
        legend.justification = "center",
        plot.tag = element_text(size = 15, face = "bold"),
        panel.border = element_rect(fill = NA, color = "black"),
      ) +
      ggplot2::guides(
        fill = guide_colorbar(
        barwidth = 15, 
        barheight = 1.25,
        title.position = "top",    
        title.hjust = 0.5, 
        frame.colour = "black",
        ticks.colour = "black", 
        draw.llim = TRUE,
        draw.ulim = TRUE,
      )  
    )
  )          
}

#' @export 
opree_map <- function(data_ = NULL, pallete_ = "Spectral", lgd_break){
    return(
        if(is.null(data_)){
            stop("Insira os dados de entrada!")    
        } else if (!inherits(data_, "sf")){
           stop("dados de entrada não padronizados, prepare os dados usando a função opree_prep_data")
        }else {
            data_ %>% 
            ggplot2::ggplot() + 
            ggplot2::geom_sf(
                fill = "transparent",
                colour = "black") +
            ggplot2::geom_sf(
                #data = hex_, 
                fill = "transparent",
                colour = "grey") +
            ggplot2::geom_sf(
                data = data_ %>% dplyr::filter(n_ocs > 0), 
                aes(fill = n_ocs),
                colour = alpha("grey", .75), linewidth = 0.2) +
            ggplot2::scale_fill_gradientn(
                colors = rev(RColorBrewer::brewer.pal(11, pallete_)),
                na.value = "transparent", 
                limits = c(min(data_$n_ocs), max(data_$n_ocs)),
                breaks = function(x) {
                    std_breaks <- scales::extended_breaks(n = lgd_break)(x)
                    # Remove any breaks within 7% of the edges to prevent overlap
                    buffer <- 0.07 * (max(x) - min(x))
                    filtered <- std_breaks[std_breaks > (min(x) + buffer) & 
                                           std_breaks < (max(x) - buffer)]
                    sort(unique(c(min(x), filtered, max(x))))
                },
                expand = c(0, 0)
            ) +
            ggplot2::labs(
                fill = "Acumulo de espécies"
            ) +
            ggplot2::theme_bw(base_size = 15) +
            ggplot2::theme(
                axis.text = element_text(size = 10),
                legend.text = element_text(size = 10),
                legend.position = "bottom",
                legend.title = element_text(size = 15),
                legend.justification = "center",
                plot.tag = element_text(size = 15, face = "bold"),
                panel.border = element_rect(fill = NA, color = "black"),
            ) +
            ggplot2::guides(
                fill = guide_colorbar(
                    barwidth = 15, 
                    barheight = 1.25,
                    title.position = "top",    
                    title.hjust = 0.5, 
                    frame.colour = "black",
                    ticks.colour = "black", 
                    draw.llim = TRUE,
                    draw.ulim = TRUE,
                )  
            )
        }
    )
}
