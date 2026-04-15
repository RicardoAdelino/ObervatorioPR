#' @title Plot grid maps derived from opree data
#' @description This function uses occurrence records to estimate the spatial extent of records using grid custom grid cells 
#' @param data_`data.frame` generated from `opree_spat_grid` function
#' @param lgd_break `string` scale of color values. Default is 8
#' @param name_ `string` plot title
#' @param pallete_ `string` any color sequence derived from `RColorBrewer`
#' @param scale_label = "string" Set the title of color range palete
#' @import dplyr
#' @import ggplot2
#' @import sf 
#' @import RColorBrewer
#' @examples 
#' \dontrun{
#' vt <- db_splist %>% filter(eco_evo_class == "terrestrial_vertebrate") %>% pull(species)
#' sp <- db_oco %>% filter(species_adj %in% vt) 
#' pr_ <- geometrias %>% filter(class == "OpenStreeMap") %>% vect()
#' teste <- opree_spat_grid(
#'    data_ = sp, 
#'    shape_ = pr_, 
#'    long_ = "long_dec", 
#'    lat_ = "lat_dec",
#'    area = 100, 
#'    hex_ = FALSE
#')
#'
#'opree_map(teste, lgd_break = 8)
#'}
#' @export 
opree_map <- function(data_ = NULL, pallete_ = "Spectral", lgd_break = 8, scale_label = ""){
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
                fill = scale_label
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
