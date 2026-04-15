#' @title Vulnerability index 
#' @description This index uses colonization pressure by kilometer square of spatial area to map the accumulation of non-native species for any spatial file collection
#' @param data_ `data.frame` with occurrence records, species name or species groups
#' @param long `string` longitude name in data frame
#' @param lat `string` latitude name in data frame
#' @param shp_ `spatVec` geometies in `spatVec` class
#' @param shape_var `string` variable of interest inside geometry in `shp_`
#' @param data_var `string` variable name with grupo of interest (i.e. species) em `data_`
#' @param area `numeric` conversion unity scale. Default is 1e6  
#' 
#' @return `data frame` containing the estimated indicator value, classification rank, and relative percentage for each polygon existing in the `geom` parameter
#' @import dplyr
#' @import terra
#' @import tidyr
#' @import tidyterra 
#' @examples 
#' \dontrun{
#' vt <- db_splist %>% filter(eco_evo_class == "terrestrial_vertebrate") %>% pull(especie)
#' sp <- db_oco %>% filter(especie_ajustado %in% vt)
#' municipios <- geometrias %>% filter(class == "IAT") %>% vect()
#' ecor <- geometrias %>% filter(class == "IBGE") %>% vect()
#' ind <- opree_cp(
#'    data_ = sp, 
#'    long = "long_dec", 
#'    lat = "lat_dec", 
#'    shp_ = ecor,
#'    shape_var = "nome", 
#'    data_var = "especie_ajustado"
#')
#'} 
#' @export 
opree_cp <- function(data_, long, lat, shp_ ,shape_var, data_var, area = 1e6){
  if(inherits(shp_, "sf")) {
    stop("geometria na classe sf, ajuste para terra usando terra::vect()")
  }
  if(!inherits(data_, "terra")) {
    dados <- terra::vect(data_, geom = c(long, lat), crs = "EPSG:4326")
  }
  print('Alinhando dados espacias, isso pode levar alguns minutos!')
  pts_muni <- terra::intersect(dados, shp_[shape_var])
  pts_muni <- tidyterra::as_tibble(pts_muni)
  rich <- pts_muni %>%
    dplyr::filter(!is.na(.data[[shape_var]])) %>%
    dplyr::distinct(.data[[data_var]], .data[[shape_var]]) %>% 
    dplyr::count(.data[[shape_var]], name = "N") 
  areas <- terra::expanse(shp_) / area #Vetor
  mun_areas <- tibble::tibble(shp_[[shape_var]], area_km2 = as.numeric(areas))   
  idx <-  dplyr::left_join(rich, mun_areas)
  idx <- idx %>% 
    dplyr::mutate(
      ind = N / area_km2, # espécies por 1000km²
      rank = rank(desc(ind)), 
      perc = percent_rank(ind)
  )
  return(idx)
}
