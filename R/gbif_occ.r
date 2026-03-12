#' Registros de ocorrência do GBIF
#'
#' Esta função permite fazer o download dos registros de ocorrência do GBIF em formato `.csv` para uma ou múltiplas espécies.
#' A função esta ajustada pra buscar registros diferentes de: `incerteza maior que 1km de precisão`,`coordenadas geográficos nulas (i.e., 0)`, `coordenadas invalidas`, `coordenadas com erro`,`registros de espécimens mortos`,`zoológicos e jardins botânicos`. 
#' Desta maneira a função retona apenas registros que se referem a: `especimens preservados`,`material citado`,`observação humana`,`registros de maquinas (i.e., fotografia, filmagens)` 
#'    
#' @param taxa `vetor` com a nomenclatura biniminal das espécies (i.e., c("Lepus saxatilis","Puma concolor"))
#' @param login `string` com o nome do usuário registrado na plataforma do GBIF
#' @param password `string` com a senha do usuário registrado na plataforma do GBIF
#' @param mail `string` com o email do usuario registrado na plataforma do GBIF
#' @param dir `string` com o diretorio de destino para o download dos dados de ocorrência
#' 
#' @return lista de dados e metadados. O elemento de lista de nome `Data` retorna os registros de ocorrência compilados em formato `csv`, enquanto o elemento de lista `Meta_data` retorna uma table de resumo dos dados incluindo o número de registro (DOI) da planilha gerada.
#' 
#' @examples 
#' NOT RUN
#' # Lista de espécies
#' spName  <- c("Lepus saxatilis","Puma concolor")
#' 
#' # Itera ao longgo da lista de espécies
#' gbif_list <- list()
#' 
#' # Download dos dados
#' for(i in 1:length(spName)){
#'    gbif_list[[i]] <- 
#'    opr_gbif(
#'        taxa = spName[i],
#'        login = "",
#'        password = "",
#'        mail = "", 
#'        dir = ""
#'    )
#'    # Print progress
#'cat("\rFinished", i, "of", length(spName))
#' 
#' # Organiza meta dados em data frame
#' purrr::map(gbif_list, pluck,"Meta_table") %>% bind_rows() 
#'
#' # Organiza ocorrencia em data frame
#' purrr::map(gbif_list, pluck,"Data") %>% bind_rows()

opree_gbif <- function(taxa,login, password, mail,dir){
    # Get Taxon key
    tk <- rgbif::name_backbone(taxa)$usageKey
    # Data requirements
    get_data <- rgbif::occ_download(
    # Personal login
    user = login,
    # Password 
    pwd = password,
    # Email adress
    email = mail,
    # Taxon Key 
    pred("taxonKey", tk),
    # Subset extent of interest
    pred("country","BR"),
    # Control geospatial errors (Zero Coordinates, Country coordinate mis-match, Coordinate invalid, Coordinate invalid)
    # More details in (https://docs.gbif.org/course-data-use/en/geospatial-filters-issues.html)
    pred("hasGeospatialIssue", FALSE),
    # Only data with occurrences
        pred("hasCoordinate", TRUE),
    # Only status classfied as present
    pred("occurrenceStatus","PRESENT"),
    # No use Fossil data (no live) and Living species (zoo and botanical gardens)
    # Retain only, Preserved Specimen, Material Citation, Human Observation, Machine Observation 
    pred_not(
        pred_in(
            "basisOfRecord",
            c("FOSSIL_SPECIMEN","LIVING_SPECIMEN")
            )
        ),
    # Control uncertainty distance in meters
    pred_or(  
        pred_lt("coordinateUncertaintyInMeters",10000),
        pred_isnull("coordinateUncertaintyInMeters")
    ),
    # Output format
    format = "SIMPLE_CSV"
    )

    #wait request acceptance and data subseting
    get_data %>% rgbif::occ_download_wait()

    # Load dataset
    compile_data <- get_data %>% 
        rgbif::occ_download_get(., path = dir) %>% 
        rgbif::occ_download_import()

    # Create metada forreference report
    meta <- get_data %>% rgbif::occ_download_wait()

    # Table of dataset parameters
    meta_table <- tibble::tibble(
        TaxonKey = tk,
        Taxa = taxa,
        DataReq = attr(get_data,"created"),
        #attr(pd,"downloadLink"),
        Nocc = meta[["totalRecords"]],
        Ndataset = meta[["numberDatasets"]],
        DOI = attr(get_data,"doi"),
        Citation = attr(get_data,"citation"),   
    ) 
    
    # Output
    return(
        list(
            Data = compile_data,    
            Meta_table = meta_table
        )
    )
}

