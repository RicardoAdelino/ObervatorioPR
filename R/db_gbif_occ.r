#' @title Download occurrences from GBIF
#' @description This function allows you to download GBIF occurrence records in `.csv` format for one or multiple species.
#' The function is adjusted to search for records other than: `uncertainty greater than 1km of precision`, `null geographic coordinates (i.e., 0)`, `invalid coordinates`, `coordinates with error`, `records of dead specimens`, `zoos and botanical gardens`.
#' In this way, the function returns only records that refer to: `preserved specimens`, `cited material`, `human observation`, `machine records (i.e., photography, filming)`  
#' @param taxa `vector` speceis binomial (i.e., c("Lepus saxatilis","Puma concolor"))
#' @param login `string` personal login used to log in GBIF
#' @param password `string` personal password used to log in GBIF
#' @param mail `string` personal email used to log in GBIF
#' @param dir `string` path to store downloaded file
#' @return List of occurrence data and metadata. List element `Data` store occurrences in `csv` format while `Meta_data` store summary metadata including occurrence `DOI`.
#' @import rgbif 
#' @import tibble
#' @export 
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
    rgbif::pred("taxonKey", tk),
    # Subset extent of interest
    rgbif::pred("country","BR"),
    # Control geospatial errors (Zero Coordinates, Country coordinate mis-match, Coordinate invalid, Coordinate invalid)
    # More details in (https://docs.gbif.org/course-data-use/en/geospatial-filters-issues.html)
    rgbif::pred("hasGeospatialIssue", FALSE),
    # Only data with occurrences
    rgbif::pred("hasCoordinate", TRUE),
    # Only status classfied as present
    rgbif::pred("occurrenceStatus","PRESENT"),
    # No use Fossil data (no live) and Living species (zoo and botanical gardens)
    # Retain only, Preserved Specimen, Material Citation, Human Observation, Machine Observation 
    rgbif::pred_not(
        rgbif::pred_in(
            "basisOfRecord",
            c("FOSSIL_SPECIMEN","LIVING_SPECIMEN")
            )
        ),
    # Control uncertainty distance in meters
    rgbif::pred_or(  
        rgbif::pred_lt("coordinateUncertaintyInMeters",10000),
        rgbif::pred_isnull("coordinateUncertaintyInMeters")
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

