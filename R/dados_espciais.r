#' Acesso a dados espaciais
#' 
#' Esta função permite fazer o download de camadas espacias relevantes para analise dos dados
#' 

opree_spat <- function(url, nome_shp = NULL) {
  # Cria diretório temporário único
  temp_dir <- tempfile(pattern = "shp_download_", tmpdir = tempdir())
  dir.create(temp_dir, showWarnings = FALSE)
  
  # Caminho do ZIP
  zip_path <- file.path(temp_dir, "shapefile.zip")
  
  # Download do ZIP
  cat("Baixando arquivo...\n")
  download.file(url, zip_path, mode = "wb", quiet = TRUE)
  
  # Extrai arquivos
  cat("Extraindo shapefile...\n")
  unzip(zip_path, exdir = temp_dir, junkpaths = TRUE)
  
  # Lista arquivos .shp
  shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  if (length(shp_files) == 0) {
    unlink(temp_dir, recursive = TRUE, force = TRUE)
    stop("Nenhum arquivo .shp encontrado no ZIP!")
  }
  
  # Seleciona shapefile específico ou primeiro disponível
  if (!is.null(nome_shp)) {
    shp_file <- shp_files[stringr::str_detect(shp_files, nome_shp)]
    if (length(shp_file) == 0) shp_file <- shp_files[1]
  } else {
    shp_file <- shp_files[1]
  }
  
  # Carrega shapefile
  cat("Carregando shapefile:", basename(shp_file), "\n")
  shp <- sf::st_read(shp_file, quiet = TRUE)
  
  # REMOVE TUDO do diretório temporário
  cat("Limpando arquivos temporários...\n")
  unlink(temp_dir, recursive = TRUE, force = TRUE)
  
  cat("Sucesso! Shapefile carregado com", nrow(shp), "features.\n")
  return(shp)
}
