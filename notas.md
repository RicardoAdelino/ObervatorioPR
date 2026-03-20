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
