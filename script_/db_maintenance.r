# Este script faz a checagem da lista de espécies com nome ajustado
# Prepara a versão para o pacote
#/media/ricardo/Backup1/Observatorio/Modulo1/files_

gc()
library(tidyverse)

sp_list <- dplyr::bind_rows(
    readRDS("data/iat_dados.rds") %>% 
        mutate(source = "IAT") %>% 
        select(ESPECIE,source) %>% 
        janitor::clean_names() %>% 
        distinct(especie,.keep_all = TRUE),
    readRDS("data/base_nac_dados.rds")%>% 
        mutate(source = "IHR") %>% 
        select(ESPECIE,source) %>% 
        janitor::clean_names() %>% 
        distinct(especie,.keep_all = TRUE),

    readRDS("data/lit_rev_dados.rds")%>% 
        mutate(source = "LR") %>% 
        select(ESPECIE,source) %>% 
        janitor::clean_names() %>% 
        distinct(especie,.keep_all = TRUE),
    readRDS("data/appa_dados.rds")%>% 
        mutate(source = "APPA") %>% 
        select(ESPECIE,source) %>% 
        janitor::clean_names()%>% 
        distinct(especie,.keep_all = TRUE),
    readRDS("data/icmbio_dados.rds")%>% 
        mutate(source = "ICMBio") %>% 
        select(ESPECIE,source) %>% 
        janitor::clean_names() %>% 
        distinct(especie,.keep_all = TRUE)
    ) %>% 
    add_row(
        especie = 
        c(
            "Monomorium spp.", 
            "Cardiocondyla spp.",
            "Bryconamericus exodon",
            "Pheidole megacephala",
            "Monomorium floricola",
            "Cardiocondyla nuda",
            "Tapinoma melanocephalum",
            "Monomorium pharaonis",
            "Paratrechina longicornis", 
            "Heniochus acuminatus"
        ), 
        source = "LR"
    ) %>%  
    mutate(
        # Corrige sinonimos
        especie = dplyr::case_when(
            #especie == "Capra hircus"~  "Capra aegagrus",                
            especie == "Oeceoclades maculata"~  "Eulophia maculata",
            especie == "Rhaphiolepis loquata"~  "Eriobotrya japonica",
            especie == "Sechium edule"~  "Sicyos edulis",
            especie == "Lithobates catesbeianus" ~  "Aquarana catesbeianus",
            especie == "Bidens tinctoria" ~  "Coreopsis tinctoria",
            especie == "Duchesnea indica" ~  "Potentilla indica",
            especie == "Sansevieria trifasciata" ~  "Dracaena trifasciata",
            especie == "Senecio madagascariensis"  ~  "Kleinia madagascariensis",
            especie == "Nandayus nenday" ~  "Aratinga nenday",
            #especie == "Bubalus bubalis" ~  "Bubalus arnee",
            especie == "Penaeus vannamei" ~  "Litopenaeus vannamei",
            especie == "Urochloa subquadripara" ~  "Urochloa distachyos",
            especie == "Christella dentata" ~  "Thelypteris dentata",
            especie == "Aleurites fordii" ~  "Vernicia fordii",
            especie == "Cinnamomum camphora" ~  "Camphora officinarum",
            especie == "Citrus limettioides" ~  "Citrus limon",
            especie == "Citrus sinensis" ~  "Citrus aurantium",
            especie == "Cupressus horizontalis" ~  "Cupressus sempervirens",
            especie == "Platanus acerifolia" ~  "Platanus hispanica",
            especie == "Rapanea ferruginea" ~  "Myrsine coriacea",
            especie == "Tabebuia alba" ~  "Handroanthus albus",
            especie == "Tabebuia avellanedae" ~  "Handroanthus impetiginosum",
            especie == "Thuja orientalis" ~  "Platycladus orientalis",
            especie == "Yuca elephantipes" ~  "Yucca gigantea",
            especie == "Dichogaster annae" ~  "Benhamia annae",
            especie == "Achatina fulica" ~  "Lissachatina fulica",
            especie == "Cupressus lusitanica" ~  "Hesperocyparis lusitanica",
            especie == "Prosopis pallida" ~  "Neltuma pallida",
            especie == "Urochloa decumbens" ~  "Urochloa eminii",
            especie == "Urochloa maxima" ~  "Megathyrsus maximus",
            especie == "Amynthas morrisi" ~  "Perichaeta morrisi",
            especie == "Metaphire californica" ~  "Pheretima californica",
            especie == "Metaphire schmardae" ~  "Megascolex schmardae", #Duplodicodrilus schmardae
            especie == "Dichogaster affinis" ~  "Benhamia affinis",
            especie == "Eukerria eiseniana" ~  "Kerria eiseniana",
            especie == "Bimastos parvus" ~  "Allolobophora parva",
            #especie == "Nematogenia lacuum" ~  "Pygmaeodrilus lacuum",
            especie == "Bidens sulphurea" ~  "Cosmos sulphureus",
            especie == "Panicum maximum" ~  "Megathyrsus maximus",
            especie == "Pennisetum purpureum" ~  "Cenchrus purpureus",
            especie == "Rhynchelytrum repens" ~  "Melinis repens",
            especie == "Rottboellia exaltata" ~  "Rottboellia cochinchinensis",
            especie == "Plectranthus barbatus" ~  "Coleus barbatus",
            especie == "Schefflera arboricola" ~  "Heptapleurum arboricola",
            especie == "Dracaena marginata" ~  "Dracaena reflexa",
            especie == "Hemigraphis alternata" ~  "Strobilanthes alternata",
            #especie= = "Hibiscus rosa-sinensis" ~  "Phragmanthera capitata",
            especie == "Dypsis lutescens" ~  "Chrysalidocarpus lutescens",
            especie == "Serissa foetida" ~  "Serissa japonica",
            especie == "Garveia franciscana" ~  "Calyptospadix cerulea",
            especie == "Myoforceps aristatus" ~  "Leiosolenus aristatus",
            especie == "Leporinus elongatus" ~  "Megaleporinus elongatus",
            especie == "Leporinus macrocephalus" ~  "Megaleporinus macrocephalus",
            especie == "Tilapia rendalli" ~  "Coptodon rendalli",
            especie == "Citrus X aurantium" ~  "Citrus aurantium",
            especie == "Citrus limonia" ~  "Citrus limon",
            especie == "Eugenia jambos" ~  "Syzygium jambos",
            especie == "Brachiaria decumbens" ~  "Urochloa eminii",
            especie == "Brachiaria humidicola" ~  "Urochloa dictyoneura",
            especie == "Botrylloides nigrum" ~ "Botrylloides niger", 
            especie == "Leporinus obtusidens" ~ "Megaleporinus obtusidens",
            especie == "Chirona amaryllis" ~ "Striatobalanus amaryllis",
            especie == "Lafoeina amirantensis" ~ "Cirrholovenia tetranema",
            especie == "Leporinus piavussu" ~ "Megaleporinus piavussu",
            especie == "Ciona intestinalis" ~ "Ciona robusta",
            TRUE ~ especie
    ), 
    # Corrige tipografia
    especie = dplyr::case_when(
        especie == "Methaphire californica"	~ "Pheretima californica",
        especie == "Saccostrea cucullata"  	~ "Saccostrea cuccullata",
        especie == "Leucaena leococephala"~ "Leucaena leucocephala",
        especie == "Ligustrum lucidun"~ "Ligustrum lucidum",
        especie == "Rhododendrun Thomsomii"~ "Rhododendron thomsonii",
        especie == "Nassarius faveolatus"  ~ "Nassarius foveolatus",
        especie == "#Apteronotus aff. albifrons"~"Apteronotus albifrons",
        #especie == "Pheretima darnleiensis"~ "Perichaeta darnleiensis",
        especie == "Dichogaster modigliani"~ "Dichogaster modiglianii",
        especie == "Lubricus rubellus" ~  "Lumbricus rubellus",
        especie == "Methaphire californica"~ "Pheretima californica",
        especie == "Sonchus oleracius" ~ "Sonchus oleraceus",
        especie == "Cotoneaster franchetti" ~ "Cotoneaster franchetii",
        especie == "Ficus benjamina variegata" ~ "Ficus benjamina",
        especie == "Cyrstostachys renda" ~ "Cyrtostachys renda",
        especie == "Ectopleura dumortieri" ~ "Ectopleura dumortierii",
        especie == "Lepus europeaus" ~ "Lepus europaeus",
        especie == "Amythas corticis" ~ "Amynthas corticis",
        especie == "Amythas gracilis" ~ "Amynthas gracilis",
        especie == "Pinus elliotti" ~ "Pinus elliottii",
        especie == "Ocnerodrilidae juveniles" ~ "Ocnerodrilidae spp.",
        especie == "Dichogaster juveniles" ~ "Dichogaster spp.",
        especie == "Acanthodrilidae juveniles" ~ "Acanthodrilidae spp.",
        especie == "Megascolecidae juveniles" ~ "Megascolecidae spp.",
        especie == "Lumbricidae juveniles" ~ "Lumbricidae spp.",
        especie == "Carya illinoensis" ~ "Carya illinoinensis",
        especie == "Coscinodiscus wailesii" ~ "Coscinodiscus wailesii",
        especie == "Tityus trivitattus" ~ "Tityus trivittatus",        
        TRUE ~ especie
        ), 
    especie = dplyr::case_when(
        especie == "Coscinodiscus sp. 1" ~ "Coscinodiscus spp.", 
        TRUE ~ especie 
        )
    ) %>%  
    dplyr::filter(
        !especie  %in%  c(
            "Pseudoplatystoma corruscans x Pseudoplatystoma reticulatum",
            "Piaractus mesopotamicus x Colossoma macropomum",
            "Magnolia x soulageana"
    )
)

tbl_wdr <- sp_list %>% 
    group_by(especie,source) %>% 
    count() %>% 
    pivot_wider(
        ., 
        names_from = source, 
        values_from = n, 
        values_fill = 0
    ) %>% 
    mutate(
        ident = case_when(
            str_detect(especie, " spp.") ~ "genero",
            str_detect(especie, "Citrus limon|Crocosmia crocosmiiflora|Citrus aurantium|Magnolia soulageana|Platanus hispanica|Viola wittrockiana") ~ "hibrido",
            str_detect(especie, "Ocnerodrilidae sp|Acanthodrilidae sp|Megascolecidae sp|Lumbricidae sp") ~ "familia",
            TRUE ~ "especie" # Default case
        ),
        .before = "especie"
    ) %>%
dplyr::rename("specie" = "especie")





