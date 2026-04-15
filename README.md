Language: [English](README.md) | [Portuguese](README_PTBr.md)
<!-- header -->
# opree:Obervatorio Paranaense de Espécies Exóticas <a href=""><img src="man/figures/logo.png" align="right" height="159" alt="" /></a> 

Opree is a collaborative, continuous-flow project designed to aggregate biological and geographic data for exotic species in the state of Paraná. Toghether with the support of the state's network of experts on exotic and invasive species, **opree** integrates and stores information originating from public and private databases to form the foundation of the **Paraná Observatory of Exotic Species**.

## Package structure
The database contains approximately **25,000** records of occurrences of non-native species collected for the state of Paraná.
The database uses information from multipĺe research projects, studies and scientific literature, specialized databases, and ongoing research projects to compile geospatial observation data of exotic species.
The organization level of collecting information is reported in the flowchart:

```mermaid
%%{init: {'themeVariables': { 'fontSize': '24px'}}}%%
flowchart TD
    %% Parte 1: levantamento preliminar de dados
    a@{shape: rounded, label: "Researcher Institurions and experts"} -.-> A@{shape: processes, label: "Preliminary survey of species"}

    b@{shape: rounded, label: "Contact with protection areas managers"}-.-> A@{shape: processes, label = "Preliminary survey of species"}
    
    c@{shape: rounded, label: "Contact with the state-owned company in Paranaguá"} -.-> A@{shape: processes, label = "Preliminary survey of species"}
    
    d@{shape: rounded, label: "Public available data bases"} -.-> A@{shape: processes, label = "Preliminary survey of species"}

    %% Parte 2: Suplementação por revisão sistemática
    rsis@{shape: rounded, label: "Literature review"} -.- |"Web Of Science dB"| B@{shape: processes, label: "Supplement of the preliminary list of EEIs"}

    A@{shape: processes, label = "Preliminary survey of species"} --> B@{shape: processes, label: "Supplement of the preliminary survey of species"}

    rsis2@{shape: rounded, label: "Recommendation"} -.- |"Expertise"| B@{shape: processes, label: "Supplement of the preliminary survey of species"}

    taxa@{shape: rounded, label: "Taxonomic \n Review"} -.- C@{shape: processes, label: "Data refinement"}
     
    taxa@{shape: rounded, label: "Taxonomic \n Review"}-- "Specialized databases" <--> db@{shape: rounded, label: "IUCN </br> WORMS </br> NEMESIS </br> KEOW </br> FshBase </br> Catalog of Fishes </br> GBIF"}
        
    style db text-align:left

    B@{shape: processes, label: "Supplement of the preliminary survey of species"}-- "Starting point" --> C@{shape: processes, label: "Refinamento dos dados"}

    status@{shape: rounded, label: "Assessment of species status classification"} -.- C@{shape: processes, label: "Data refinement"}

    %%oc@{shape:rounded,label:"Coleta dos registros de ocorrência"} --> D@{shape: database, label: "Base de dados atualizada"}

    subgraph spat["<span style='font-size:27px'> Data compilation </span>" ]
        geocode(Indirect inputs - geocode) -.- D@{shape: database, label: "OPrEE"}  
        C@{shape: processes, label: "Data refinement"} -.- D@{shape: database, label: "OPrEE dB"}  
        dbc(Published data base collections) -.- D@{shape: database, label: "OPrEE"}
        pesq(Wildlife survey data) -.- D@{shape: database, label: "OPrEE"}
    end
    
    D@{shape: database, label: "OPrEE"} --> D2@{shape: braces, label: "Quantitative synthesis and <br/> tools"}

    D2@{shape: braces, label: "Quantitative synthesis and <br/> tools"} -.-> app@{shape: ,label: "R package"}

    D2@{shape: braces, label: "Quantitative synthesis and <br/> tools"} -.-> app2@{shape: ,label: "Interactive dashboard"}

    D2@{shape: braces, label: "Quantitative synthesis and <br/> tools"} -.-> app3@{shape: ,label: "Species Distribution Models"}

    app@{shape: ,label: "R package"} -.-> E@{shape: braces, label: "Executive Summary"}
    app2@{shape: ,label: "Intereactive dashboard"}-.->E@{shape: braces, label: "Executive Summary"}
    app3@{shape: ,label: "SDMs"}-.->E@{shape: braces, label: "Executive Summary"}
 
    
    classDef largeNode font-size:28px;
    class A largeNode;
    class B largeNode;
    class C largeNode;
    class D largeNode;
    class D2 largeNode;
    class E largeNode;
    class app largeNode;
    class app2 largeNode;
    class app3 largeNode;
```

## Package Features

### Installation
Although the package is hosted in the `ObservatorioPR` repository, the package name is `opree`. Therefore, after installing via the repository, it is necessary call the `opree` library.

```r
#install depencies
install.packages(devtools)
#install package
devtools::install_github("RicardoAdelino/ObervatorioPR")
#load package 
library(opree)
```