## ObervatorioPR
**Desenvolverdor:** José Ricardo Pires Adelino

O pacote de dados é uma ferramenta desenvolvida como parte do projeto `Observatório Paranaense de Espécies Exóticas` e tem por finalidade oferecer suporte aos dados disponíveis e a ferramentas de acesso para análise e evaliação dos dados disponíveis. 

A função opr_sumario permite visualizar tabelas descritivas do número por categorias de espécies exóticas. As principais categorias disponíveis são: `reino`,`classe`,`ordem`,`família`, `ecossistema`,`forma_de_vida`. Ao usar o parametro `all = TRUE` a função executa a síntese descritiva para todas categorias disponíveis automaticamente, retornanado. Caso seja de interesse a investigação de uma categoria em especifico, é possível passar o nome categoria desejada no parametro `categoria` e alterar o parametro `all = FALSE`

``` 
opr_sumario_(categoria = NULL, all = TRUE)

```
Com a função `opr_gbif`é possível baixar os dados dos registros de ocorrência da base de dados `GBIF` em seu computador. Certifique-se de que tenha um cadastro na plataforma `GBIF`. A velocidade da requisição com a API do `GBIF` dependerá da velocidade da conexão com a internet. Erros de conexão podem eventualmente interromper o processo de download. Cada especies requisitada terá os dados armazenados de forma crua no computador em formato `.zip`. O diretório de armazenamento é definido pelo parametro `dir`. É recomendado que após o download o usuário salve o objeto final `gbif_list` usando a função nativa `saveRDS`. A função retorna uma lista de dois níveis por espécies requisitada: `nível 1` permite acesso a planilha `.csv` original com o registros de ocorrência enquanto o `nível 2` permite acesso aos `metadados` da requisição, incluindo o número de registro `DOI` dos dados gerados.

```
# Exemplo de lista de espécies
spName  <- c("Panthera onca","Stigmaphyllon ellipticum")

# Itera ao longgo da lista de espécies
gbif_list <- list()
for(i in 1:length(spName)){
    gbif_list[[i]] <- 
    opr_gbif(
        taxa = spName[i],
        login = "",
        password = "",
        mail = "", 
        dir = ""
    )
    # Print progress
cat("\rFinished", i, "of", length(spName))
}

# Organiza meta dados em data frame
purrr::map(gbif_list, pluck,"Meta_table") %>% bind_rows() 

# Organiza ocorrencia em data frame
purrr::map(gbif_list, pluck,"Data") %>% bind_rows()

```

A função `opr_spLink` permite acessar os registros de ocorrência para o Paraná dentro da base de dados brasileira [!species link](https://specieslink.net) utilizando os serviços de API da plataforma. Antes de usar a função, certifique-se de requisitar seu código de acesso `(token)` ao [!serviço de API da plataforma](https://specieslink.net/ws/1.0/).O procedimento para uso da função consiste em duas etapas: `organização dos dados` e `lista de requisições`. A pimeira é acessível a partir da função `data_prep` que padroniza a nomenclatura binominal e gera a lista de caminhos pra requisição na API da plataforma. A requisição dos dados é feita pela funçao `opr_spLink` que tem como parametros a lista gerada na preparação dos dados. Para evitar planilhas muito extensas é recomendado que o parametro `lst = TRUE`, que possibilita acessar a planilha de dados separada por espécie.  

```
# Exemplo reproduzível
spList <- data_prep(    
    splist = c("Perna perna","Perna viridis","Abramites hypselonotus","Callithrix jacchus"), 
    token = "adicine seu token"
)

opr_spLink(api_list = spList, lst = TRUE)
```