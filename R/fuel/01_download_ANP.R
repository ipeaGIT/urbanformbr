# http://www.anp.gov.br/dados-estatisticos
rm(list=ls())
library(magrittr)
library(data.table)


#### 1. Donwload raw data ---------------------------------------------------------------

# main link
#
# https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/vendas-de-derivados-de-petroleo-e-biocombustiveis

link_gasoline <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/vaehdpm/gasolina-c/vendas-anuais-de-gasolina-c-por-municipio.csv"
link_etanol <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/vaehdpm/etanol-hidratado/vendas-anuais-de-etanol-hidratado-por-municipio.csv"

`%nin%` <- Negate(`%in%`)
`%nlike%` <- Negate(`%like%`)

download.file(url = link_gasoline,
              destfile = "../../data-raw/ANP/vendas-anuais-de-gasolina-c-por-municipio.csv"
              ,mode = "wb")
download.file(url = link_etanol,
              destfile = "../../data-raw/ANP/vendas-anuais-de-etanol-hidratado-por-municipio.csv"
              ,mode = "wb")


df_etanol_raw <- fread("../../data-raw/ANP/vendas-anuais-de-gasolina-c-por-municipio.csv",encoding = "UTF-8")

names(df_etanol_raw) <- janitor::make_clean_names(names(df_etanol_raw))
df_etanol_raw
