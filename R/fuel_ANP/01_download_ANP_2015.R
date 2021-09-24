# http://www.anp.gov.br/dados-estatisticos
rm(list=ls())
library(magrittr)
library(data.table)

fixed_link <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/de/arquivos-vendas-de-derivados-de-petroleo-e-biocombustiveis/"
fixed_link1 <- "http://www.anp.gov.br/arquivos/dados-estatisticos/vendas-combustiveis/"
`%nin%` <- Negate(`%in%`)
`%nlike%` <- Negate(`%like%`)

etanol_link <-  paste0("etanol-hidratado-municipio-2015.xls")

gasolina_link <-  paste0("gasolina-C-municipio-2015.xls")

diesel_link <- paste0("oleo-diesel-municipio-2015.xls")

fuel_link <- c(etanol_link,gasolina_link,diesel_link)

for(i in 1:length(fuel_link)){ # i = 1

  # message
  message(fuel_link[i])

  # destination file
  tmp_destifile <- gsub(pattern = "/"
                        ,replacement = ""
                        ,x = fuel_link[i])

    download.file(url = paste0(fixed_link1,fuel_link[i]),
                  destfile = paste0("../../data-raw/ANP/",tmp_destifile),mode = "wb")

}


# XLS
xls_files <- list.files(path = "../../data-raw/ANP/",pattern = ".xls",full.names = TRUE)
xls_files <- xls_files[xls_files %like% '2015']

dt_all <- lapply(seq_along(xls_files),function(i){ # i = 1

  message(i)
  dt <- readxl::read_xls(path = xls_files[i],skip = 5) %>% data.table::setDT()
  colnames(dt) <- c("cod_ibge","municipios","volume")
  dt <- dt[!is.na(cod_ibge) & !is.na(municipios) & !is.na(volume)]

  # add fuel
  dt[,fuel := data.table::fcase(xls_files[i] %like% "etanol","etanol",
                                xls_files[i] %like% "gasolina","gasolina",
                                xls_files[i] %like% "diesel","diesel")]
  # extract year
  dt[,year := as.numeric(gsub("[^\\d]+", "", xls_files[i], perl=TRUE))]
  return(dt)
}) %>% data.table::rbindlist()




readr::write_rds(x = dt_all,file = "data/fuel_cities_2015.rds",compress = "gz")
