# http://www.anp.gov.br/dados-estatisticos
rm(list=ls())
library(magrittr)
library(data.table)

fixed_link <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/de/arquivos-vendas-de-derivados-de-petroleo-e-biocombustiveis/"
fixed_link1 <- "http://www.anp.gov.br/arquivos/dados-estatisticos/vendas-combustiveis/"
`%nin%` <- Negate(`%in%`)
`%nlike%` <- Negate(`%like%`)

etanol_link <- c( paste0("etanol-hidratado-municipio-",
                         c(2000,2017,2018),
                         ".xlsx"),
                  paste0("etanol-hidratado-municipio-",
                         c(2001:2016),
                         ".xls"))

gasolina_link <- c(paste0("gasolina-C-municipio-",
                          c(2000,2010,2017:2018),
                          ".xlsx"),
                   paste0("gasolina-C-municipio-",
                          c(2001:2009,2011:2016),
                          ".xls"))

diesel_link <- c(paste0("oleo-diesel-municipio-",
                        c(2000,2017,2018),
                        ".xlsx"),
                 paste0("oleo-diesel-municipio-",
                        c(2001:2016),
                        ".xls"))

fuel_link <- c(etanol_link,gasolina_link,diesel_link)

for(i in 1:length(fuel_link)){ # i = 1

  # message
  message(fuel_link[i])

  # destination file
  tmp_destifile <- gsub(pattern = "/"
                        ,replacement = ""
                        ,x = fuel_link[i])
  # download
  if(fuel_link[i] %like% "2019"){
    download.file(url = paste0(fixed_link,fuel_link[i]),
                  destfile = paste0("../../data-raw/ANP/",tmp_destifile))
  }else{
    download.file(url = paste0(fixed_link1,fuel_link[i]),
                  destfile = paste0("../../data-raw/ANP/",tmp_destifile))
  }


}


xlsx_files <- list.files(path = "../../data-raw/ANP/",pattern = ".xlsx",full.names = TRUE)
xslx_all <- lapply(seq_along(xlsx_files),function(i){ # i = 1
  message(i)
  dt <- openxlsx::read.xlsx(xlsxFile = xlsx_files[i],startRow = 5) %>% data.table::setDT()
  dt <- openxlsx::read.xlsx(xlsxFile = "../../data-raw/ANP/etanol-hidratado-municipio-2000.xlsx",startRow = 5) %>% data.table::setDT()
  colnames(dt) <- c("cod_ibge","municipios","volume")
  dt <- dt[!is.na(cod_ibge) & !is.na(municipios) & !is.na(volume)]
  # add fuel
  dt[,fuel := data.table::fcase(xlsx_files[i] %like% "etanol","etanol",
                                xlsx_files[i] %like% "gasolina","gasolina",
                                xlsx_files[i] %like% "diesel","diesel")]
  # extract year
  dt[,year := as.numeric(gsub("[^\\d]+", "", xlsx_files[i], perl=TRUE))]
  return(dt)
}) %>% data.table::rbindlist()

# XLS
xls_files <- list.files(path = "../../data-raw/ANP/",pattern = ".xls",full.names = TRUE)
xls_files <- xls_files[xls_files %nlike% "xlsx"]
xls_all <- lapply(seq_along(xlsx_files),function(i){ # i = 1
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

# merge
dt_all <- rbind(xls_all,xslx_all)

readr::write_rds(x = dt_all,file = "data/fule_cities.rds",compress = "gz")
