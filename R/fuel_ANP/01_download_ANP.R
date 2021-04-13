# http://www.anp.gov.br/dados-estatisticos
library(magrittr)
library(data.table)

fixed_link <- "http://www.anp.gov.br"
`%nin%` <- Negate(`%in%`)
`%nlike%` <- Negate(`%like%`)

etanol_link <- c( paste0("/arquivos/dados-estatisticos/vendas-combustiveis/etanol-hidratado-municipio-",
                         c(2000,2017,2018),
                         ".xlsx"),
                  paste0("/arquivos/dados-estatisticos/vendas-combustiveis/etanol-hidratado-municipio-",
                         c(2001:2016),
                         ".xls"))

gasolina_link <- c(paste0("/arquivos/dados-estatisticos/vendas-combustiveis/gasolina-C-municipio-",
                          c(2000,2010,2017:2018),
                          ".xlsx"),
                   paste0("/arquivos/dados-estatisticos/vendas-combustiveis/gasolina-C-municipio-",
                          c(2001:2009,2011:2016),
                          ".xls"))

diesel_link <- c(paste0("/arquivos/dados-estatisticos/vendas-combustiveis/oleo-diesel-municipio-",
                        c(2000,2017,2018),
                        ".xlsx"),
                 paste0("/arquivos/dados-estatisticos/vendas-combustiveis/oleo-diesel-municipio-",
                        c(2001:2016),
                        ".xls"))

fuel_link <- c(etanol_link,gasolina_link,diesel_link)

for(i in 1:length(fuel_link)){

  # message
  message(fuel_link[i])

  # destination file
  tmp_destifile <- gsub(pattern = "/arquivos/dados-estatisticos/vendas-combustiveis/"
                        ,replacement = ""
                        ,x = fuel_link[i])
  # download
  download.file(url = paste0(fixed_link,fuel_link[i]),
                destfile = paste0("data-raw/anp/",tmp_destifile))

}


xlsx_files <- list.files(path = "data-raw/anp",pattern = ".xlsx",full.names = TRUE)
xslx_all <- lapply(seq_along(xlsx_files),function(i){ # i = 8
  message(i)
  dt <- openxlsx::read.xlsx(xlsxFile = xlsx_files[i],startRow = 5) %>% data.table::setDT()
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
xls_files <- list.files(path = "data-raw/anp",pattern = ".xls",full.names = TRUE)
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
