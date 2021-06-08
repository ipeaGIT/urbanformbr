#
# load packages
#
rm(list=ls())
library(ggrepel)
source('R/colours.R')
source("R/setup.R")
source("R/urb_pop/colors_plot.R")
source("R/style.R")
source("R/urb_pop/aop_style1.R")
#devtools::install_github("rpradosiqueira/sidrar")
library("sidrar")
library(PNADcIBGE)
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library("geobr")
#remove.packages("geobr")
mapview::mapviewOptions(platform = 'mapdeck')
options(mc.cores=20)

## IBGE data -------

# Tabela 6579 - População residente estimada
# Tabela 202 - População residente, por sexo e situação do domicílio
#
info2020 <- sidrar::info_sidra(x = 202)

pop202 <- sidrar::get_sidra(x = 202
                            , variable = 93
                            , period = as.character(c(1970, 1980, 1991, 2000, 2010))
                            #, geo = "Brazil"
                            #, geo.filter = "City"

)

# fix names
data.table::setDT(pop202)
names(pop202) <- janitor::make_clean_names(names(pop202))

# write output
readr::write_rds(pop202,"data/table_202_ibge.rds",compress="gz")

## PNAC download
# Downloading data
pnadc.df2 <- PNADcIBGE::get_pnadc(year=2020,
                                  quarter=4,
                                  vars="V1022",
                                  defyear=2020,
                                  defperiod=4,
                                  labels=FALSE,
                                  deflator=FALSE,
                                  design=FALSE,
                                  savedir=tempdir())
year=2019;
quarter=4;
vars="V1022";
defyear=2019;
defperiod=4;
labels=FALSE;
deflator=FALSE;
design=FALSE;
interview = NULL; topic = NULL
savedir=tempdir()
link_name = "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/2019/PNADC_042019.zip"
dataname = "PNADC_042019"
link_dici = "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Dicionario_e_input_20201202.zip"
link_def = "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Deflatores.zip"

#dataname
source("R/urb_pop/download_pnad.R")
pnadc.df2 <- my_getpnadc(year = 2019,
                         link_name = link_name,
                         dataname = dataname,
                         link_dici = link_dici,
                         link_def = link_def,
                         quarter=4,
                         interview = NULL,
                         topic = NULL,
                         vars = "V1022",
                         defyear = 2019,
                         defperiod = 4,
                         labels = TRUE,
                         deflator = TRUE,
                         design = TRUE,
                         savedir = tempdir())

pnadc.df3 <- pnadc.df2$variables

data.table::setDT(pnadc.df3)

pnadc.df3[,.SD[1],by = c("Trimestre","Estrato","V1022","V1027","V1029")]

pnadc.df3[,situacao := data.table::fifelse(V1022 == "Urbana",1,0)]

pnadc.df3[,situacao := sum(situacao) / .N, by = UF]
pnadc.df3[,.SD[1], by = UF]

pnadc.df1 <- PNADcIBGE::read_pnadc(microdata = "data-raw/PNADC_042020.txt",
                                   input_txt = "data-raw/dicionario_PNADC_microdados_trimestral.xls",
                                   vars = "V1022")

input_pnad <- "data-raw/PNADC_042020.txt"
input_txt = "data-raw/dicionario_PNADC_microdados_trimestral.xls"
pnad <- readr::read_table2(input_pnad, col_names = FALSE)

pnadf <- pnad %>% subset(X2 %in% c("Ano", "Trimestre", "UF"
                                   , "UPA", "Estrato", "V1008", "V1027", "V1029", "V1030",
                                   "V1031", "posest", "V2003", vars))

pnadf
X1 = X2 = X3 = start = end = NULL
input <- suppressWarnings(suppressMessages({
  readr::read_table2(file = input_pnad, col_names = FALSE) %>%
    subset(substr(X1, 1, 1) == "@") %>% dplyr::mutate(type = ifelse(substr(X3,
                                                                           1, 1) == "$", "c", "d"), start = as.numeric(gsub("@",
                                                                                                                            "", X1)), X3 = as.integer(chartr("$", " ", X3)),
                                                      end = start + X3 - 1)
}))
input <- input %>% subset(X2 %in% c("Ano", "Trimestre", "UF",
                                    "UPA", "Estrato", "V1008", "V1027", "V1029", "V1030",
                                    "V1031", "posest", "V2003", "V1022"))

columns <- input %$% readr::fwf_positions(start, end, X2)
input <- readr::read_table2(input_txt, col_names = FALSE) %>%
  subset(substr(X1, 1, 1) == "@") %>% dplyr::mutate(type = ifelse(substr(X3,
                                                                         1, 1) == "$", "c", "d"), start = as.numeric(gsub("@",
                                                                                                                          "", X1)), X3 = as.integer(chartr("$", " ", X3)),
                                                    end = start + X3 - 1)


dictionary.path2 <- pnadc_example(path="dictionaryexample.xls")
pnadc.df2 <- pnadc_labeller(data_pnadc=pnadc.df2, dictionary.file=dictionary.path2)

# ###
# download population projection

sidrar::info_sidra(x = 6579)
pop_6579 <- sidrar::get_sidra(x = 6579
                              , variable = 9324
                              , period = as.character(c(2020))
                              , geo = "City"
                              #, geo.filter = "City"
)
# fix names
data.table::setDT(pop_6579)
names(pop_6579) <- janitor::make_clean_names(names(pop_6579))

readr::write_rds(pop_6579,"data/table_6579_ibge.rds",compress="gz")


# download and expand comparable areas

comp_areas <- geobr::read_comparable_areas(start_year=1970,
                                           end_year=2010,
                                           simplified = FALSE)
data.table::setDT(comp_areas)

ca_exp <- lapply(1:nrow(comp_areas),function(i){
  code_muni_2010 <- stringr::str_split(comp_areas$list_code_muni_2010[i],",",
                                       simplify = TRUE) %>% as.vector()
  dt <- data.table::data.table(comp_areas[i,.(code_amc,geom)],code_muni_2010)
  return(dt)
}) %>% data.table::rbindlist()

readr::write_rds(ca_exp,"data/comparable_areas_ibge.rds",compress = "gz")


# read pop

my_years <- as.character(c(1970, 1980, 1991, 2000, 2010))
pop <- lapply(my_years,function(i){
  tmp_pop <-  sidrar::get_sidra(x = 202
                                , variable = 93
                                , period = i
                                , geo = "City"
                                , classific = c("c1"))
  data.table::setDT(tmp_pop)
  tmp_pop <- tmp_pop[Sexo == "Total" & `Situação do domicílio` == "Total"]
  return(tmp_pop)
}) %>% data.table::rbindlist()
names(pop) <- janitor::make_clean_names(names(pop))


readr::write_rds(pop,"data/population_muni_ibge.rds",compress = "gz")

