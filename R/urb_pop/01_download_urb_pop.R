#
# libraries -------------
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
library("geobr")


mapview::mapviewOptions(platform = 'mapdeck')

options(mc.cores=20)

#
# Tabela 6579 - População residente estimada
# Tabela 202 - População residente, por sexo e situação do domicílio
#

info2020 <- sidrar::info_sidra(x = 202)



# download CENSUS--------------------

pop202 <- sidrar::get_sidra(x = 202
                            , variable = 93
                            , period = as.character(c(1970, 1980, 1991, 2000, 2010))
                            #, geo = "Brazil"
                            #, geo.filter = "City"

)
# fix names
data.table::setDT(pop202)
names(pop202) <- janitor::make_clean_names(names(pop202))

readr::write_rds(pop202,"data/table_202_ibge.rds",compress="gz")


# download population projection---------

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


# download and expand comparable areas----------

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


# read CENSOS-------

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

# save pop CENSO------

readr::write_rds(pop,"data/population_muni_ibge.rds",compress = "gz")

## download PNAD-----

# Downloading data
pnadc.df2 <- PNADcIBGE::get_pnadc(year=2020,
                                  quarter=4,
                                  vars="V1022",
                                  defyear=2020,
                                  defperiod=4,
                                  labels=TRUE,
                                  deflator=TRUE,
                                  design=FALSE,
                                  savedir=tempdir())

pnadc.svy2 <- PNADcIBGE::pnadc_design(data_pnadc=pnadc.df2)
# Calculating unemployment rate
pnadc.svy2_rate <- survey::svymean(x=~V1022, design=pnadc.svy2, na.rm=TRUE)

pnadc.svy2_dt <- data.table::as.data.table(pnadc.svy2_rate)
pnadc.svy2_dt[,V1022 := c(1:2)]
pnadc.svy2_dt[,SE :=NULL]

# save PNAD----
readr::write_rds(pnadc.svy2_dt,"data/pnadc.rds",compress = "gz")
