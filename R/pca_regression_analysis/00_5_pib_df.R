#
# prep DENATRAN fleet
#
rm(list=ls())
library(readr)
library(data.table)
library(geobr)
library(magrittr)
library(sidrar)

#### read ----
# read urban concentration
uca <- geobr::read_urban_concentrations() %>%
  data.table::setDT()
uca[,code_muni := as.character(code_muni)]

# read denatran fleet
den <- readr::read_rds("../../data/urbanformbr/pca_regression_df/fleet_and_pop.rds")

# download PIB sidrar
# https://sidra.ibge.gov.br/Tabela/5938
sidrar::info_sidra(x = 5938)
pib1 <- sidrar::get_sidra(x = 5938
                              , variable = c(37) #c(37,513,517,6575,525)
                              , period = as.character(c(2002, 2003, 2004, 2005,
                                                        2006, 2007, 2008, 2009))
                              , geo = "City")
pib2 <- sidrar::get_sidra(x = 5938
                          , variable = c(37) #c(37,513,517,6575,525)
                          , period = as.character(c(2010, 2011,2012, 2013,
                                                    2014, 2015, 2016))
                          , geo = "City")
pib3 <- sidrar::get_sidra(x = 5938
                          , variable = c(37) #c(37,513,517,6575,525)
                          , period = as.character(c(2017,2018))
                          , geo = "City")
# 1,19,26,33,40

# rbindlist
pib_all <- data.table::rbindlist(list(pib1,pib2,pib3)) %>% setDT()
names(pib_all) <- janitor::make_clean_names(names(pib_all))

# add uca
pib_all[1]
uca[1]
pib_all[uca,on = c("municipio_codigo" = "code_muni"),
        code_urban_concentration := i.code_urban_concentration]

# remove empty urban concentration
pib_all <- pib_all[!is.na(code_urban_concentration)]

# calcula pib por code_urban_concentration
pib_all_df <- data.table::copy(pib_all)[, lapply(.SD,sum,na.rm=TRUE),
                                          by = .(code_urban_concentration,
                                                 ano),
                                          .SDcols = "valor"]

pib_all_df <- pib_all_df[!is.na(valor) & valor != 0]


# add pop
pib_all_df[1]
den[,ANO := as.character(ANO)]

pib_all_df[den,on = c("code_urban_concentration",
                      "ano" = "ANO"),pop := POP]

# pib_capita
pib_all_df[,pib_capita := pop/valor]

## save
readr::write_rds(pib_all_df,"../../data/urbanformbr/pca_regression_df/pib.rds",
                 compress = "gz")
