rm(list=ls())
library(magrittr)
library(data.table)
library(ggplot2)

fuel_city <- readr::read_rds("../../data/urbanformbr/fuel/fuel_cities.rds")


uca <- geobr::read_urban_concentrations() %>% setDT()
uca[, code_muni := as.character(code_muni)]

# read denatran fleet
den <- readr::read_rds("data-raw/DENATRAN/DENATRAN_jan.rds")
den[1]

### analysis 1
#
# add pop into fuel_city
fuel_city01 <- data.table::copy(fuel_city)
fuel_city01[1]
fuel_city01[den,on = c("cod_ibge" = "CODE",
                       "year" = "ANO"),pop := i.POP]

# add code_urban_concentration into fuel_city
fuel_city01[uca,on = c("cod_ibge"="code_muni"),
            code_urban_concentration := i.code_urban_concentration]

# remove NA's and keep fuel use onlt for 2018
fuel_city01 <- fuel_city01[!is.na(code_urban_concentration)]

# sum ethanol and gasoline
fuel_city01 <- fuel_city01[fuel %in% c("etanol","gasolina")]
fuel_city01[,volume := as.numeric(volume)]
fuel_city01[, `:=`(volume = sum(volume, na.rm = TRUE),
                   pop  = sum(pop, na.rm = TRUE)),
                   by = .(code_urban_concentration,year)]
fuel_city01 <- fuel_city01[,.SD[1],by = .(code_urban_concentration,year)]
fuel_city01[,fuel := "etanol+gasolina"]
fuel_city01[,`:=`(cod_ibge = NULL,municipios = NULL)]

# estimate population/fuel
fuel_city01[,`:=`(pop_fuel = volume/pop)]

## save
readr::write_rds(fuel_city01,"../../data/urbanformbr/pca_regression_df/fuel.rds",
                 compress = "gz")


