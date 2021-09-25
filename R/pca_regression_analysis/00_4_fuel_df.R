rm(list=ls())
library(magrittr)
library(data.table)
library(ggplot2)


### read files ----
# fuel
fuel_city <- readr::read_rds("../../data/urbanformbr/fuel/fuel_cities.rds")


# urban concentration
uca <- geobr::read_urban_concentrations() %>% setDT()
uca[, code_muni := as.character(code_muni)]



# add pop into fuel_city----------
fuel_city01 <- data.table::copy(fuel_city)
fuel_city01[1]


# add code_urban_concentration into fuel_city
fuel_city01[uca, on = c("cod_ibge"="code_muni"),
            code_urban_concentration := i.code_urban_concentration]


# Keep only cities in urban concentration areas
fuel_city01 <- fuel_city01[!is.na(code_urban_concentration)]

# keep data for 2010 (same as other covariates)
fuel_city01 <- subset(fuel_city01, year==2010)

# sum ethanol and gasoline
fuel_city01 <- fuel_city01[fuel %in% c("etanol","gasolina")]
fuel_city01[,volume := as.numeric(volume)]

fuel_city01 <- fuel_city01[, .(fuel_consumption_total = sum(volume),
                               fuel = paste0(unique(fuel), collapse = '+')),
                           by=.(year, code_urban_concentration)]

head(fuel_city01)
# fuel_city01[, `:=`(volume = sum(volume, na.rm = TRUE),
#                    fuel = paste0(fuel, collapse = '+')
#                    # , pop  = sum(pop, na.rm = TRUE)
#                    ),
#                    by = .(code_urban_concentration,year)]
#
# fuel_city01 <- fuel_city01[,.SD[1],by = .(code_urban_concentration,year)]
# fuel_city01[,fuel := "etanol+gasolina"]
# fuel_city01[,`:=`(cod_ibge = NULL,municipios = NULL)]
#
# # estimate population/fuel
# fuel_city01[,`:=`(fuel_consumption_per_capita  = volume/pop)]
#
#
# # remove "Inf"
# # this happens for year 2001
# fuel_city01 <- fuel_city01[fuel_consumption_per_capita != "Inf"]


## save
readr::write_rds(fuel_city01,"../../data/urbanformbr/pca_regression_df/fuel.rds",
                 compress = "gz")


