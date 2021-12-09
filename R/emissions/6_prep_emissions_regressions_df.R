# prep data
# Import packages ----

rm(list=ls())
library(ggplot2)
library(magrittr)
library(data.table)
library(patchwork)

# Read data ------
message("Reading data \n")
message("---------------------")

df_moto_raw <-  readr::read_rds("../../data/urbanformbr/emissions/motocycle_emissions.rds")
df_carro_raw <- readr::read_rds("../../data/urbanformbr/emissions/passanger_cars_emissions.rds")

uca <- geobr::read_urban_concentrations(simplified = TRUE) %>%
  data.table::setDT() %>%
  .[,code_muni := as.character(code_muni)]

geobr_regions <- geobr::read_state() %>% data.table::setDT()

df_energy <- readr::read_rds("../../data/urbanformbr/pca_regression_df/energy_per_capita_2010.rds")

df_pop <- readr::read_rds("../../data/urbanformbr/pca_regression_df/fleet_and_pop.rds")
df_pop <- df_pop[ANO == 2010,.SD,.SDcols = c("code_urban_concentration","POP")]
data.table::setnames(df_pop,"POP","pop")


# Prep -----
# * Moto -----
df_moto_prep <- data.table::copy(df_moto_raw)
colPols <- names(df_moto_prep)[names(df_moto_prep) %like% "^CO2"]
df_moto_prep <- df_moto_prep[,.SD
                             ,.SDcols = c("code_urban_concentration"
                                          ,"classe","age_adj",colPols)]

df_moto_prep <- melt(df_moto_prep, measure= colPols
                     ,value.name = "Emissions",variable.name = "type_vehicle")
df_moto_prep[,type_vehicle := gsub("CO2_","",type_vehicle)]
# * Carro -----
df_carro_prep <- data.table::copy(df_carro_raw)
colPols <- names(df_carro_prep)[names(df_carro_prep) %like% "^CO2"]
df_carro_prep <- df_carro_prep[,.SD
                               ,.SDcols = c("code_urban_concentration"
                                            ,"classe","age_adj",colPols)]
df_carro_prep <- melt(df_carro_prep, measure= colPols
                      ,value.name = "Emissions",variable.name = "type_vehicle")
df_carro_prep[,type_vehicle := gsub("CO2_","",type_vehicle)]


# * Bind -----

dt_bind <- rbind(df_carro_prep,df_moto_prep)




# * processing -----

dt_pol <- data.table::copy(dt_bind)[,sum(Emissions),by = .(code_urban_concentration)]
data.table::setnames(dt_pol,"V1","Emissions")
dt_pol[,Emissions := units::set_units(Emissions,"t")]

dt_pol[df_pop,on = "code_urban_concentration",pop := i.pop]
dt_pol[df_energy,on = "code_urban_concentration",tep := i.tep]
dt_pol[,emissions_capita := as.numeric(Emissions) / pop]

# remove columns
dt_pol <- dt_pol[,.SD,.SDcols = c("code_urban_concentration","emissions_capita")]

# * write emissions per capita ------

readr::write_rds(x = dt_pol,"../../data/urbanformbr/pca_regression_df/co2_per_capita2010.rds")
