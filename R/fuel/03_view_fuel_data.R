rm(list=ls())
library(magrittr)
library(data.table)
library(ggplot2)

fuel_city <- readr::read_rds("../../data/urbanformbr/fuel/fuel_cities.rds")


dens_city <- readr::read_rds("../../data/urbanformbr/density-experienced/density-experienced_urban-concentrations.rds")
data.table::setDT(dens_city)
dens_city[,code_muni := as.character(code_muni)]


pop <- readr::read_rds("../../data/urbanformbr/population/table_6579_ibge.rds")


### analysis 1
#
# add pop into fuel_city
fuel_city01 <- data.table::copy(fuel_city)
fuel_city01[pop,on = c("cod_ibge" = "municipio_codigo"),
          pop := i.valor ]

# add code_urban_concentration into fuel_city
fuel_city01[dens_city,on = c("cod_ibge"="code_muni"),
          code_urban_concentration := i.code_urban_concentration]

# remove NA's and keep fuel use onlt for 2018
fuel_city01 <- fuel_city01[!is.na(code_urban_concentration) &
                         year == 2018]

# sum ethanol and gasoline
fuel_city01 <- data.table::copy(fuel_city01)[fuel %in% c("etanol","gasolina")]
fuel_city01[,volume := as.numeric(volume)]
fuel_city01 <- fuel_city01[, volume := sum(volume),by = .(cod_ibge)]
fuel_city01 <- fuel_city01[,.SD[1],by = cod_ibge]
fuel_city01[,fuel := "etanol+gasolina"]
fuel_city01[,`:=`(pop = sum(pop),
                  volume = sum(volume)),by = .(fuel,code_urban_concentration)]
fuel_city01 <- fuel_city01[,.SD[1],by = .(fuel,code_urban_concentration)]

# estimate population/fuel
fuel_city01 <- fuel_city01[,.(pop = sum(pop, na.rm = TRUE),
               volume = sum(volume, na.rm = TRUE)),
            by = .(fuel,code_urban_concentration)]


# estimate density by code_urban_concentration
temp_denscity <- data.table::copy(dens_city)[,.(
  weighted_density10km2 = weighted.mean(x=pop_density10km2, w=pop_10km),
  weighted_density05km2 = weighted.mean(x=pop_density05km2, w=pop_05km),
  weighted_density01km2 = weighted.mean(x=pop_density01km2, w=pop_01km)),
  by=code_urban_concentration ]


# add density into fuel consumption
fuel_city01 <- fuel_city01[temp_denscity, on = "code_urban_concentration"]

# estimate population/fuel

# # fuel per pop
# fuel_city01[,fuel_per_pop := volume/pop]
# # add state info
# state_info <- geobr::read_municipality() %>% data.table::setDT()
# state_info[,code_muni := as.character(code_muni)]
# fuel_city01[state_info, on = c("cod_ibge"="code_muni"),
#             abbrev_state := i.abbrev_state]
#
# # add regions
# regions_info <- geobr::read_state() %>% data.table::setDT()
# fuel_city01[regions_info, on = "abbrev_state",name_region := i.name_region]

# plot by region

p1 <- ggplot() +
  geom_point(data = fuel_city01,
             aes(y = fuel_per_pop ,x = weighted_density01km2))

p2 <- ggplot() +
  geom_point(data = fuel_city01,
             aes(y = fuel_per_pop ,x = weighted_density05km2))

p3 <- ggplot() +
  geom_point(data = fuel_city01,
             aes(y = fuel_per_pop ,x = weighted_density10km2))

p1 / p2 / p3
### ANALYSIS # 2
#
# add pop into fuel_city
fuel_city01 <- data.table::copy(fuel_city)
fuel_city01 <- fuel_city01[pop,on = c("cod_ibge" = "municipio_codigo"),
                           pop := i.valor ]

## add code_urban_concentration into fuel_city
#fuel_city[dens_city,on = c("cod_ibge"="code_muni"),
#          code_urban_concentration := i.code_urban_concentration]
fuel_city01 <- fuel_city01[year == 2018]

# sum ethanol and gasoline
fuel_city01 <- data.table::copy(fuel_city01)[fuel %in% c("etanol","gasolina")]
fuel_city01[,volume := as.numeric(volume)]
fuel_city01 <- fuel_city01[,volume := sum(volume),by = .(cod_ibge)]
fuel_city01 <- fuel_city01[,.SD[1],by = cod_ibge]
fuel_city01[,fuel := "etanol+gasolina"]
fuel_city01[,`:=`(pop = sum(pop),
                  volume = sum(volume)),by = .(fuel,cod_ibge)]
fuel_city01 <- fuel_city01[,.SD[1],by = .(fuel,cod_ibge)]
fuel_city01[,fuel_pop := volume/pop]

# estimate density
city3 <- data.table::copy(dens_city)[,.(
  density10km2 = weighted.mean(x=pop_density10km2, w=pop_10km),
  density05km2 = weighted.mean(x=pop_density05km2, w=pop_05km),
  density01km2 = weighted.mean(x=pop_density01km2, w=pop_01km)),
  by=code_muni ]


# add density into fuel consumption
fuel_city02 <- fuel_city01[city3, on = c("cod_ibge" = "code_muni")]


ggplot() +
  geom_point(data = fuel_city02,
             aes(y = fuel_pop ,x = density10km2))

