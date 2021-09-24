rm(list=ls())
library(magrittr)
library(data.table)
library(mapview)
library(rgdal)
source('R/colours.R')
source("R/setup.R")
source("R/urb_pop/colors_plot.R")
source("R/style.R")
# read fuel consumption
fuel_city <- readr::read_rds("../../data/urbanformbr/fuel/fuel_cities.rds")

# read experienced density
dens_city <- readr::read_rds("../../data/urbanformbr/density-experienced/density-experienced_urban-concentrations.rds")
data.table::setDT(dens_city)
dens_city[,code_muni := as.character(code_muni)]

# read population
pop <- readr::read_rds("../../data/urbanformbr/population/table_6579_ibge.rds")

# read streets intersected
list_streets <- list.files("../../data/urbanformbr/osmdata/rds_intersected/",
                       full.names = TRUE)
city_code <- list.files("../../data/urbanformbr/osmdata/pbf/",
                        full.names = FALSE)

# read osm_length

tmp_road <- readr::read_rds("data/osm_length.rds")
##### Analise

# calculate length of citie's street

road_length_f <- function(i){ # i = 1

  tmp_st <- readr::read_rds(list_streets[i])
  tmp_st <- tmp_st[tmp_st$highway %in% c("residential","service",
                                       "unclassified","tertiary",
                                       "track","secondary",
                                       "path","primary",
                                       "secondary_link","tertiary_link",
                                       "living_street","primary_link",
                                       "trunk","trunk_link",
                                       "construction","motorway",
                                       "motorway_link","services",
                                       "road"),]
  tmp_length <- sf::st_length(tmp_st$geometry) %>% sum() %>%
    units::set_units("km")
  dt_export <- data.table("city" = city_code[i],
                          "road_length" = tmp_length)


  return(dt_export)
}


# run function

future::plan(strategy = 'multisession', workers=10)

#dt <- furrr::future_map(.x = seq_along(list_streets)[1],.f = road_length_f) %>%
#  data.table::rbindlist()
# dt <- lapply(seq_along(list_streets),road_length_f) %>%
#   data.table::rbindlist()

#readr::write_rds(dt,"data/osm_length.rds",compress="gz")


### analysis 1

# FUEL
# add pop into fuel_city
tmp_fuel <- data.table::copy(fuel_city)
tmp_fuel[pop,on = c("cod_ibge" = "municipio_codigo"),
            pop := i.valor ]

# add code_urban_concentration into fuel_city

tmp_fuel[dens_city,on = c("cod_ibge"="code_muni"),
            code_urban_concentration := i.code_urban_concentration]
# remove NA's and keep fuel use onlt for 2018
tmp_fuel <- tmp_fuel[!is.na(code_urban_concentration) &
                             year == 2018 &
                       fuel %in% c("etanol","gasolina")]
# sum ethanol and gasoline
tmp_fuel[,volume := as.numeric(volume)]
tmp_fuel <- tmp_fuel[, volume := sum(volume),by = .(cod_ibge)]
tmp_fuel <- tmp_fuel[,.SD[1],by = cod_ibge]
tmp_fuel[,fuel := "etanol+gasolina"]
tmp_fuel[,`:=`(pop = sum(pop),volume = sum(volume)),
         by = .(fuel,code_urban_concentration)]
tmp_fuel <- tmp_fuel[,.SD[1],by = .(fuel,code_urban_concentration)]

# estimate population/fuel
tmp_fuel <- tmp_fuel[,.(pop = sum(pop, na.rm = TRUE),
                              volume = sum(volume, na.rm = TRUE)),
                           by = .(fuel,code_urban_concentration)]
# ratio volum_per_pop
tmp_fuel[,fuel_per_pop := volume/pop]

# density by code_urban_concentration
temp_denscity <- data.table::copy(dens_city)[,.(
  weighted_density10km2 = weighted.mean(x=pop_density10km2, w=pop_10km),
  weighted_density05km2 = weighted.mean(x=pop_density05km2, w=pop_05km),
  weighted_density01km2 = weighted.mean(x=pop_density01km2, w=pop_01km)),
  by=code_urban_concentration ]

# add density into tmp_fuel
tmp_fuel[temp_denscity,on = "code_urban_concentration",
         weighted_density01km2 := i.weighted_density01km2]

# ROAD
#
# add urban_concentration into tmp_road
tmp_road[dens_city,on = c("city"="code_muni"),
   code_urban_concentration := i.code_urban_concentration]

# add pop into tmp_road
tmp_road[pop,on = c("city"="municipio_codigo"),pop := i.valor]

# road_index estimates
tmp_road <- tmp_road[,.(road_index = road_length * 1000 / (pop )),
         by = code_urban_concentration][,.SD[1],by = code_urban_concentration]


# add road into tmp_fuel
tmp_fuel[tmp_road,on = "code_urban_concentration",road_index := i.road_index]


#### plot fuel vs density


ggplot(data = tmp_fuel,
       aes(y = fuel_per_pop,
           x = weighted_density01km2))+
  geom_point()+
  labs(x = "Densidade experimentada a 1 km <br>(hab/km²)",
       y = "Consumo de combustível <br>(m³/hab.ano)")+
  aop_style()

ggsave("figures/urb_pop/fuel_densidade.png",scale=0.9,
       width = 27*0.7,height = 18*0.7,dpi = 300,units = "cm")

#### plot length road vs density


ggplot(data = tmp_fuel,
       aes(y = as.numeric(road_index),
           x = weighted_density01km2))+
  geom_point()+
  labs(x = "Densidade experimentada a 1 km (hab/km²)",
       y = "Transporte rodoviário <br>(km de vias por 1000 habitantes)")+
  aop_style()

ggsave("figures/urb_pop/street_densidade.png",scale=0.9,
       width = 27*0.7,height = 18*0.7,dpi = 300,units = "cm")

#### plot fuel vs road length

ggplot(data = tmp_fuel,
       aes(y = as.numeric(road_index),
           x = fuel_per_pop))+
  geom_point()+
  ylim(c(0,50))+
  labs(x = "Volume combustível <br>(m³/hab.ano)",
       y = "Transporte rodoviário <br>(km de vias por 1000 habitantes)")+
  geom_smooth(method = "lm",formula = y ~ x)+
  aop_style()




ggsave("figures/urb_pop/fuel_street.png",scale=0.9,
       width = 27*0.7,height = 18*0.7,dpi = 300,units = "cm")


