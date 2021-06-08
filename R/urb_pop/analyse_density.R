rm(list=ls())
library(magrittr)
library(data.table)
library(ggplot2)
library(ggpmisc)

dens_city <- readr::read_rds("../../data/urbanformbr/density-experienced/density-experienced_urban-concentrations.rds")
data.table::setDT(dens_city)
dens_city[,code_muni := as.character(code_muni)]


pop <- readr::read_rds("../../data/urbanformbr/population/population_muni_ibge.rds")


ibge_dens <- geobr::read_urban_area(year = 2015,simplified = FALSE)
ibge_dens$area <- sf::st_area(ibge_dens)
data.table::setDT(ibge_dens)
ibge_dens[,code_muni := as.character(code_muni)]
ibge_dens <- ibge_dens[,.(total_area = sum(area, na.rm = TRUE)),by = code_muni]


# estimate density by code_urban_concentration
temp_denscity <- data.table::copy(dens_city)[,.(
  weighted_density10km2 = weighted.mean(x=pop_density10km2, w=pop_10km),
  weighted_density05km2 = weighted.mean(x=pop_density05km2, w=pop_05km),
  weighted_density01km2 = weighted.mean(x=pop_density01km2, w=pop_01km)),
  by=code_urban_concentration ]

# add code_urban_concentration into ibge_dens

tmp_ibge_dens <- data.table::copy(ibge_dens)[dens_city,on = "code_muni",
          code_urban_concentration := i.code_urban_concentration]

# add population data into ibge_dens

tmp_ibge_dens[pop[ano == 2010],on=c("code_muni"="municipio_codigo"),
              pop2010 := i.valor]

# estimate density ibge by code_urban_concentration
tmp_ibge_dens <- tmp_ibge_dens[,.(
  density_ibge_km2 = sum(pop2010) /
    sum(as.numeric(units::set_units(total_area,"km^2")))),
  by = code_urban_concentration]

# add experienced_density into 'tmp_ibge_dens
temp_denscity[tmp_ibge_dens,on = "code_urban_concentration",
              density_ibge_km2 := i.density_ibge_km2]


axis_limit <- c(0,
                max(c(temp_denscity$weighted_density01km2,
                      temp_denscity$density_ibge_km2),na.rm = TRUE))
dt_xy <- data.table(x = 1:max(axis_limit),y = 1:max(axis_limit))
my.formula <- y ~ x

p <- ggplot(data = temp_denscity,
             aes(x = weighted_density01km2,y = density_ibge_km2)) +
  geom_point()+
  geom_line(data = dt_xy,aes(x = x, y = y))+
  geom_smooth(data = temp_denscity,
              aes(x = weighted_density01km2,y = density_ibge_km2),
              method = "lm", formula = my.formula)+
   stat_poly_eq(#data = dt_xy,aes(x = x, y = y),
                formula =my.formula,
                color = "blue",
                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                parse = TRUE) +
  annotate(geom="text", x= 10^4, y= 1.1*10^4,
           label="x = y",size=5,
             color="black")+
  labs(x = "Densidade experimentada (Raio 1km)",
         y = "Densidade zonas urbanas IBGE (Pop / Area)")+
  coord_cartesian(xlim = axis_limit,
                  ylim = axis_limit)
  ggExtra::ggMarginal(p, type = "histogram")

summary(temp_denscity$weighted_density01km2)
summary(temp_denscity$density_ibge_km2)






