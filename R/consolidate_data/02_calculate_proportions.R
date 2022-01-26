library("sidrar")
library("tidyverse")
library('geobr')
library('sf')
library('data.table')

#library(devtools)
#install_github("rpradosiqueira/sidrar")

# download data
br <- read_country(simplified =F)
ua <- read_urban_concentrations(simplified = F) %>% mutate(
  code_muni = as.character(code_muni))


# remove international areas
ua <- subset(ua, !(name_urban_concentration  %like% 'Internacional'))

# calculate areas
area_br <- st_area(br)

ua <- st_make_valid(ua)
ua$area <- st_area(ua)
area_ua <- sum(ua$area)

# proportion of area --------------
percent_area_ua <- (area_ua / area_br)*100

##### Pib of Urban Concentrations ----

#list_of_variables <- sidrar::info_sidra(x = 21)

pib_br <- get_sidra(x = 5938,
                 variable =37,
                 period = "2010",
                 geo = "City",
                 header = TRUE,
                 format = 1) 

colnames(pib_br)[4] <- "code_muni" 

pib_ua <- ua %>% left_join(pib_br, by = "code_muni")

percent_pib <- (sum(pib_ua$Valor)/sum(pib_br$Valor,na.rm = T))*100

