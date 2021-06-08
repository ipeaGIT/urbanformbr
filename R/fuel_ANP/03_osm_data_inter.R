rm(list=ls())
library(magrittr)
library(data.table)
library(mapview)
library(rgdal)

dens_city <- readr::read_rds("../../data/urbanformbr/density-experienced/density-experienced_urban-concentrations.rds")
data.table::setDT(dens_city)
dens_city[,code_muni := as.character(code_muni)]

list_osm <- list.files("../../data/urbanformbr/osmdata/pbf/",
                       full.names = TRUE)
city_code <- list.files("../../data/urbanformbr/osmdata/pbf/",
                        full.names = FALSE)
city_code <- gsub(pattern = ".osm.pbf",replacement = "",city_code)

br_muni <- geobr::read_municipality()

#
# function to intersect muni
#

intersect_muni <- function(i){
  message("---")
  message(city_code[i])

  # read muni
  tmp_muni <- br_muni[br_muni$code_muni == as.numeric(city_code[i]),] %>%
    sf::st_transform(4326)

  # read osm
  osm <- sf::st_read(list_osm[i],layer = "lines")

  # intersection
  tmp_inter <- sf::st_intersection(osm,tmp_muni$geom)

  readr::write_rds(tmp_inter,
                   sprintf("../../data/urbanformbr/osmdata/rds_intersected/%s.rds",
                           city_code[i]),compress = 'gz')
  return(NULL)
}

#
# function over all cities
#

future::plan(strategy = 'multisession', workers=10)
furrr::future_map(.x = seq_along(city_code),.f = intersect_muni)
