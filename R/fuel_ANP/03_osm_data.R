rm(list=ls())
library(magrittr)
library(data.table)
library(ggplot2)
library(tictoc)

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
# fuel_city01[,`:=`(pop = sum(pop),
#                   volume = sum(volume)),by = .(fuel,code_urban_concentration)]
# fuel_city01 <- fuel_city01[,.SD[1],by = .(fuel,code_urban_concentration)]



# add geometry
muni <- geobr::read_municipality()
data.table::setDT(muni)
muni[,code_muni := as.character(code_muni)]

fuel_city01[muni,on = c("cod_ibge"="code_muni"),geometry := i.geom]

fuel_city02 <- sf::st_as_sf(fuel_city01) %>% sf::st_set_crs(4326)
#

extrai_malha_viaria <- function(i) { # i = list_muni[1]

  muni <- fuel_city02[fuel_city02$cod_ibge == i,]
  ## encontrar bounding box da cidade, a partir do grid de topografia

  muni_bbox <- sf::st_bbox(muni) %>% as.matrix() %>% t() %>% as.data.frame()

  #' construir paths com a localização do Osmosis e dos arquivos PBF de
  #' origem e destino

  osmosis_path <- sprintf("../../data-raw/malha_viaria/osmosis/bin/osmosis.bat")

  # PBF do Brasil inteiro
  br_pbf <- sprintf("../../data-raw/malha_viaria/2020/br/brazil-latest-filtered.osm.pbf")

  # PBF do município
  muni_pbf <- sprintf("../../data/urbanformbr/osmdata/%s.osm.pbf", i)

  # Constrói linha de comando para executar o Osmosis
  osmosis_cmd <- sprintf("%s --read-pbf %s --bounding-box left=%s bottom=%s right=%s top=%s --write-pbf %s",
                         osmosis_path, br_pbf,
                         muni_bbox$xmin, muni_bbox$ymin, muni_bbox$xmax, muni_bbox$ymax,
                         muni_pbf)

  # Chama o Osmosis
  tictoc::tic(msg = i)
  shell(osmosis_cmd, translate = TRUE)
  tictoc::toc()
}

list_muni <- unique(fuel_city02$cod_ibge)

purrr::walk(list_muni[-c(1:374)], extrai_malha_viaria)
#system.time(extrai_malha_viaria(muni = "poa", year = 2020))
