# description -------------------------------------------------------------

# this script
# i. reads GHS-BUILT-BRASIL raster data from urban concentration (1km)
# ii. estimates the density of built up areas

# TO DO LIST
## CHECK IF NODATA VALUE (-200) INFLUENCES THE ESTIMATION
## USE FULL DATA (WORLD) TO COMPARE ESTIMATION WITH SUMMARY STATISTICS AT
# CORBANE ET AL (2019) OR FLORCZYK ET AL (2019)

# setup -------------------------------------------------------------------

source('R/setup.R')
library(stars)
library(raster)
library(rgdal)
#library(terra)
library(exactextractr)


# directory ---------------------------------------------------------------

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/"


# read data ---------------------------------------------------------------
# raster
bhz_bua <- readr::read_rds('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/GHS_BUILT_LDS1975_UC_BHZ_R2018A_54009_1K_V2_0_raster.rds')
# sf
bhz <- geobr::read_urban_concentrations() %>%
  dplyr::filter(code_urban_concentration == 3106200)
# change projection
bhz <- sf::st_transform(bhz, raster::projection(bhz_bua))

# raster way --------------------------------------------------------------



# exactestractr way -------------------------------------------------------

# read data
#bhz_bua <- readr::read_rds('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/GHS_BUILT_LDS1975_UC_BHZ_R2018A_54009_1K_V2_0_raster.rds')
#
extrair <- exactextractr::exact_extract(bhz_bua, bhz)
extrair[1:2] %>% lapply(function(x) head(x))
extrair_combined <- dplyr::bind_rows(extrair, .id = 'id')
dplyr::glimpse(extrair_combined)
extrair_id <- extrair_combined %>%
  dplyr::mutate(id = as.numeric(id)) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(bua_mean = mean(value))

bhz <- geobr::read_urban_concentrations() %>%
  dplyr::filter(code_urban_concentration == 3106200)

bhz$bua_mean <- extrair_id$bua_mean

bhz %>% sf::st_transform(crs = 4326) %>%
  ggplot() +
  geom_sf(aes(fill = bua_mean))

