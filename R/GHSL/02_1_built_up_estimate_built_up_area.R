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
library(viridis)
#library(hrbrthemes)


# directory ---------------------------------------------------------------

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/"


# read data ---------------------------------------------------------------

# * bhz -------------------------------------------------------------------

# raster
files_input <- dir(ghsl_dir, pattern = 'BHZ.*raster.tif$')

#bhz_bua <- raster::raster('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/GHS_BUILT_LDS1975_UC_BHZ_R2018A_54009_1K_V2_0_raster.tif')
bhz_bua <- purrr::map(files_input, ~raster::raster(paste0(ghsl_dir, .))
  )
anos <- purrr::map_chr(files_input, ~stringr::str_extract(., "(?<=LDS)[0-9]{4}"))
names(bhz_bua) <- paste0('bhz_bua_',anos)


# sf
bhz <- geobr::read_urban_concentrations() %>%
  dplyr::filter(code_urban_concentration == 3106200)
# change projection
#bhz <- sf::st_transform(bhz, raster::projection(bhz_bua))
projecao_files <- rgdal::GDALinfo('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/GHS_BUILT_LDS1975_UC_BHZ_R2018A_54009_1K_V2_0_raster.tif') %>%
  attr('projection')
bhz <- sf::st_transform(bhz, raster::projection(projecao_files))


# * brasil ----------------------------------------------------------------

# raster
files_input_br <- dir(ghsl_dir, pattern = 'BRASIL.*raster.tif$')

#bhz_bua <- raster::raster('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/GHS_BUILT_LDS1975_UC_BHZ_R2018A_54009_1K_V2_0_raster.tif')
br_bua <- purrr::map(files_input_br, ~raster::raster(paste0(ghsl_dir, .)))
#anos <- purrr::map_chr(files_input, ~stringr::str_extract(., "(?<=LDS)[0-9]{4}"))
names(br_bua) <- paste0('br_bua_',anos)


# sf
br <- geobr::read_country()
# change projection
#bhz <- sf::st_transform(bhz, raster::projection(bhz_bua))
#projecao_files <- rgdal::GDALinfo('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/GHS_BUILT_LDS1975_UC_BHZ_R2018A_54009_1K_V2_0_raster.tif') %>%
#  attr('projection')
br <- sf::st_transform(br, raster::projection(projecao_files))


# exactestractr way -------------------------------------------------------

funcao_extrair <- function(bua, shape){

  # raster
  bhz_bua <- raster::raster(paste0(ghsl_dir, bua))

  extrair <- exactextractr::exact_extract(bhz_bua, shape) %>%
    dplyr::bind_rows(., .id = 'id') %>%
    dplyr::mutate(id = as.numeric(id)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(bua_mean = mean(value))

  return(extrair)

}

# BHZ ---------------------------------------------------------------------

extracao_valores <- purrr::map(files_input, ~funcao_extrair(., bhz))

names(extracao_valores) <- paste0('bhz_bua_', anos)

bhz <- bhz %>%
  dplyr::mutate(
    bua_mean_1975 = 0.01 * extracao_valores$bhz_bua_1975$bua_mean,
    bua_mean_1990 = 0.01 * extracao_valores$bhz_bua_1990$bua_mean,
    bua_mean_2000 = 0.01 * extracao_valores$bhz_bua_2000$bua_mean,
    bua_mean_2014 = 0.01 * extracao_valores$bhz_bua_2014$bua_mean
  )


p1975 <- bhz %>%
  sf::st_transform(crs = 4326) %>%
  ggplot() +
  geom_sf(aes(fill = bua_mean_1975)) +
  scale_fill_viridis() +
  ggtitle('1975')

p1990 <- bhz %>%
  sf::st_transform(crs = 4326) %>%
  ggplot() +
  geom_sf(aes(fill = bua_mean_1990)) +
  scale_fill_viridis() +
  ggtitle('1990')

p2000 <- bhz %>%
  sf::st_transform(crs = 4326) %>%
  ggplot() +
  geom_sf(aes(fill = bua_mean_2000)) +
  scale_fill_viridis() +
  ggtitle('2000')

p2014 <- bhz %>%
  sf::st_transform(crs = 4326) %>%
  ggplot() +
  geom_sf(aes(fill = bua_mean_2014)) +
  scale_fill_viridis() +
  ggtitle('2014')

(p1975 + p1990) / (p2000 + p2014)


# br ---------------------------------------------------------------------

extracao_valores_br <- purrr::map(files_input_br, ~funcao_extrair(., br))

names(extracao_valores_br) <- paste0('br_bua_', anos)

br <- br %>%
  dplyr::mutate(
    bua_mean_1975 = 0.01 * extracao_valores_br$br_bua_1975$bua_mean,
    bua_mean_1990 = 0.01 * extracao_valores_br$br_bua_1990$bua_mean,
    bua_mean_2000 = 0.01 * extracao_valores_br$br_bua_2000$bua_mean,
    bua_mean_2014 = 0.01 * extracao_valores_br$br_bua_2014$bua_mean
  )


pbr1975 <- br %>%
  sf::st_transform(crs = 4326) %>%
  ggplot() +
  geom_sf(aes(fill = bua_mean_1975)) +
  scale_fill_viridis() +
  ggtitle('1975')

pbr1990 <- br %>%
  sf::st_transform(crs = 4326) %>%
  ggplot() +
  geom_sf(aes(fill = bua_mean_1990)) +
  scale_fill_viridis() +
  ggtitle('1990')

pbr2000 <- br %>%
  sf::st_transform(crs = 4326) %>%
  ggplot() +
  geom_sf(aes(fill = bua_mean_2000)) +
  scale_fill_viridis() +
  ggtitle('2000')

pbr2014 <- br %>%
  sf::st_transform(crs = 4326) %>%
  ggplot() +
  geom_sf(aes(fill = bua_mean_2014)) +
  scale_fill_viridis() +
  ggtitle('2014')

(pbr1975 + pbr1990) / (pbr2000 + pbr2014)






# read data
#bhz_bua <- readr::read_rds('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/GHS_BUILT_LDS1975_UC_BHZ_R2018A_54009_1K_V2_0_raster.rds')
#
extrair_br <- exactextractr::exact_extract(bhz_bua, br)
#extrair_br[1:2] %>% lapply(function(x) head(x))
extrair_br_combined <- dplyr::bind_rows(extrair_br, .id = 'id')
#dplyr::glimpse(extrair_combined)
extrair_br_id <- extrair_br_combined %>%
  dplyr::mutate(id = as.numeric(id)) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(bua_mean = mean(value))


br$bua_mean <- extrair_br_id$bua_mean

br %>% sf::st_transform(crs = 4326) %>%
  ggplot() +
  geom_sf(aes(fill = bua_mean))
