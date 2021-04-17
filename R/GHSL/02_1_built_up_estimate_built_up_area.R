# description -------------------------------------------------------------

# this script
# i. reads GHS-BUILT-BRASIL (1km res) raster from each urban concentration
#..areas (uca)
# ii. estimates the density of built up areas

# TO DO LIST
## USE FULL DATA (WORLD) TO COMPARE ESTIMATION WITH SUMMARY STATISTICS AT
# CORBANE ET AL (2019) OR FLORCZYK ET AL (2019)

# setup -------------------------------------------------------------------

source('R/setup.R')


# directory ---------------------------------------------------------------

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/"


# read data ---------------------------------------------------------------

# * bhz -------------------------------------------------------------------

# raster
files_input <- dir(ghsl_dir, pattern = 'BHZ.*raster.tif$')

#bhz_bua <- raster::raster('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/GHS_BUILT_LDS1975_UC_BHZ_R2018A_54009_1K_V2_0_raster.tif')
bhz_bua <- purrr::map(files_input, ~raster::raster(paste0(ghsl_dir, .)))
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




# funcao correta ----------------------------------------------------------
####### CORRIGIR NAs -> devem vir da base de uca
# codigos
#1501709
#3507605
#2103000
#4305108

# criar vetor com cada ano c(1975,1990,2000,2014)
# criar funcao que le todos as ucas de cada ano
# purrr::map da funcao
ghsl_built_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/"


f_extrair <- function(raster, shape){

  extrair <- exactextractr::exact_extract(raster, shape) %>%
    dplyr::bind_rows(., .id = 'id') %>%
    dplyr::mutate(id = as.numeric(id)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(bua_mean = mean(value, na.rm = T)) %>%
    dplyr::mutate(id = shape$name_uca_case) %>%
    dplyr::rename(name_uca_case = id) %>%
    data.table::setDT()

}

#files_input <- dir(ghsl_built_dir)
files_input_1975 <- dir(ghsl_built_dir, pattern = '1975')
files_input_1990 <- dir(ghsl_built_dir, pattern = '1990')
files_input_2000 <- dir(ghsl_built_dir, pattern = '2000')
files_input_2014 <- dir(ghsl_built_dir, pattern = '2014')

#input_1975 <- files_input_1975[1:2]
#input_1990 <- files_input_1990[1:2]
#input_2000 <- files_input_2000[1:2]
#input_2014 <- files_input_2014[1:2]

#anos <- as.character(c(1975,1990,2000,2014))

#files_input <- purrr::map(anos, ~dir(ghsl_built_dir, pattern = anos))
#names(files_input) <- paste0('files',anos)

######### CORRIGIR FUNCAO PARA OBTER UCA_ALL DE CADA ANO
# OU RODAR FUNCAO INTEIRA PARA 740 UCAs e agregalas

funcao <- function(input){

  bua_uca <- purrr::map(input, ~raster::raster(paste0(ghsl_built_dir, .)))

  anos <- purrr::map_chr(input, ~stringr::str_extract(., "(?<=LDS)[0-9]{4}"))

  uca_name <- purrr::map_chr(input, ~stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2)"))

  names(bua_uca) <- uca_name

  uca_all <- readr::read_rds('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds')

  uca_all <- sf::st_transform(uca_all, raster::projection(purrr::pluck(bua_uca, 1)))

  uca_split <- base::split(uca_all, uca_all$name_uca_case)

  extrair <- purrr::map2(.x = bua_uca, .y = uca_split, function(x,y) f_extrair(x,y))

  extrair <- purrr::map2(extrair, anos, function(x,y)
    data.table::setnames(x, old = 'bua_mean', new = paste0('bua_mean', y))
  )

  extrair <- data.table::rbindlist(extrair)

  uca_all <- dplyr::left_join(uca_all, extrair, by = c('name_uca_case' = 'name_uca_case'))

  return(uca_all)

}

future::plan(future::multicore)


###### teste
files_input <- map(files_input, ~head(.,2))

#extrair <- furrr::future_map(files_input, ~funcao(.))
extrair <- furrr::future_map(files_input, ~furrr::future_map(., ~funcao(.)))

teste <- furrr::future_map(extrair, ~data.table::rbindlist(.))



#  teste manual -----------------------------------------------------------

#extrair <- furrr::future_map(files_input, ~funcao(.))
extrair <- furrr::future_map(
  list(files_input_1975,files_input_1990,files_input_2000,files_input_2014),
  ~funcao(.)
  )
#teste1975 <- funcao(files_input_1975)
#names(extrair) <- paste0('files', c(1975,1990,2000,2014))

uca_all_final <- dplyr::left_join(
  purrr::pluck(extrair, 1),
  #extrair$files1975,
  data.table::setDT(purrr::pluck(extrair, 2))[, .(name_uca_case, bua_mean1990)],
  by = c('name_uca_case' = 'name_uca_case')
)

uca_all_final <- dplyr::left_join(
  uca_all_final,
  data.table::setDT(purrr::pluck(extrair, 3))[, .(name_uca_case, bua_mean2000)],
  by = c('name_uca_case' = 'name_uca_case')
)

uca_all_final <- dplyr::left_join(
  uca_all_final,
  data.table::setDT(purrr::pluck(extrair, 4))[, .(name_uca_case, bua_mean2014)],
  by = c('name_uca_case' = 'name_uca_case')
)


# create directory
if (!dir.exists("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/results")){
  dir.create("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/results")
}

saveRDS(
  object = uca_all_final,
  file = '//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/results/uca_pop_100000_built_up_area_results.rds',
  compress = 'xz'
)


