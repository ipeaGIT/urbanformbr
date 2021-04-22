# description -------------------------------------------------------------

# this script
# 1. reads data from built up area cropped for each urban concentration area...
#...for 1975 and 2014
# 2. converts area covered with built up area (value>0) as polygon
# 3. saves the resulting sf as rds for use at urbanformbr

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_built_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/"


# read data ---------------------------------------------------------------

# NAO RODAR -> CRIAR FUNCAO CORRETA

funcao <- function(input){

  # read all raster files from one year in a list
  bua_uca <- purrr::map(input, ~raster::raster(paste0(ghsl_built_dir, .)))

  # extract the year
  anos <- purrr::map_chr(input, ~stringr::str_extract(., "(?<=LDS)[0-9]{4}"))

  # extract uca name
  uca_name <- purrr::map_chr(input, ~stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2)"))

  # rename each raster in the list
  names(bua_uca) <- uca_name


  ### PROJECTION: O QUE FAZER? fazer projecao antes ou depois de extrair o valor?

  ### CLASSIFICACAO: QUAL CRITERIO? qual criterio (em termos quant.) de area
  # construida para classifica-la como "centro urbano" (e com isso obter..
  #..o poligono de area construida de cada ano)

  bua_bhz1975 <- raster::raster('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/GHS_BUILT_LDS1975_belo_horizonte_mg_R2018A_54009_1K_V2_0_raster.tif')

  bua_bhz2014 <- raster::raster('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/GHS_BUILT_LDS2014_belo_horizonte_mg_R2018A_54009_1K_V2_0_raster.tif')

  bh1975 <- raster::rasterToPolygons(bua_bhz1975) %>%
    sf::st_as_sf() %>%
    dplyr::rename(bua_value = 1) %>%
    dplyr::mutate(
      grupo = dplyr::case_when(
        bua_value > 0 ~ 'construida',
        T ~ 'nao consturida'
      ),
      segmented = dplyr::case_when(
        bua_value >= 25 ~ 'urban',
        T ~ 'rural'
      )
    )

  bh_grupo1975 <- bh1975 %>%
    sf::st_transform(4326) %>%
    dplyr::group_by(grupo) %>%
    dplyr::summarise() %>%
    ggplot() +
    geom_sf(aes(fill = grupo)) +
    viridis::scale_fill_viridis(discrete = T, option = 'D')

  bh_segmented1975 <- bh1975 %>%
    sf::st_transform(4326) %>%
    dplyr::group_by(segmented) %>%
    dplyr::summarise() %>%
    ggplot() +
    geom_sf(aes(fill = segmented)) +
    viridis::scale_fill_viridis(discrete = T, option = 'D', direction = -1)


  bh2014 <- raster::rasterToPolygons(bua_bhz2014) %>%
    sf::st_as_sf() %>%
    dplyr::rename(bua_value = 1) %>%
    dplyr::mutate(
      grupo = dplyr::case_when(
        bua_value > 0 ~ 'construida',
        T ~ 'nao consturida'
      ),
      segmented = dplyr::case_when(
        bua_value >= 25 ~ 'urban',
        T ~ 'rural'
      )
    )

  bh_grupo2014 <- bh2014 %>%
    sf::st_transform(4326) %>%
    dplyr::group_by(grupo) %>%
    dplyr::summarise() %>%
    ggplot() +
    geom_sf(aes(fill = grupo)) +
    viridis::scale_fill_viridis(discrete = T, option = 'D')

  bh_segmented2014 <- bh2014 %>%
    sf::st_transform(4326) %>%
    dplyr::group_by(segmented) %>%
    dplyr::summarise() %>%
    ggplot() +
    geom_sf(aes(fill = segmented)) +
    viridis::scale_fill_viridis(discrete = T, option = 'D', direction = -1)

  bh_grupo1975 + bh_grupo2014
  bh_segmented1975 + bh_segmented2014




  # read uca shapefiles in one dataset
  uca_all <- readr::read_rds('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds')

  # change shape crs
  uca_all <- sf::st_transform(uca_all, raster::projection(purrr::pluck(bua_uca, 1)))

  # split shape dataset into list with each uca as an individual element
  uca_split <- base::split(uca_all, uca_all$name_uca_case)

  # reorder shape list according to raster list (must do to ensure match)
  uca_split <- uca_split[order(names(bua_uca))]

  # extract raster information (average built up area within shape)
  extrair <- purrr::map2(.x = bua_uca, .y = uca_split, function(x,y) f_extrair_mean(x,y))

  # change built up area column name
  extrair <- purrr::map2(extrair, anos, function(x,y)
    data.table::setnames(x, old = 'bua_mean', new = paste0('bua_mean', y))
  )

  # bind all datasets together
  extrair <- data.table::rbindlist(extrair)

  # left join built up area extracted to uca shape
  uca_all <- dplyr::left_join(uca_all, extrair, by = c('name_uca_case' = 'name_uca_case'))

  return(uca_all)

}


# run function ------------------------------------------------------------

# set up parallel
future::plan(future::multicore)

# files vector
years <-c('1975','1990','2000','2014')
files <- purrr::map(years, ~dir(ghsl_built_dir, pattern = .))

input <- files[[1]]

# run for multiple years
extrair <- furrr::future_map(files, ~funcao(.))
