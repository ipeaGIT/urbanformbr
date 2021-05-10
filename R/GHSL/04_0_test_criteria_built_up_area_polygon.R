# description -------------------------------------------------------------

# this script
# 1. reads data from built up area cropped for selected urban areas (1975-2014)
# areas: for,bsb,bhz,ctb,poa,rio,sp,nat,man
#
# 2. tests different criteria for defining built-up area polygon (urban extent)
# ..to be compared between cities.
# 2.1 test 1km pixel. different cutoffs (>0%, 25%, 50%)

# setup -------------------------------------------------------------------

source("R/setup.R")

# directory ---------------------------------------------------------------

ghsl_built_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/"


# read data ---------------------------------------------------------------

# vetor com cidades e periodos desejados
#for,bsb,bhz,ctb,poa,rio,sp,nat,man
files_cities <- dir(
  "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/",
  pattern = "(1975|2014).*(sao_paulo|fortaleza|brasilia|curitiba|belo_horizonte|rio_de_janeiro|bage|porto_alegre)"
)

input <- files_cities



# NAO RODAR -> CRIAR FUNCAO CORRETA

funcao <- function(input) {

  # read all raster files from one year in a list
  bua_uca <- purrr::map(input, ~ raster::raster(paste0(ghsl_built_dir, .)))

  # extract the year
  anos <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS)[0-9]{4}"))

  # extract uca name
  uca_name <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2)"))

  # add years to name
  uca_name <- paste0(uca_name,'_',anos)

  # rename each raster in the list
  names(bua_uca) <- uca_name

  str(bua_uca,max.level = 1)


  # * funcao raster polygons and classify -----------------------------------
  # ver funcoes p/ obter bh1975 abaixo
  # incluir group_by e summarise depois
  # deixar o st_transform para depois? o que fazer? ESCLARECER
  # fazer projecao antes ou depois de extrair o valor?
bua_raster <- bua_uca$bage_rs_1975

  f_raster_pol_class <- function(bua_raster){

  bua_pol <- bua_raster %>%
    # convert raster to polygon (sp)
    raster::rasterToPolygons() %>%
    # transform to sf
    sf::st_as_sf() %>%
    # rename first column
    dplyr::rename(bua_value = 1) %>%
    # create columns classifying area based on cutoff values (0,25,50%)
    dplyr::mutate(
      cutoff_0 = dplyr::case_when(
        bua_value > 0 ~ "Construída",
        T ~ "Não construida"
      ),
      cutoff_10 = dplyr::case_when(
        bua_value >= 10 ~ "Construída",
        T ~ "Não construida"
      ),
      cutoff_25 = dplyr::case_when(
        bua_value >= 25 ~ "Construída",
        T ~ "Não construida"
      ),
      cutoff_50 = dplyr::case_when(
        bua_value >= 50 ~ "Construída",
        T ~ "Não construida"
        )
    )

  }

  # run for every uca
  bua_pol <- purrr::map(bua_uca, f_raster_pol_class)


  # * convert crs polygon and summarise -------------------------------------
  fteste <- function(base, variavel){

    variavel <- rlang::ensym(variavel)

    base %>%
      dplyr::group_by(!!variavel) %>%
      dplyr::summarise()

  }

  # create column name vector
  vetor <- paste0('cutoff_',c(0,10,25,50))
  # generate converted list of sf df
  #a <- pmap(list(variavel = vetor), fteste, base = bua_pol$bage_rs_1975)

  bua_convert <- map(bua_pol,
           ~pmap(list(variavel = vetor), fteste, base = .)
           )

  fnames <- function(base){
    rlang::set_names(base, paste0('cutoff_',c(0,10,25,50)))
  }

  # rename list elements
  bua_convert <- purrr::map(bua_convert, ~fnames(.))
  666666666666666
  # RENOMEAR ELEMENTOS DA LISTA





  b <- modify_depth(.x = a, .depth = 2, ~ rlang::set_names(names(.), nm = vector))
  a <- (a, ~rlang::set_names(., vetor))

  teste2 <- bua_pol$bage_rs_1975 %>%
    dplyr::group_by(cutoff_50) %>%
    dplyr::summarise() #%>%
    sf::st_transform(4326)

  a[[1]] %>%
    sf::st_transform(4326) %>%
    ggplot() +
    geom_sf(aes(fill = cutoff_0)) +
    viridis::scale_fill_viridis(discrete = T, option = "D")


  ### PROJECTION: O QUE FAZER? fazer projecao antes ou depois de extrair o valor?

  ### CLASSIFICACAO: QUAL CRITERIO? qual criterio (em termos quant.) de area
  # construida para classifica-la como "centro urbano" (e com isso obter..
  # ..o poligono de area construida de cada ano)

  bua_bhz1975 <- raster::raster("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/GHS_BUILT_LDS1975_belo_horizonte_mg_R2018A_54009_1K_V2_0_raster.tif")

  bua_bhz2014 <- raster::raster("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/GHS_BUILT_LDS2014_belo_horizonte_mg_R2018A_54009_1K_V2_0_raster.tif")

  bh1975 <- raster::rasterToPolygons(bua_bhz1975) %>%
    sf::st_as_sf() %>%
    dplyr::rename(bua_value = 1) %>%
    dplyr::mutate(
      grupo = dplyr::case_when(
        bua_value > 0 ~ "construida",
        T ~ "nao consturida"
      ),
      segmented = dplyr::case_when(
        bua_value >= 25 ~ "urban",
        T ~ "rural"
      )
    )

  bh_grupo1975 <- bh1975 %>%
    sf::st_transform(4326) %>%
    dplyr::group_by(grupo) %>%
    dplyr::summarise() %>%
    ggplot() +
    geom_sf(aes(fill = grupo)) +
    viridis::scale_fill_viridis(discrete = T, option = "D")

  bh_segmented1975 <- bh1975 %>%
    sf::st_transform(4326) %>%
    dplyr::group_by(segmented) %>%
    dplyr::summarise() %>%
    ggplot() +
    geom_sf(aes(fill = segmented)) +
    viridis::scale_fill_viridis(discrete = T, option = "D", direction = -1)


  bh2014 <- raster::rasterToPolygons(bua_bhz2014) %>%
    sf::st_as_sf() %>%
    dplyr::rename(bua_value = 1) %>%
    dplyr::mutate(
      grupo = dplyr::case_when(
        bua_value > 0 ~ "construida",
        T ~ "nao consturida"
      ),
      segmented = dplyr::case_when(
        bua_value >= 25 ~ "urban",
        T ~ "rural"
      )
    )

  bh_grupo2014 <- bh2014 %>%
    sf::st_transform(4326) %>%
    dplyr::group_by(grupo) %>%
    dplyr::summarise() %>%
    ggplot() +
    geom_sf(aes(fill = grupo)) +
    viridis::scale_fill_viridis(discrete = T, option = "D")

  bh_segmented2014 <- bh2014 %>%
    sf::st_transform(4326) %>%
    dplyr::group_by(segmented) %>%
    dplyr::summarise() %>%
    ggplot() +
    geom_sf(aes(fill = segmented)) +
    viridis::scale_fill_viridis(discrete = T, option = "D", direction = -1)

  bh_grupo1975 + bh_grupo2014
  bh_segmented1975 + bh_segmented2014




  # read uca shapefiles in one dataset
  uca_all <- readr::read_rds("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")

  # change shape crs
  uca_all <- sf::st_transform(uca_all, raster::projection(purrr::pluck(bua_uca, 1)))

  # split shape dataset into list with each uca as an individual element
  uca_split <- base::split(uca_all, uca_all$name_uca_case)

  # reorder shape list according to raster list (must do to ensure match)
  uca_split <- uca_split[order(names(bua_uca))]

  # extract raster information (average built up area within shape)
  extrair <- purrr::map2(.x = bua_uca, .y = uca_split, function(x, y) f_extrair_mean(x, y))

  # change built up area column name
  extrair <- purrr::map2(extrair, anos, function(x, y) {
    data.table::setnames(x, old = "bua_mean", new = paste0("bua_mean", y))
  })

  # bind all datasets together
  extrair <- data.table::rbindlist(extrair)

  # left join built up area extracted to uca shape
  uca_all <- dplyr::left_join(uca_all, extrair, by = c("name_uca_case" = "name_uca_case"))

  return(uca_all)
}


# run function ------------------------------------------------------------

# set up parallel
future::plan(future::multicore)

# files vector
years <- c("1975", "1990", "2000", "2014")
files <- purrr::map(years, ~ dir(ghsl_built_dir, pattern = .))

input <- files[[1]]

# run for multiple years
extrair <- furrr::future_map(files, ~ funcao(.))
