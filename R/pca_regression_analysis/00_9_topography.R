# description -------------------------------------------------------------
# Adaptaion from acesso_oport/R/01_8-topografia.R to download topography raster data
# from earthdata.nasa.gov, using urban extent polygons in 2014 for each urban
# concentration area.

# Username and password are extracted from aopint package, using aop APIs

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### Leitura e filtro de elevacao/topografia

#' info:
#' Os dados de topografia são provenientes da missão SRTM (_Shuttle Radar
#' Topography Mission_), que é um esforço de pesquisa internacional que obtém
#' dados de elevação numa precisão de 30 metros. Os dados de elevação do SRTM são
#' divididos por quadrículo de 1 grau de latidude e 1 longitude, então é
#' necessário:
#'   1 - identificar os quadrículos que cobrem a area de concentracao urbana escolhida;
#'   2 - baixar o raster correspondente a cada quadrículos em uma pasta temporária;
#'   3 - unir os grids em um grid único, usando a função raster::mosaic();
#'   4 - recortar do raster a área correspondente ao bounding box do município;
#'   5 - salvar o raster do município na pasta correspondente.
#'
#' Para ter acesso a esses dados, é necessário criar um login no site
#' https://urs.earthdata.nasa.gov, e informar os dados de usuário e senha quando
#' for rodar esse script.


# setup -------------------------------------------------------------------
source("R/setup.R")


  # * update Renviron -------------------------------------------------------

  # Load AOP APIs username and passwords, using aopint package
  library(aopint) # https://github.com/dhersz/aopint
  aopint::atualizar_renviron()
  username <- Sys.getenv("EARTHDATA_LOGIN")
  password <- Sys.getenv("EARTHDATA_PASS")


# define function ---------------------------------------------------------

# nameuca <- "abaetetuba"
# code_uca <- 1500107

# read urban extent 2014 polygons
urban_extent <- readr::read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_2014_cutoff20.rds")

# reproject urbanextent polygon to 4326

# urban concentration areas political administrative shapes
urban_shapes <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")
# filter only 184 obs
urban_shapes <- subset(urban_shapes, code_urban_concentration %in% urban_extent$code_muni)
urban_shapes <- urban_shapes %>%
  dplyr::select(-starts_with("pop"))

# reproject urban extent crs to urban_shapes
urban_extent <- sf::st_transform(urban_extent, sf::st_crs(urban_shapes))

urban_extent <- urban_extent %>%
  dplyr::arrange(name_uca_case)

urban_shapes <- urban_shapes %>%
  dplyr::arrange(name_uca_case)


f_download_srtm <- function(code_uca) {

  message(paste0("\n working on ", code_uca,"\n"))

  urban_extent_subset <- subset(urban_extent, code_muni == code_uca)

  urban_shapes_subset <- subset(urban_shapes, code_urban_concentration == code_uca)

  # muni_sf %>% mapview()

  # extract bounding box
  bbox <- st_bbox(urban_shapes_subset)
  bbox <- as.integer(bbox) - 1

  # identify which tiles are needed to cover the whole study area
  lons <- seq(floor(bbox[1]), ceiling(bbox[3]), by = 1)
  lats <- seq(floor(bbox[2]), ceiling(bbox[4]), by = 1)
  tiles <- expand.grid(lat = lats, lon = lons) %>%
    mutate(hx = data.table::fifelse(lon < 0, "W", "E"),
           hy = data.table::fifelse(lat < 0, "S", "N"))
  tile = sprintf("%s%02d%s%03d", tiles$hy, abs(tiles$lat), tiles$hx, abs(tiles$lon))

  # build the url's for each tile
  urls <- paste0("https://e4ftl01.cr.usgs.gov/MEASURES/SRTMGL1.003/2000.02.11/",
                 tile, ".SRTMGL1.hgt.zip")

  # download zip files and extract raster tiles
  outputdir <- tempdir()
  zipfiles <- paste0(outputdir, "\\", tile, ".hgt.zip")
  rstfiles <- paste0(outputdir, "\\", tile, ".hgt")

  walk2(urls, zipfiles, function(url, filename) {
    httr::GET(url = url,
              authenticate(username, password),
              write_disk(path =filename, overwrite = TRUE),
              progress())
  })

  walk(zipfiles, unzip, exdir = outputdir)

  # read all raster tiles, merge them together, and then crop to the study area's bounding box
  rst <- map(rstfiles, raster)
  if (length(rst) == 1) {
    rst_layer <- rst[[1]]
  } else {
    rst_layer <- do.call(raster::mosaic, args = c(rst, fun = mean))
  }

  #rst_layer_crop <- raster::crop(rst_layer, st_bbox(muni_sf))
  rst_layer_crop <- raster::crop(rst_layer, urban_extent_subset)
  rst_layer_mask <- raster::mask(rst_layer_crop, urban_extent_subset)

  # create df
  output_df <- sf::st_drop_geometry(urban_extent_subset)
  output_df$sd_topography <- raster::cellStats(rst_layer_mask, sd)

  return(output_df)

}


# run function all ucas ---------------------------------------------------

# future setting
#n_cores <- 10
#future::plan(future::multicore, workers = n_cores)
#furrr::future_map_dfr

# save in a df all topografy standard deviation
df_topo <- purrr::map_df(
  urban_shapes$code_urban_concentration,
  ~f_download_srtm(code_uca = .)
  )

# save results ---------------------------------------------------------------

saveRDS(
  object = df_topo,
  file = '../../data/urbanformbr/pca_regression_df/censo.rds',
  compress = 'xz'
)


# detectar erro -----------------------------------------------------------

erro <- purrr::map(
  urban_shapes$code_urban_concentration,
  purrr::possibly(~f_download_srtm(.x),'erro') # incluir funcao para verificar erro
)

# erro -> linha 76 -> 4108304 - internacional_de_foz_do_iguacu_brasil_ciudad_del_este_paraguai

map(erro, class) %>% unique()
teste <- purrr::keep(erro, inherits, 'character')

teste2 <- purrr::discard(erro, inherits, 'character')
teste2 <- bind_rows(teste2)

# deram erro (antes arrange): erro[[8]], erro[[147]], erro[[183]]
# aracaju (2800308) salvador (2927408) vitoria (3205309)
uca_erro <- urban_shapes %>% filter(!code_urban_concentration %in% teste2$code_muni)



