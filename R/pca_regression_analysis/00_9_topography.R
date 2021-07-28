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

  urban_extent_subset <- subset(urban_extent, code_muni == code_uca) %>%
    dplyr::group_by(code_muni, name_uca_case) %>%
    dplyr::summarise()

  urban_shapes_subset <- subset(urban_shapes, code_urban_concentration == code_uca)

  # muni_sf %>% mapview()

  # extract bounding box
  if (code_uca == 3205309){
    bbox <- st_bbox(urban_extent_subset)
  } else {
    bbox <- st_bbox(urban_shapes_subset)
  }

  data.table::fifelse(
    str_detect(names(bbox), "min$"),
    floor(bbox),
    ceiling(bbox)
    )

  #bbox["xmin"] <- floor(bbox["xmin"])
  #bbox["xmax"] <- ceiling(bbox["xmax"])
  #bbox["ymin"] <- floor(bbox["ymin"])
  #bbox["ymax"] <- ceiling(bbox["ymax"])

  #bbox["xmin"] <- bbox["xmin"] - 1
  #bbox["xmax"] <- bbox["xmax"] + 1
  #bbox["ymin"] <- bbox["ymin"] - 1
  #bbox["ymax"] <- bbox["ymax"] + 1

  #bbox <- as.integer(bbox) - 1

  bbox <- ifelse(bbox, as.integer(floor(bbox)), "erro")

  #(bbox <- data.table::fifelse(
  #  bbox < 0,
  #  as.integer(floor(bbox)),
  #  as.integer(ceiling(bbox))
  #))

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

  walk(
    zipfiles, function(z){
      if (file.size(z) > 500){
        unzip (z, exdir = outputdir)
      }
    }
    )

  # read all raster tiles, merge them together, and then crop to the study area's bounding box
  rst <- purrr::map(
    rstfiles, function(r){
      if (file.exists(r)) {
        return(raster(r))
      }
    }
    )

  rst <- rst %>%
    purrr::discard(is.null)

  if (length(rst) == 1) {
    rst_layer <- rst[[1]]
  } else {
    rst_layer <- do.call(raster::mosaic, args = c(rst, fun = mean, na.rm = T))
  }

  #rst_layer_crop <- raster::crop(rst_layer, st_bbox(muni_sf))
  rst_layer_crop <- raster::crop(rst_layer, urban_extent_subset)
  rst_layer_mask <- raster::mask(rst_layer_crop, urban_extent_subset)

  # create df
  output_df <- sf::st_drop_geometry(urban_extent_subset)
  output_df$sd_topography <- raster::cellStats(rst_layer_mask, sd, na.rm = T)

  return(output_df)

}


# run function all ucas ---------------------------------------------------

# future setting
#n_cores <- 10
#future::plan(future::multicore, workers = n_cores)
#furrr::future_map_dfr

# save in a df all topografy standard deviation
df_topo <- purrr::map_df(
  urban_extent$code_muni,
  ~f_download_srtm(code_uca = .)
  )

# save results ---------------------------------------------------------------

saveRDS(
  object = df_topo,
  file = '../../data/urbanformbr/pca_regression_df/topography.rds',
  compress = 'xz'
)


# detectar erro -----------------------------------------------------------

erro <- purrr::map(
  urban_extent$code_muni,
  purrr::possibly(~f_download_srtm(.x),'erro') # incluir funcao para verificar erro
)

map(erro, class) %>% unique()
teste <- purrr::keep(erro, inherits, 'character')

teste2 <- purrr::discard(erro, inherits, 'character')
teste2 <- bind_rows(teste2)

# deram erro (antes arrange): erro[[8]], erro[[147]], erro[[183]]
# aracaju_se (2800308)
# salvador_ba (2927408)
# vitoria_es (3205309)

uca_erro <- urban_shapes %>% filter(!code_urban_concentration %in% teste2$code_muni)


codigos_erro <- c(2800308,2927408,3205309)
filtro_erro <- urban_shapes %>% filter(code_urban_concentration %in% codigos_erro)
sf::st_is_valid(filtro_erro)
filtro_erro_valid <- sf::st_make_valid(filtro_erro)
# salvar erros para explorar
filtro_split <- split(filtro_erro, filtro_erro$name_uca_case)
purrr::walk2(
  .x = filtro_split, .y = names(filtro_split),
  ~sf::st_write(
    obj = .x,
    dsn = paste0("../../data/urbanformbr/urban_area_shapes/", .y,".gpkg")
    )
  )




# * check union polygons --------------------------------------------------

uc <- geobr::read_urban_concentrations(simplified = F)
ara <- uc %>% filter(code_urban_concentration==2800308)
sf::st_write(ara, "../../data/urbanformbr/urban_area_shapes/ara.gpkg")
teste <- ara %>% filter(code_muni %in% c(2804805,2803609))

### geometry union with dissolve_polygons function

# to_multipolygon
dissolved <- to_multipolygon(teste)
# simplify_temp_sf
dissolved <- simplify_temp_sf(dissolved)
# dissolve_polygons
dissolved <- dissolve_polygons(dissolved, 'code_urban_concentration')

### geometry union with sf::st_union

a <- sf::st_union(teste)


### geometry union with summarize
b <- ara %>% dplyr::group_by(code_urban_concentration) %>%
  dplyr::summarise()
# substituir b por urban_shapes_subset (testar usando union no lugar do dissolve)
# resultado é o mesmo (lons, lats, tiles, todos sao iguais)

# em 2800308, nao é possivel acessar o url S12W037
# em 2927408, nao é possivel acessar o url S14W038
# em 3205309, multiplos url nao existem (mais do que 4 urls, como o restante)
# erro na linha 106: walk(zipfiles, unzip, exdir = outputdir)


# detectar erro 2 ---------------------------------------------------------


erro <- purrr::map(
  urban_extent$code_muni,
  purrr::possibly(~f_download_srtm(.x),'erro') # incluir funcao para verificar erro
)

map(erro, class) %>% unique()
teste <- purrr::keep(erro, inherits, 'character')

teste2 <- purrr::discard(erro, inherits, 'character')
teste2 <- bind_rows(teste2)

# uca errro: 1400100  boa_vista


uca_erro <- urban_shapes %>% filter(!code_urban_concentration %in% teste2$code_muni)


# TESTAR CIDADES EM DIFERENTES POSICOES GEOGRAFICAS
# toda no hemisferio SUL
  # salvador
# toda no hemisferio Norte (existe na amostra?)
  # boa vista
# parte Sul parte Norte
  # macapa
cod_hem <- c(2927408,1400100,1600303)

code_uca <- cod_hem[[1]]

# save urban shape
sf::write_sf(
  urban_shapes_subset,
  paste0("../../data/urbanformbr/urban_area_shapes/",code_uca,"_shape.gpkg"
         )
  )
# save raster
raster::writeRaster(
  rst_layer,
  paste0("../../data/urbanformbr/urban_area_shapes/",code_uca,"_raster.tif"),
  overwrite=TRUE
  )


# COMPARACAO BOUNDING BOX X QGIS
# formula nova:
#bbox["ymin"] <- bbox["ymin"] - 1
#bbox["ymax"] <- bbox["ymax"] + 1
#bbox <- ifelse(bbox, as.integer(floor(bbox)), "erro")
2927408
xmin ymin xmax ymax
-39  -15  -38  -12
1400100
xmin ymin xmax ymax
-62    1  -61    4
1600303
xmin ymin xmax ymax
-52   -2  -50    2

# formula original:
#as.integer(st_bbox(urban_shapes_subset)) - 1
2927408
xmin ymin xmax ymax
-39  -14  -38  -13
1400100
xmin ymin xmax ymax
-62   1  -61   2
1600303
xmin ymin xmax ymax
-52  -1 -50   0

# QGIS
2927408
xmin ymin xmax ymax
-39   -14 -37  -12
1400100
xmin ymin xmax ymax
-62   2   -59   4
1600303
xmin ymin xmax ymax
-52   -1   -49  2
2800308
xmin ymin xmax ymax
-38  -12  -36  -10

# nova formula:
#data.table::fifelse(str_detect(names(bbox), "min$"),floor(bbox),ceiling(bbox))
2800308
xmin ymin xmax ymax
-38  -12  -36  -10
