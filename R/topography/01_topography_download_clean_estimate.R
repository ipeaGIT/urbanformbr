# description -------------------------------------------------------------
# this cript downloads, cleans, and extracts topography metrics from nasa earthdata
#..topography raster data for all 182 urban concentration areas in the study

# OBS1: Adaptaion from acesso_oport/R/01_8-topografia.R

# OBS2: Username and password are extracted from aopint package, using aop APIs

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
source("R/fun_support/setup.R")

# setup parallel ----------------------------------------------------------

future::plan(future::multicore, workers = future::availableCores() / 2)

  # * update Renviron -------------------------------------------------------

  # Load AOP APIs username and passwords, using aopint package
  # https://github.com/dhersz/aopint
  # install.packages("aopint", repos = "https://dhersz.r-universe.dev")
  library(aopint)
  aopint::atualizar_renviron()
  username <- Sys.getenv("EARTHDATA_LOGIN")
  password <- Sys.getenv("EARTHDATA_PASS")


# read necessary files ----------------------------------------------------

# nameuca <- "abaetetuba"
# code_uca <- 1500107

# read urban extent 2014 polygons
urban_extent <- readr::read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_2014_cutoff20.rds")

# reproject urbanextent polygon to 4326

# urban concentration areas political administrative shapes
urban_shapes <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")
urban_shapes <- urban_shapes %>%
  dplyr::select(-starts_with("pop"))

# reproject urban extent crs to urban_shapes
urban_extent <- sf::st_transform(urban_extent, sf::st_crs(urban_shapes))

urban_extent <- urban_extent %>%
  dplyr::arrange(code_urban_concentration)

urban_shapes <- urban_shapes %>%
  dplyr::arrange(code_urban_concentration)

# define function ---------------------------------------------------------


f_download_srtm <- function(code_uca) {

  message(paste0("\n working on ", code_uca,"\n"))

  # subset urban extent
  urban_extent_subset <- subset(urban_extent, code_urban_concentration == code_uca) %>%
    dplyr::group_by(code_urban_concentration, name_uca_case) %>%
    dplyr::summarise()

  # subset urban shapes
  urban_shapes_subset <- subset(urban_shapes, code_urban_concentration == code_uca)

  # muni_sf %>% mapview()

  # extract bounding box
  # 3205309 (vitoria_es) has an island that prevents st_bbox to work properly on urban
  #...SHAPE (all political administrative area). hence, 3205309 should use urban extent
  if (code_uca == 3205309){
    bbox <- st_bbox(urban_extent_subset)
  } else {
    bbox <- st_bbox(urban_shapes_subset)
  }

  bbox <- data.table::fifelse(
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

  walk2(
    urls,
    zipfiles,
    function(url, filename) {
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

  rst_layer_terrain <- raster::terrain(
    rst_layer_mask,
    opt = c("slope","TRI"),
    neighbours = 8,
    unit = 'degrees'
  )

  # generate variables (all for urban extent, not administrative polygon)
  data.table::setDT(output_df)[
    ,
    `:=`(
      # elevation
      sd_elevation = raster::cellStats(rst_layer_mask, sd, na.rm = T),
      # mean slope
      mean_slope = raster::cellStats(rst_layer_terrain, mean, na.rm = T)["slope"],
      # sd slope
      sd_slope = raster::cellStats(rst_layer_terrain, sd, na.rm = T)["slope"],
      # tri (indice de rugosidade) extensao urbana
      mean_ruggedness = raster::cellStats(rst_layer_terrain, mean, na.rm = T)["tri"]
    )
  ]


  # * count number cells above threshold ------------------------------------

    ## count number of raster cells above threshold

  # subset only slope raster
  #df_slope <- subset(rst_layer_terrain, "slope") %>%
    # raster frequency table based on cells' value
  #  raster::freq(useNA = "no", merge = T) %>%
  #  data.table::as.data.table()

  # proportion of cells according to the their value
  #df_slope[, prop := count / sum(count)]

  # column: proportion of cells with slope above 10
  #output_df[
  #  ,
  #  prop_slope_above_10 := df_slope[value > 10, sum(prop)]
  #]


  # * return output ---------------------------------------------------------

  return(output_df)

}


# run function all ucas ---------------------------------------------------

# future setting
#n_cores <- 10
#future::plan(future::multicore, workers = n_cores)
#furrr::future_map_dfr

# save in a df all topografy standard deviation
df_topo <- furrr::future_map_dfr(
  urban_extent$code_urban_concentration,
  ~f_download_srtm(code_uca = .)
  )

# save results ---------------------------------------------------------------

data.table::fwrite(
  x = df_topo
  , file = '../../data/urbanformbr/topography/topography_non_filtered_df.csv'
  , sep = ";"
  , append = F
)
