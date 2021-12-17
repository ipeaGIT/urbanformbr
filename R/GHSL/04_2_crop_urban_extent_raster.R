# description -------------------------------------------------------------

# this script crops and saves raster for each urban extent (saved at 04_1)
#..containing, for every year:
# i) built-up area data
# ii) population

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# directory ---------------------------------------------------------------

# * built up area

ghsl_built_dir <- "../../data/urbanformbr/ghsl/BUILT/UCA/"

# files vector
years <-c('1990','2000','2014')
files_built <- purrr::map(years, ~dir(ghsl_built_dir, pattern = ., full.names = T))

# * population

ghsl_pop_dir <- "../../data/urbanformbr/ghsl/POP/UCA/"

# files vector
files_pop <- purrr::map(years, ~dir(ghsl_pop_dir, pattern = ., full.names = T))


# define function ---------------------------------------------------------------


# * function built -----------------------------------------------------------------

f_crop_raster_cutoff_built <- function(input){

  # read all raster files from one year in a list
  #bua_raster <- purrr::map(input, ~ raster::raster(paste0(ghsl_built_dir, .)))
  bua_raster <- purrr::map(input, ~ raster::raster(paste0(.)))

  # extract the year
  anos <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS)[0-9]{4}"))

  # extract uca name
  uca_name <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2)"))

  # add years to name
  #uca_name <- paste0(uca_name,'_',anos)

  # rename each raster in the list
  names(bua_raster) <- uca_name

  # read 20% polygon cutoff
  bua_polygon20 <- readr::read_rds(
    paste0('../../data/urbanformbr/ghsl/results/urban_extent_uca_',unique(anos),'_cutoff20.rds')
    )
  bua_polygon20 <- bua_polygon20 %>%
    dplyr::arrange(name_uca_case)

  bua_polygon20 <- split(x = bua_polygon20, f = factor(bua_polygon20$name_uca_case))

  # make sure crs from polygon and raster are identical
  #identical(crs(bua_polygon20$abaetetuba), crs(bua_raster$abaetetuba))

  # make sure both lists contain same elements
  # i.e. filter santa_cruz_do_sul_rs excluded at script 04_1
  bua_raster <- bua_raster[names(bua_polygon20)]

  # crop raster
  uca_crop <- furrr::future_map2(
    .x = bua_raster, .y = bua_polygon20, function(x,y)
      raster::crop(x = x, y = y)
    )
  # mask raster with each uca sf
  uca_mask <- furrr::future_map2(
    .x = uca_crop, .y = bua_polygon20, function(x, y)
      raster::mask(x = x, mask = y)
  )

  # create directory
  if (!dir.exists("../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20_raster")){
    dir.create("../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20_raster")
  }

  files_output <- purrr::map_chr(
    names(uca_mask),
    ~paste0('GHS_BUILT_LDS',unique(anos),'_urban_extent_cutoff_20_', ., '_1K_raster.tif')
  )

  # write each mask as raster file
  purrr::walk2(
    uca_mask, files_output, function(x,y)
      raster::writeRaster(
        x = x,
        filename = paste0('../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20_raster/', y),
        overwrite = T
      )
  )

}


# * function pop -------------------------------------------------------------------

f_crop_raster_cutoff_pop <- function(input){

  # read all raster files from one year in a list
  #bua_raster <- purrr::map(input, ~ raster::raster(paste0(ghsl_built_dir, .)))
  bua_raster <- purrr::map(input, ~ raster::raster(.))

  # extract the year
  anos <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=E)[0-9]{4}"))

  # extract uca name
  uca_name <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=E[\\d]{4}_).+(?=_R2)"))

  # add years to name
  #uca_name <- paste0(uca_name,'_',anos)

  # rename each raster in the list
  names(bua_raster) <- uca_name


  # read 20% polygon cutoff
  if (unique(anos) == '2015') {
    bua_polygon20 <- readr::read_rds(
      paste0('../../data/urbanformbr/ghsl/results/urban_extent_uca_','2014','_cutoff20.rds')
    )
  } else {
    bua_polygon20 <- readr::read_rds(
      paste0('../../data/urbanformbr/ghsl/results/urban_extent_uca_',unique(anos),'_cutoff20.rds')
    )
  }

  bua_polygon20 <- bua_polygon20 %>%
    dplyr::arrange(name_uca_case)

  bua_polygon20 <- split(x = bua_polygon20, f = factor(bua_polygon20$name_uca_case))

  # make sure crs from polygon and raster are identical
  #identical(crs(bua_polygon20$abaetetuba), crs(bua_raster$abaetetuba))

  # make sure both lists contain same elements
  # i.e. filter santa_cruz_do_sul_rs excluded at script 04_1
  bua_raster <- bua_raster[names(bua_polygon20)]

  # crop raster
  uca_crop <- furrr::future_map2(
    .x = bua_raster, .y = bua_polygon20, function(x,y)
      raster::crop(x = x, y = y)
  )
  # mask raster with each uca sf
  uca_mask <- furrr::future_map2(
    .x = uca_crop, .y = bua_polygon20, function(x, y)
      raster::mask(x = x, mask = y)
  )

  # create directory
  if (!dir.exists("../../data/urbanformbr/ghsl/POP/urban_extent_cutoff_20_raster")){
    dir.create("../../data/urbanformbr/ghsl/POP/urban_extent_cutoff_20_raster")
  }

  files_output <- purrr::map_chr(
    names(uca_mask),
    ~paste0('GHS_POP_E',unique(anos),'_urban_extent_cutoff_20_', ., '_1K_raster.tif')
  )

  # write each mask as raster file
  purrr::walk2(
    uca_mask, files_output, function(x,y)
      raster::writeRaster(
        x = x,
        filename = paste0('../../data/urbanformbr/ghsl/POP/urban_extent_cutoff_20_raster/', y),
        overwrite = T
      )
  )

}

  # run function ------------------------------------------------------------

future::plan(future::multicore)

# * built up area ---------------------------------------------------------

# run for multiple years
purrr::walk(files_built, ~f_crop_raster_cutoff_built(.))

# * population ------------------------------------------------------------

# run for multiple years
purrr::walk(files_pop, ~f_crop_raster_cutoff_pop(.))

