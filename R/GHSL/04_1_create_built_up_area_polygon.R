# description -------------------------------------------------------------

# this script saves polygons for each urban concentration areas based on built-up
#..area percentage defined at script 04-0 (i.e. above 25% built-up area)
# these polygons for built-up area are defined as "urban extent" and will be used
#..to compare urban expansion in terms of increase in density and urban footprint

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_built_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/"

# files vector
years <-c('1975','1990','2000','2014')
files <- purrr::map(years, ~dir(ghsl_built_dir, pattern = .))

input <- files[[1]]

# define function ---------------------------------------------------------------
f_create_polygon <- function(){

  # read all raster files from one year in a list
  bua_uca <- purrr::map(input, ~ raster::raster(paste0(ghsl_built_dir, .)))

  # extract the year
  #anos <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS)[0-9]{4}"))

  # extract uca name
  uca_name <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2)"))

  # add years to name
  #uca_name <- paste0(uca_name,'_',anos)

  # rename each raster in the list
  names(bua_uca) <- uca_name

  # * function raster polygons and classify -----------------------------------

  f_raster_pol_class <- function(bua_raster){

    bua_pol <- bua_raster %>%
      # convert raster to polygon (sp)
      raster::rasterToPolygons() %>%
      # transform to sf
      sf::st_as_sf() %>%
      # rename first column
      dplyr::rename(bua_value = 1) %>%
      # create columns classifying area based on cutoff values (10,25%)
      dplyr::mutate(
          cutoff_25 = data.table::fcase(
          bua_value >= 25, 'Construída',
          bua_value < 25, 'Não construída'
        )
      )

  }

  # run for every uca
  bua_pol <- purrr::map(bua_uca, f_raster_pol_class)

  # * convert crs polygon and summarise -------------------------------------
  f_group_summarise <- function(base, variavel){

    # rlang::ensym?
    variavel <- rlang::sym(variavel)

    base %>%
      dplyr::group_by(!!variavel) %>%
      dplyr::summarise()

  }

  # create column name vector
  vetor <- paste0('cutoff_',c(25))
  # generate converted list of sf df

  bua_convert <- purrr::map(bua_pol, ~f_group_summarise(base = ., variavel = vetor))

  # * reproject crs ---------------------------------------------------------
  # reproject crs
  bua_convert <- purrr::map(bua_convert, ~sf::st_transform(., crs = 4326))

  # filter built-up area
  bua_convert <- purrr::map(bua_convert,
                            ~dplyr::filter(., .$cutoff_25 == 'Construída'))

  # add uca name to df
  nomes <- names(bua_convert)
  bua_convert <- purrr::map2(
    bua_convert, nomes, function(x,y)
    dplyr::mutate(
      .data = x,
      name_muni = y
    )
  )

  bua_convert <- purrr::map2(
    .x = bua_convert, .y = nomes, function(x,y)
      purrr::map(.x = x, ~dplyr::mutate(., name_muni = y))
  )

  # reduce list into df
  bua_reduce <- purrr::reduce(bua_convert, dplyr::bind_rows)

  666666666666
  # add code_muni



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
