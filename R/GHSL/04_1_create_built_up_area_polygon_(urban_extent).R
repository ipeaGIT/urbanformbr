# description -------------------------------------------------------------

# this script saves polygons for each urban concentration areas based on built-up
#..area percentage defined at script 04-0 (i.e. built-up area >= 20%)
# these polygons for built-up area are defined as "urban extent" and will be used
#..to compare urban expansion in terms of increase in density and urban footprint

# setup -------------------------------------------------------------------

source('R/setup.R')

# directory ---------------------------------------------------------------

ghsl_built_dir <- "../../data/urbanformbr/ghsl/BUILT/UCA/"

# files vector
years <-c('1975','1990','2000','2014')
files <- purrr::map(years, ~dir(ghsl_built_dir, pattern = .))

# APAGAR DEPOIS
#input <- files[[1]]

# define function ---------------------------------------------------------------
# set up parallel
future::plan(future::multicore)

f_create_polygon_cutoff <- function(input){

  # read all raster files from one year in a list
  bua_uca <- purrr::map(input, ~ raster::raster(paste0(ghsl_built_dir, .)))

  # extract the year
  anos <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS)[0-9]{4}"))

  # extract uca name
  uca_name <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2)"))

  # add years to name
  #uca_name <- paste0(uca_name,'_',anos)

  # rename each raster in the list
  names(bua_uca) <- uca_name

  # * function raster polygons and classify -----------------------------------

  # obs: uca that do not meet 20% cutoff criteria in 1975
  # santa_cruz_do_sul_rs : max(bua_value) = 13.5358
  # remove / exclude santa_cruz_do_sul_rs
  bua_uca$santa_cruz_do_sul_rs <- NULL

  f_raster_pol_class <- function(bua_raster){

    bua_pol <- bua_raster %>%
      # convert raster to polygon (sp)
      raster::rasterToPolygons() %>%
      # transform to sf
      sf::st_as_sf() %>%
      # rename first column
      dplyr::rename(bua_value = 1) %>%
      # create columns classifying area based on cutoff values
      dplyr::mutate(
          cutoff_20 = data.table::fcase(
          bua_value >= 20, 'Construída',
          bua_value < 20, 'Não construída'
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
  vetor <- paste0('cutoff_',c(20))
  # generate converted list of sf df

  bua_convert <- purrr::map(bua_pol, ~f_group_summarise(base = ., variavel = vetor))

  # * reproject crs ---------------------------------------------------------
  # reproject crs
  #bua_convert <- purrr::map(bua_convert, ~sf::st_transform(., crs = 4326))


  # * filter built-up area --------------------------------------------------
  # filter built-up area (this polygon is the `urban extent`)
  bua_convert <- purrr::map(bua_convert,
                            ~dplyr::filter(., .$cutoff_20 == 'Construída'))

  # add uca name to df
  nomes <- names(bua_convert)
  bua_convert <- purrr::map2(
    bua_convert, nomes, function(x,y)
    dplyr::mutate(
      .data = x,
      name_uca_case = y
    )
  )

  # read urban shapes saved at `urban_shapes.R` to get code_muni
  urban_shapes <- readr::read_rds('../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds') %>%
    sf::st_drop_geometry() %>%
    data.table::setDT() %>%
    dplyr::rename(code_muni = code_urban_concentration)
  # remove santa_cruz_do_sul_rs
  urban_shapes <- urban_shapes[!name_uca_case=='santa_cruz_do_sul_rs']

  # reduce list into df
  bua_reduce <- bind_rows(bua_convert)

  # add code_muni to urban extent
  bua_reduce <- dplyr::left_join(
    bua_reduce,
    urban_shapes %>% select(code_muni, name_uca_case),
    by = c('name_uca_case')
    )

  # reorder and select variables
  bua_reduce <- bua_reduce %>%
    dplyr::select(code_muni, name_uca_case, geometry)


  # * save data -------------------------------------------------------------

  ano <- unique(anos)

  # save as one df
  saveRDS(
    object = bua_reduce,
    file = paste0('../../data/urbanformbr/ghsl/results/urban_extent_uca_',ano,'_cutoff20.rds'),
    compress = 'xz'
  )

  # save each polygon separately
  bua_split <- split(bua_reduce, bua_reduce$name_uca_case)


  furrr::future_walk2(
    .x = bua_split, .y = names(bua_split), function(x,y)
      sf::st_write(
        obj = x,
        dsn = paste0("../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20_shape/urban_extent_",ano,"_cutoff_20_",y,".gpkg"),
        append = F)
  )


}

# run function ------------------------------------------------------------


# run for multiple years
furrr::future_map(files, ~f_create_polygon_cutoff(.))
