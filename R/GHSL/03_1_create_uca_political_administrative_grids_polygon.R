# description -------------------------------------------------------------

# this script reads raster data from GHSL population and build area datasets
# and converts them to a grided vector data (without considering the 20% cutoff)
# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# setup parallel ----------------------------------------------------------

future::plan(future::multicore, workers = future::availableCores() / 2)

# define function ---------------------------------------------------------

# year <- 2014

f_create_uca_grids <- function(year) {

  df_pol_admin_uca_shapes <- readr::read_rds('../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds') %>%
    select(-pop_ibge_total_2010)

  # code_uca = 1501709
  # name_uca = "braganca"

  urban_areas_cells <- furrr::future_map2_dfr(
    df_pol_admin_uca_shapes$code_urban_concentration,
    df_pol_admin_uca_shapes$name_uca_case,
    function(code_uca, name_uca) {

      raster_built <- sprintf("../../data/urbanformbr/ghsl/BUILT/UCA/GHS_BUILT_LDS%s_%s_R2018A_54009_1K_V2_0_raster.tif", year, code_uca)

      if (year == 2014) {y <- 2015} else {y <- year}

      raster_pop <- sprintf("../../data/urbanformbr/ghsl/POP/UCA/GHS_POP_E%s_%s_R2019A_54009_1K_V1_0_raster.tif", y, code_uca)

      if (file.exists(raster_built) & file.exists(raster_pop)) {

        urban_area <- raster(raster_built) %>%
          rasterToPolygons() %>%
          st_as_sf() %>%
          mutate(
            code_urban_concentration = code_uca,
            name_uca_case = name_uca,
            cell = row_number()
          ) %>%
          select(code_urban_concentration, name_uca_case, cell, built = 1, geometry)

        urban_pop <- raster(raster_pop) %>%
          rasterToPolygons() %>%
          st_as_sf() %>%
          st_centroid() %>%
          rename(pop = 1)

        urban_area <- st_join(urban_area, urban_pop)

        # number of obs might differ -> is that a problem?
        # it generates NA in some ucas. replace NA with VALUE==ZERO
        data.table::setDT(urban_area)

        f_replace_na <- function(DT){
          for (j in seq_len(ncol(DT))) {
            set(DT, which(is.na(DT[[j]])), j, 0)
          }
        }

        f_replace_na(urban_area)

        # urban_area <- urban_area %>%
        #   dplyr::mutate(
        #     built = tidyr::replace_na(built, 0)
        #     , pop = tidyr::replace_na(pop, 0)
        #   )
        #

        return(urban_area)

      } else {
        return (null)
      }
    })

  urban_areas_cells <- urban_areas_cells %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326)

  write_rds(urban_areas_cells, sprintf("../../data/urbanformbr/ghsl/results/grid_uca_%s_political_administrative_area.rds", year))

}

# run for mulitple years --------------------------------------------------

anos <- c("1990","2000","2014")

furrr::future_walk(anos, ~f_create_uca_grids(.))


