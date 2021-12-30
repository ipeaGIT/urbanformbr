# description -------------------------------------------------------------

# this script reads raster data from GHSL population and build area datasets
# and converts them to a grided vector data
# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# setup parallel ----------------------------------------------------------

future::plan(future::multicore, workers = future::availableCores() / 2)

# define function ---------------------------------------------------------

# year <- 2014

f_create_uca_grids <- function(year) {

  urban_areas <- read_rds(sprintf("../../data/urbanformbr/ghsl/results/urban_extent_uca_%s_cutoff20.rds", year))

  # code_uca = 1501709
  # name_uca = "braganca"

  urban_areas_cells <- furrr::future_map2_dfr(
    urban_areas$code_urban_concentration,
    urban_areas$name_uca_case,
    function(code_uca, name_uca) {
      raster_built <- sprintf("../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20_raster/GHS_BUILT_LDS%s_%s_urban_extent_cutoff_20_1K_raster.tif", year, code_uca)

      if (year == 2014) {y <- 2015} else {y <- year}
      raster_pop <- sprintf("../../data/urbanformbr/ghsl/POP/urban_extent_cutoff_20_raster/GHS_POP_E%s_%s_urban_extent_cutoff_20_1K_raster.tif", y, code_uca)


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

        return(urban_area)
      } else {
        return (null)
      }
    })

  write_rds(urban_areas_cells, sprintf("../../data/urbanformbr/ghsl/results/grid_uca_%s_cutoff20.rds", year))

}

# run for mulitple years --------------------------------------------------

anos <- c("1990","2000","2014")

furrr::future_walk(anos, ~f_create_uca_grids(.))


