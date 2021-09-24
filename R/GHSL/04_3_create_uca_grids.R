# description -------------------------------------------------------------

# this script reads raster data from GHSL population and build area datasets
# and converts them to vector data

# setup -------------------------------------------------------------------

source('R/setup.R')


# define function ---------------------------------------------------------

# year <- 2014

create_uca_grids <- function(year) {
  urban_areas <- read_rds(sprintf("../../data/urbanformbr/ghsl/results/urban_extent_uca_%s_cutoff20.rds", year))

  # code = 1501709
  # name = "braganca"

  urban_areas_cells <- map2_df(urban_areas$code_muni, urban_areas$name_uca_case, function(code, name) {
    raster_built <-paste0("../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20/",
                         "GHS_BUILT_LDS", year, "_urban_extent_cutoff_20_",
                         name, "_1K_raster.tif")

    if (year == 2014) { y <- 2015} else {y <- year}
    raster_pop <-paste0("../../data/urbanformbr/ghsl/POP/urban_extent_cutoff_20/",
                          "GHS_POP_E", y, "_urban_extent_cutoff_20_",
                          name, "_1K_raster.tif")

    if (file.exists(raster_built) & file.exists(raster_pop)) {
      urban_area <- raster(raster_built) %>%
        rasterToPolygons() %>% st_as_sf() %>%
        mutate(code_muni=code, name_uca_case = name, cell = row_number()) %>%
        select(code_muni, name_uca_case, cell, built = 1, geometry)

      urban_pop <- raster(raster_pop) %>%
        rasterToPolygons() %>% st_as_sf() %>%
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


# run function ------------------------------------------------------------
# run for each year present at GHSL

create_uca_grids(1975)
create_uca_grids(1990)
create_uca_grids(2000)
create_uca_grids(2014)


