# description -------------------------------------------------------------

# UPDATE DESCRIPTION

# setup -------------------------------------------------------------------

source('R/setup.R')


# define function ---------------------------------------------------------

# year <- 2014

f_create_total_area_uca_grids <- function(){

  # * read data -------------------------------------------------------------

  # * * read polygons -------------------------------------------------------
  # consolidated area = urban extent 1975
  polygon_consolidated_area <- readr::read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_1975_cutoff20.rds")

  # expansion area = st_sym_difference(urban_extent_1975, urban_extent_2014)
  polygon_expansion_area <- readr::read_rds("../../data/urbanformbr/ghsl/results/expansion_area_cutoff20.rds")

  # # total area = urban extent 2014
  polygon_total_area <- readr::read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_2014_cutoff20.rds")

  # * * read raster ---------------------------------------------------------

  year <- 1975
  code = 3106200
  name = "belo_horizonte_mg"


  urban_areas_cells <- map2_df(urban_areas$code_muni, urban_areas$name_uca_case, function(code, name) {
    raster_built <-paste0("../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20/",
                          "GHS_BUILT_LDS", year, "_urban_extent_cutoff_20_",
                          name, "_1K_raster.tif")

    if (year == 2014) { y <- 2015} else {y <- year}
    raster_pop <-paste0("../../data/urbanformbr/ghsl/POP/urban_extent_cutoff_20/",
                        "GHS_POP_E", y, "_urban_extent_cutoff_20_",
                        name, "_1K_raster.tif")

    if (file.exists(raster_built) & file.exists(raster_pop)) {
      urban_area <- raster::raster(raster_built) %>%
        rasterToPolygons() %>%
        st_as_sf() %>%
        mutate(
          code_muni = code,
          name_uca_case = name,
          cell = row_number()
          ) %>%
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





}




# run function ------------------------------------------------------------
# run for each year present at GHSL

create_uca_grids(1975)
create_uca_grids(1990)
create_uca_grids(2000)
create_uca_grids(2014)


