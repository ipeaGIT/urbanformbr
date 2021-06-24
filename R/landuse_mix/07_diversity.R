# description -------------------------------------------------------------

# this script process the downloaded logradouros data, and aggregates them by
# urban concentration area

# setup -------------------------------------------------------------------

source('R/setup.R')

# list of Brazilian municipalities
munis_df <- geobr::lookup_muni("all")

urban_areas <- read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_2014_cutoff20.rds")



code = 1501709
name = "braganca"

urban_areas_cells <- map2_df(urban_areas$code_muni, urban_areas$name_uca_case, function(code, name) {
  raster_file <-paste0("../../data/urbanformbr/ghsl/BUILT/urban_extent_cutoff_20/",
                       "GHS_BUILT_LDS2014_urban_extent_cutoff_20_",
                       name, "_1K_raster.tif")

  if (file.exists(raster_file)) {
    urban_area <- raster(raster_file) %>%
      rasterToPolygons() %>% st_as_sf() %>%
      mutate(code_muni=code, name_uca_case = name, cell = row_number()) %>%
      select(-1, code_muni, name_uca_case, cell, geometry)

    return(urban_area)
  } else {
    return (null)
  }
})

uca_sf <- urban_areas_cells %>%
  filter(name_uca_case=="bage_rs") %>%
  st_transform(4326)

# list of Brazilian municipalities
munis_df <- geobr::lookup_muni("all")

# muni <- 4301602 # Bagé

# extract urban area name and uf from data
muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

geo_2010_file <- paste0("../../data/urbanformbr/cnefe/geo/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".gpkg")
cnefe_sf <- st_read(geo_2010_file) %>% st_transform(4326)


cnefe_join_sf <- st_join(cnefe_sf, uca_sf)

landuse_by_cell <- cnefe_join_sf %>%
  st_set_geometry(NULL) %>%
  group_by(landuse_id, cell) %>%
  summarise(count = n())


