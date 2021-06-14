# description -------------------------------------------------------------

# this script process the downloaded logradouros data, and aggregates them by
# urban concentration area

# setup -------------------------------------------------------------------

source('R/setup.R')

urban_areas <- geobr::read_urban_concentrations()

# urban_area_code <- urban_areas$code_urban_concentration[[1]]
# urban_area_code <- 2211001
# urban_area_code <- 4301602 # Bagé
# process_urban_area(4301602)
# urban_area_code <- 2927408 # Salvador

output_dir <- tempdir()

# urban_area_code <- 2927408 # Salvador
# urban_area_code <- 4314407 # Pelotas


geocode_missing_by_logradouro_2019 <- function(urban_area_code) {
  if (!dir.exists("../../data/cnefe_geo_missing/")) { dir.create("../../data/cnefe_geo_missing/") }

  # extract urban area name and uf from data
  urban_area_name <- unique(subset(urban_areas, code_urban_concentration == urban_area_code)$name_urban_concentration)
  urban_area_name <- unlist(strsplit(urban_area_name, split = "/"))[1]
  urban_area_uf <- unique(subset(urban_areas, code_urban_concentration == urban_area_code & code_muni == urban_area_code)$abbrev_state)

  message(sprintf("Geocoding %s by 2019 logradouros", urban_area_name))

  gpkg_file <- paste0("../../data/cnefe_geo_missing/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_geo_2019.gpkg")

  if (!file.exists(gpkg_file)) {
    # load cnefe with missing coordinates
    rds_file <- paste0("../../data/cnefe_geo/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_missing.rds")
    # rds_file <- str_to_lower(rds_file)

    cnefe_df <- read_rds(rds_file) %>% setDT()

    cnefe_df <- cnefe_df[is.na(latitude), ]

    # build logradouro identifier
    cnefe_df[, CD_GEO := sprintf("%02d%05d%02d%02d%04d%s%s",
                                 as.numeric(uf),
                                 as.numeric(municipality),
                                 as.numeric(district),
                                 as.numeric(sub_district),
                                 as.numeric(tract),
                                 block_number,
                                 face_number)]


    # load logradouros
    gpkg_file <- paste0("../../data/logradouros_2019/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, ".gpkg")
    gpkg_file <- str_to_lower(gpkg_file)

    logradouros_sf <- st_read(gpkg_file) %>% setDT() %>%
      mutate(CD_GEO = paste0(CD_SETOR, CD_QUADRA, CD_FACE))


    # join cnefe to logradouros
    cnefe_missing <- cnefe_df[CD_GEO %nin% logradouros_sf$CD_GEO]
    cnefe_missing[, CD_GEO := NULL]
    rds_file <- paste0("../../data/cnefe_geo_missing/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_still_missing_2019.rds")
    write_rds(cnefe_missing, rds_file)



    cnefe_df <- cnefe_df[CD_GEO %in% logradouros_sf$CD_GEO]
    cnefe_df[logradouros_sf, on = "CD_GEO", geometry := i.geom]

    cnefe_df[, point := map(geometry, st_sample, size = 1)]

    cnefe_df[ ,latitude := map_dbl(point, function(x) st_coordinates(x)[[2]])]
    cnefe_df[ ,longitude := map_dbl(point, function(x) st_coordinates(x)[[1]])]

    cnefe_df[, CD_GEO := NULL]
    cnefe_df[, point := NULL]
    cnefe_df[, geometry := NULL]

    rds_file <- paste0("../../data/cnefe_geo_missing/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_found_2019.rds")
    write_rds(cnefe_df, rds_file)

    gpkg_file <- paste0("../../data/cnefe_geo_missing/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_geo_2019.gpkg")

    cnefe_sf = st_as_sf(cnefe_df, coords = c("longitude", "latitude"), crs = 4674, agr = "constant")

    st_write(cnefe_sf, gpkg_file)
  }

}

urban_area_code <- 4314407 # Pelotas
geocode_missing_by_logradouro_2019(4314407)

# run functions -----------------------------------------------------------

codes <- unique(urban_areas$code_urban_concentration)
walk(codes, geocode_missing_by_logradouro_2019)






