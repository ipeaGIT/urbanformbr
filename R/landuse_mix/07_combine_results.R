# description -------------------------------------------------------------

# this script process the downloaded logradouros data, and aggregates them by
# urban concentration area

# setup -------------------------------------------------------------------

source('R/setup.R')

# list of Brazilian municipalities
munis_df <- geobr::lookup_muni("all")

# muni <- 4301602 # Bagé
# muni <- 4314407 # Pelotas
# muni <- 2304400 # Fortaleza
# muni <- 1101401 # Monte Negro

# function ----------------------------------------------------------------

combine_results <- function(muni) {
  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  output_file <- paste0("../../data/urbanformbr/cnefe/geo/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".gpkg")

  if (!file.exists(output_file)) {
    rds_dir <- paste0("../../data/urbanformbr/cnefe/geo/", muni_uf, "/")
    if (!dir.exists(rds_dir)) {
      dir.create(rds_dir, recursive = TRUE)
    }

    message(sprintf("Building geopackage for city %s - %s / %s", muni, muni_name, muni_uf))

    db_file <- paste0("../../data/urbanformbr/cnefe/db/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".rds")
    df_db <- read_rds(db_file) %>%
      mutate(muni_code = muni, muni_name = muni_name, muni_uf = muni_uf) %>%
      group_by(muni_code, muni_name, muni_uf, landuse_id) %>% summarise(n_addresses = n())

    # load by lat lon
    rds_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_latlon.rds")
    if (file.exists(rds_file)) {
      cnefe_latlon_sf <- read_rds(rds_file) %>%
        mutate(geocoding_method = "lat_lon") %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4674, agr = "constant")
    } else {
      cnefe_latlon_sf <- NULL
    }

    # load by logradouros
    rds_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_logradouro.rds")
    if (file.exists(rds_file)) {
      cnefe_log_sf <- read_rds(rds_file) %>%
        mutate(geocoding_method = "logradouros") %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4674, agr = "constant")
    } else {
      cnefe_log_sf <- NULL
    }

    # load by streetbase
    rds_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_streetbase_validated2.rds")
    if (file.exists(rds_file)) {
      cnefe_streetbase_sf <- read_rds(rds_file) %>%
        mutate(geocoding_method = "streetbase") %>%
        select(uf:post_code, code_tract = code_tract.x, geocoding_method, geometry)
    } else {
      cnefe_streetbase_sf <- NULL
    }

    cnefe_sf <- rbind(cnefe_latlon_sf, cnefe_log_sf, cnefe_streetbase_sf)

    st_write(cnefe_sf, output_file)


    cnefe_stats <- cnefe_sf %>%
      st_set_geometry(NULL) %>%
      mutate(muni_code = muni, muni_name = muni_name, muni_uf = muni_uf) %>%
      group_by(muni_code, muni_name, muni_uf, landuse_id, geocoding_method) %>%
      summarise(n_addresses = n()) %>%
      mutate(geocoding_method = factor(geocoding_method,
                                       levels = c("lat_lon", "logradouros", "streetbase"))) %>%
      complete(geocoding_method = geocoding_method, fill = list(n_addresses = 0)) %>%
      pivot_wider(names_from = geocoding_method, values_from = n_addresses)

    stats_df <- df_db %>%
      left_join(cnefe_stats) %>%
      mutate(n_missing = n_addresses - lat_lon - logradouros - streetbase,
             p_missing = n_missing / n_addresses)

    return(stats_df)

  } else {
    message(sprintf("Skipping %s / %s", muni_name, muni_uf))
    return(NULL)
  }

}

# apply function ----------------------------------------------------------

# process_urban_area(4301602)
codes <- unique(munis_df$code_muni)
geocoding_stats_df <- map(codes, combine_results)


