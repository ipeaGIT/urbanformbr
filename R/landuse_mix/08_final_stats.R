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

compute_stats <- function(muni) {
  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  input_file <- paste0("../../data/urbanformbr/cnefe/geo/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".gpkg")

  if (file.exists(input_file)) {

    message(sprintf("Loading %s - %s / %s", muni, muni_name, muni_uf))

    db_file <- paste0("../../data/urbanformbr/cnefe/db/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".rds")
    df_db <- read_rds(db_file) %>%
      mutate(muni_code = muni, muni_name = muni_name, muni_uf = muni_uf) %>%
      group_by(muni_code, muni_name, muni_uf, landuse_id) %>% summarise(n_addresses = n())


    cnefe_sf <- st_read(input_file)
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
      left_join(cnefe_stats)
    if (!("lat_lon" %in% names(stats_df))) {stats_df$lat_lon <- 0}
    if (!("logradouros" %in% names(stats_df))) {stats_df$logradouros <- 0}
    if (!("streetbase" %in% names(stats_df))) {stats_df$streetbase <- 0}

    stats_df <- stats_df %>%
      mutate(n_missing = n_addresses - lat_lon - logradouros - streetbase,
             p_missing = n_missing / n_addresses)


    return(stats_df)


  } else {
    return(NULL)
  }

}



# apply function ----------------------------------------------------------

# process_urban_area(4301602)
codes <- unique(munis_df$code_muni)
geocoding_stats_df <- map(codes, compute_stats)

full_stats_df <- rbindlist(geocoding_stats_df)

write_csv(full_stats_df, file = "../../data/urbanformbr/cnefe/geocoding_stats.csv")

compute_stats(2930709)
compute_stats(4301602)
