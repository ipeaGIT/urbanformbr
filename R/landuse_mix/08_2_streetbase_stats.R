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

  valid_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_streetbase_validated3.rds")
  invalid_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_streetbase_missing3.rds")

  if (file.exists(valid_file)) {

    message(sprintf("Loading %s - %s / %s", muni, muni_name, muni_uf))

    valid_sf <- read_rds(valid_file) %>% st_set_geometry(NULL)
    invalid_sf <- read_rds(invalid_file) %>% st_set_geometry(NULL)
    combined_sf <- rbind(valid_sf, invalid_sf)

    stats_df <- combined_sf %>%
      mutate(muni_code = municipality, muni_name = muni_name) %>%
      group_by(uf, muni_code, muni_name, valid, geocode_type, geocode_match, geocode_status) %>%
      summarise(min_score = min(geocode_score),
                p25_score = quantile(geocode_score, 0.25),
                mean_score = mean(geocode_score),
                median_score = median(geocode_score),
                p75_score = quantile(geocode_score, 0.75),
                max_score = max(geocode_score),
                n = n(),
                .groups = "drop"
                ) %>%
      mutate(p = n / sum(n))

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

write_csv(full_stats_df, file = "../../data/urbanformbr/cnefe/streetbase_stats.csv")
