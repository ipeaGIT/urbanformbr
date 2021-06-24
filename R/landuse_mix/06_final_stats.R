# description -------------------------------------------------------------

# this script process the downloaded logradouros data, and aggregates them by
# urban concentration area

# setup -------------------------------------------------------------------

source('R/setup.R')

# list of Brazilian municipalities
munis_df <- geobr::lookup_muni("all")

# muni <- 4301602 # Bagé
# muni <- 2304400 # Fortaleza


check_muni <- function(muni) {
  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  message(sprintf("Processing %s / %s", muni_name, muni_uf))

  # file locations
  db_file <- paste0("../../data/urbanformbr/cnefe/db/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".rds")
  db_latlon <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_latlon.rds")
  db_logr <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_logradouro.rds")
  db_logr_2019 <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_logradouro_2019.rds")
  db_missing <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_missing.rds")
  db_missing_2019 <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_missing_2019.rds")

  geo_2010_file <- paste0("../../data/urbanformbr/cnefe/geo/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".gpkg")
  geo_2019_file <- paste0("../../data/urbanformbr/cnefe/geo/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_2019.gpkg")

  #' final data.table structure:
  #' muni code
  #' muni name
  #' muni uf
  #' landuse_id
  #' n_addresses
  #' by_latlon
  #' by_logr
  #' n_geo
  #' p_geo
  #' n_missing
  #' by_logr_2019
  #' n_geo_2019
  #' p_geo_2019
  #' n_missing_2019

  ### original geocoding
  df_db <- read_rds(db_file) %>%
    mutate(muni_code = muni, muni_name = muni_name, muni_uf = muni_uf) %>%
    group_by(muni_code, muni_name, muni_uf, landuse_id) %>% summarise(n_addresses = n())

  if (file.exists(db_latlon)) {
    df_latlon <- read_rds(db_latlon) %>%
      mutate(muni_code = muni, muni_name = muni_name, muni_uf = muni_uf) %>%
      group_by(muni_code, muni_name, muni_uf, landuse_id) %>% summarise(by_latlon = n())
  } else {
    df_latlon <- tribble(~muni_code, ~muni_name, ~muni_uf, ~landuse_id, ~by_latlon,
                         muni, muni_name, muni_uf, "01", 0)
  }

  if (file.exists(db_logr)) {
    df_logr <- read_rds(db_logr) %>%
      mutate(muni_code = muni, muni_name = muni_name, muni_uf = muni_uf) %>%
      group_by(muni_code, muni_name, muni_uf, landuse_id) %>% summarise(by_logr = n())
  } else {
    df_logr <- tribble(~muni_code, ~muni_name, ~muni_uf, ~landuse_id, ~by_logr,
                       muni, muni_name, muni_uf, "01", 0)
  }

  if (file.exists(db_missing)) {
    df_missing <- read_rds(db_missing) %>%
     mutate(muni_code = muni, muni_name = muni_name, muni_uf = muni_uf) %>%
     group_by(muni_code, muni_name, muni_uf, landuse_id) %>% summarise(n_missing = n())
  } else {
    df_missing <- tribble(~muni_code, ~muni_name, ~muni_uf, ~landuse_id, ~n_missing,
                          muni, muni_name, muni_uf, "01", 0)
  }

  if (file.exists(geo_2010_file)) {
    df_geo <- st_read(geo_2010_file) %>%
      st_set_geometry(NULL) %>%
      mutate(muni_code = muni, muni_name = muni_name, muni_uf = muni_uf) %>%
      group_by(muni_code, muni_name, muni_uf, landuse_id) %>% summarise(n_geo = n())
  } else {
    df_geo <- tribble(~muni_code, ~muni_name, ~muni_uf, ~landuse_id, ~n_geo,
                      muni, muni_name, muni_uf, "01", 0)
  }

  df_2010 <- df_db %>% left_join(df_geo) %>% left_join(df_latlon) %>%
    left_join(df_logr) %>% left_join(df_missing) %>%
    mutate(p_geo = n_geo / n_addresses) %>%
    mutate(across(.cols = everything(), replace_na, 0))


  ### geocoding using 2019 logradouros data
  if (file.exists(db_logr_2019)) {
    df_logr_2019 <- read_rds(db_logr_2019) %>%
      mutate(muni_code = muni, muni_name = muni_name, muni_uf = muni_uf) %>%
      group_by(muni_code, muni_name, muni_uf, landuse_id) %>% summarise(by_logr_2019 = n())
  } else {
    df_logr_2019 <- tribble(~muni_code, ~muni_name, ~muni_uf, ~landuse_id, ~by_logr_2019,
                         muni, muni_name, muni_uf, "01", 0)
  }

  if (file.exists(db_missing_2019)) {
    df_missing_2019 <- read_rds(db_missing_2019) %>%
      mutate(muni_code = muni, muni_name = muni_name, muni_uf = muni_uf) %>%
      group_by(muni_code, muni_name, muni_uf, landuse_id) %>% summarise(n_missing_2019 = n())
  } else {
    df_missing_2019 <- tribble(~muni_code, ~muni_name, ~muni_uf, ~landuse_id, ~n_missing_2019,
                         muni, muni_name, muni_uf, "01", 0)
  }

  df_2019 <- df_2010 %>% left_join(df_logr_2019) %>%
    mutate(across(.cols = everything(), replace_na, 0)) %>%
    mutate(n_geo_2019 = n_geo + by_logr_2019, p_geo_2019 = n_geo_2019 / n_addresses)

  return(df_2019)
}


codes <- unique(munis_df$code_muni)
geocoding_stats <- map_df(codes, check_muni)

stats_by_muni <- geocoding_stats %>%
  group_by(muni_code, muni_name, muni_uf) %>%
  summarise(across(.cols = n_addresses:p_geo_2019, sum))


stats_by_muni %>%
  ggplot() +
  geom_point(aes(n_addresses, n_geo))



# muni <- 1302553

# check_muni(1302553) %>% View()


# list all land uses ------------------------------------------------------

db_files <- list.files("../../data/urbanformbr/cnefe/db/",
                       pattern = ".rds$", full.names = TRUE, recursive = TRUE)
f <- db_files[1]

load_landuses <- function(f) {
  cnefe_df <- read_rds(f)


  cnefe_df <- cnefe_df[!is.na(landuse_description), .(landuse_id, landuse_description)]
  return(cnefe_df)
}

cnefe_landuses <- map(db_files, load_landuses)
cnefe_landuses <- rbindlist(cnefe_landuses)

cnefe_landuses %>% write_rds("../../data/urbanformbr/cnefe/cnefe_landuses.rds", compress = "gz")
cnefe_landuses %>% write_csv("../../data/urbanformbr/cnefe/cnefe_landuses.csv")



