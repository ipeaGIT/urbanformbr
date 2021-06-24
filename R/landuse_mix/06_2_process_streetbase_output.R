# description -------------------------------------------------------------

# Este script prepara o input para enviar ao ArcGIS StreetBase para geocoding

# setup -------------------------------------------------------------------

source('R/setup.R')

# list of Brazilian municipalities
munis_df <- geobr::lookup_muni("all")

geocoding_results_df <- fread("../../data/urbanformbr/cnefe/cnefe_geo_streetmap_output.csv",
                              encoding = "UTF-8")
geocoding_results_df[, muni_code := substr(USER_cnefe_id, 1, 7)]

separate_output <- function(muni) {
  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  rds_input_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_missing_with_id.rds")
  rds_output_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_streetbase.rds")

  if (file.exists(rds_input_file)) { # & !file.exists(rds_output_file)) {
    # filter streetbase output by municipality
    streetbase_muni <- geocoding_results_df[muni_code == muni, ]

    # load original data sent to streetbase
    missing_df <- read_rds(rds_input_file)

    # join with streetbase output
    missing_df[streetbase_muni, on=.(cnefe_id == USER_cnefe_id),
               `:=`(latitude = i.Y, longitude = i.X, geocode_type = i.Addr_type)]

    write_rds(missing_df, rds_output_file, compress = "gz")
  }
}

# separate_output("4301602")
# separate_output("4314407")
codes <- unique(munis_df$code_muni) %>% as.character()
walk(codes, separate_output)
# separate_output("4314407")
#
# geocoding_results_df[] %>%
#   mapview(xcol="X", ycol = "Y", crs = 4326)
#
#
# # muni <- 4301602 # Bagé
# # muni <- 4314407 # Pelotas

# geocoding_results_df$USER_cnefe_id[1]
#
#
# geocoding_results_df[str_starts(USER_cnefe_id, "4314407"), ] %>%
#   mapview(xcol="X", ycol = "Y", crs = 4326)
# muni <- "4301602"
# bage_df <- fread(csv_file, encoding = "UTF-8")

# bage_df %>%
#   count(Status, sort = TRUE)



validate_streetbase <- function(muni) {
  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  rds_input_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_streetbase.rds")
  rds_output_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_streetbase_validated.rds")
  rds_missing_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_streetbase_missing.rds")

  if (file.exists(rds_input_file) & !file.exists(rds_output_file)) {
    message(sprintf("Processing city %s - %s / %s", muni, muni_name, muni_uf))

        # load census tracts for municipality
    # it has to be the original resolution data (simplified = FALSE), for accuracy
    census_tracts <- geobr::read_census_tract(code_tract = muni, simplified = FALSE, 2010) %>%
      st_transform(crs = 4674)


    streetbase_df <- read_rds(rds_input_file)
    streetbase_sf = streetbase_df %>%
      filter(!is.na(longitude)) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
      st_transform(crs = 4674)


    # for performance, checks will be made in steps

    # 1st: spatial join between geolocated points and census tracts
    #      points that fall inside correct census tract are considered valid
    #      points that do not intersect at all with any census tract are invalid

    streetbase_sf <- st_join(streetbase_sf, census_tracts) %>% mutate(valid = (code_tract.x == code_tract.y))

    valid_sf <- streetbase_sf %>% filter(valid == TRUE) %>% select(-valid)
    invalid_sf <- streetbase_sf %>% filter(is.na(code_tract.y))

    # 2nd: check the points that are inside the city, but in the wrong census tract
    #      calculate the distance from the point to the correct census tract, and
    #      use a threshold distance to evaluate correctedness

    streetbase_sf <- streetbase_sf %>% filter(valid == FALSE) %>% select(-valid)

    setDT(streetbase_sf)
    setDT(census_tracts)

    streetbase_sf[census_tracts, on = .(code_tract.x == code_tract), tract_geom := i.geom]

    points_sf <- streetbase_sf[, .(code_tract.x, cnefe_id, geometry)] %>% st_as_sf()
    tracts_sf <- streetbase_sf[, .(code_tract.x, cnefe_id, tract_geom)] %>% st_as_sf()


    if (nrow(points_sf) > 0) {
      point_distances <- st_distance(points_sf, tracts_sf, by_element = TRUE) %>% as.numeric()
      streetbase_sf$distance_to_tract <- point_distances
    } else {
      streetbase_sf$distance_to_tract <- 0
    }


    valid_sf <- rbind(valid_sf,
                      streetbase_sf[distance_to_tract <= 500] %>% st_as_sf() %>%
                        select(-tract_geom, -distance_to_tract)
                      )

    invalid_sf <- rbind(invalid_sf %>% select(-valid),
                        streetbase_sf[distance_to_tract > 500] %>% st_as_sf() %>%
                          select(-tract_geom, -distance_to_tract)
                        )

    # save validated data and still missing data
    write_rds(valid_sf, rds_output_file, compress = "gz")
    write_rds(invadlid_sf, rds_missing_file, compress = "gz")

  }
}

validate_streetbase(4314407)
validate_streetbase(4301602)
validate_streetbase(2305100)

codes <- unique(munis_df$code_muni)
walk(codes, validate_streetbase)

muni <- 2110807
muni <- 2305100

# set number of cores
options(mc.cores=12)

# Apply function in parallel
options( future.globals.maxSize = 10000 * 1024 ^ 2 )
future::plan(strategy = 'multisession', workers=12)

# Apply function in parallel
codes <- unique(munis_df$code_muni)
system.time( furrr::future_walk(.x=codes,
                                .f=validate_streetbase,
                                .progress =T) )


get_codes <- function() {
  codes <- unique(munis_df$code_muni)

  codes <- map(codes, function(code) {
    # extract urban area name and uf from data
    muni_name <- unique(subset(munis_df, code_muni == code)$name_muni)
    muni_uf <- unique(subset(munis_df, code_muni == code)$abrev_state)

    rds_output_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", code, "_", muni_name, "_", muni_uf, "_streetbase_validated.rds")

    if (!file.exists(rds_output_file)) {
      return(code)
    } else {
      return (NULL)
    }

  })

  munis <- codes[!sapply(codes, is.null)]

  return(munis)

}

codes <- get_codes()

# census_tracts %>% mapview()
#
# point_distances %>% View()
#
# streetbase_sf %>% select(-tract_geom) %>% mutate(distance_to_tract = as.double(distance_to_tract)) %>%
#   filter(distance_to_tract > 0, distance_to_tract < 5000) %>%
#   ggplot(aes(distance_to_tract)) + geom_histogram()
#
#
#
# streetbase_sf %>%
#   st_set_geometry(NULL) %>%
#   count(valid)
#
# census_tracts <- st_as_sf(census_tracts)
#
# streetbase_sf %>% mapview(zcol="geocode_type")
# streetbase_sf %>% mapview(zcol="valid")
# mapview(census_tracts) + (streetbase_sf[distance_to_tract <= 500] %>% st_as_sf() %>% mapview(zcol="distance_to_tract"))
#
# streetbase_sf[distance_to_tract <= 1000]
# streetbase_sf[distance_to_tract > 1000]
# median(streetbase_sf$distance_to_tract)
#
# valid_sf %>% mapview()




# version 2 ---------------------------------------------------------------

validate_streetbase_2 <- function(muni) {
  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  rds_input_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_streetbase.rds")
  rds_output_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_streetbase_validated2.rds")
  rds_missing_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_streetbase_missing2.rds")

  if (file.exists(rds_input_file) & !file.exists(rds_output_file)) {
    message(sprintf("Processing city %s - %s / %s", muni, muni_name, muni_uf))

    # load census tracts for municipality
    # it has to be the original resolution data (simplified = FALSE), for accuracy
    census_tracts <- geobr::read_census_tract(code_tract = muni, simplified = FALSE, 2010) %>%
      st_transform(crs = 4674) %>%
      st_buffer(0.003)


    streetbase_df <- read_rds(rds_input_file)
    streetbase_sf = streetbase_df %>%
      filter(!is.na(longitude)) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
      st_transform(crs = 4674)


    # for performance, checks will be made in steps

    # 1st: spatial join between geolocated points and census tracts
    #      points that fall inside correct census tract are considered valid
    #      points that do not intersect at all with any census tract are invalid

    streetbase_join_sf <- st_join(streetbase_sf, census_tracts) %>% mutate(valid = (code_tract.x == code_tract.y))

    valid_sf <- streetbase_join_sf %>% filter(valid == TRUE) %>% select(-valid)
    invalid_sf <- streetbase_sf %>% filter(!(cnefe_id %in% valid_sf$cnefe_id))

   # save validated data and still missing data
    write_rds(valid_sf, rds_output_file, compress = "gz")
    write_rds(invalid_sf, rds_missing_file, compress = "gz")

  }
}

# Apply function in parallel
codes <- unique(munis_df$code_muni)
walk(codes, validate_streetbase_2)

system.time( furrr::future_walk(.x=codes,
                                .f=validate_streetbase_2,
                                .progress =T) )

validate_streetbase_2(4314407)
validate_streetbase_2(4301602)
validate_streetbase_2(2305100)
validate_streetbase_2(4314902)
muni <- 4314902
