# description -------------------------------------------------------------

# Este script prepara o input para enviar ao ArcGIS StreetBase para geocoding

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# list of Brazilian municipalities
munis_df <- geobr::lookup_muni("all")

# muni <- 4301602 # Bagé

load_missing <- function(muni) {
  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  rds_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_missing.rds")

  if (file.exists(rds_file)) {
    cnefe_missing <- read_rds(rds_file) %>% setDT()

    cnefe_missing[, cnefe_id := sprintf("%s%s_%07d", uf, municipality, .I)]
    rds_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_missing_with_id.rds")
    write_rds(cnefe_missing, rds_file, compress = "gz")


    cnefe_missing[, address_a := fifelse(is.na(address_1),
                                         paste(place, address_2),
                                         paste(place, address_1, address_2))]

    cnefe_missing[, address_b := fifelse(is.na(modifier),
                                         house_number,
                                         paste(house_number, modifier))]

    cnefe_missing[, address2 := fifelse(is.na(complement1),
                                        "",
                                        fifelse(is.na(value_1),
                                                complement1,
                                                paste(complement1, value_1)))]

    cnefe_clean <- cnefe_missing[, .(cnefe_id, address_a, address_b, address2, borough, post_code, landuse_id, landuse_description)]
    cnefe_clean[, address_b := fifelse(address_b == "0 SN", "SN", address_b)]
    cnefe_clean[, address := paste0(address_a, ", ", address_b)]
    cnefe_clean[, address_a := NULL]
    cnefe_clean[, address_b := NULL]

    cnefe_clean[, city := muni_name]
    cnefe_clean[, state := muni_uf]
    cnefe_clean[, country := "Brazil"]



    setcolorder(cnefe_clean, c("cnefe_id", "country", "state", "city", "address", "address2", "borough", "post_code"))
    setnames(cnefe_clean, c("cnefe_id", "country", "state", "city", "address", "address2", "neighbourhood", "post_code", "landuse_id", "landuse_description"))

    return(cnefe_clean)
  } else {
    return(NULL)
  }
}

# geocode_muni(4301602)
codes <- unique(munis_df$code_muni)
cnefe_to_geocode <- map(codes, load_missing)

cnefe_to_geocode <- rbindlist(cnefe_to_geocode)

cnefe_to_geocode[, post_code := str_pad(post_code, side = "left", pad = "0", width = 8)]
cnefe_to_geocode[, post_code := paste0(substr(post_code, 1, 5), "-", substr(post_code, 6, 8)) ]

cnefe_to_geocode %>% write_rds("../../data/urbanformbr/cnefe/cnefe_to_streetbase.rds", compress = "gz")
cnefe_to_geocode %>% write_csv("../../data/urbanformbr/cnefe/cnefe_to_streetbase.csv")


