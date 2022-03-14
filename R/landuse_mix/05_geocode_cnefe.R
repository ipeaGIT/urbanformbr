# description -------------------------------------------------------------

# this script process the downloaded logradouros data, and aggregates them by
# urban concentration area

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# list of Brazilian municipalities
munis_df <- geobr::lookup_muni("all")

# muni <- 4301602 # Bag?
# muni <- 2304400 # Fortaleza


# support functions -------------------------------------------------------

parse_lat_lon <- function(coord, hemi = "W") {
  if (is.na(coord)) { return(NA) }

  coord_parts <- strsplit(coord, split = " ") %>% unlist()
  d <- coord_parts[1] # degree
  m <- coord_parts[2] # minute
  s <- coord_parts[3] # second

  if (length(coord_parts) == 3) {
    h <- hemi # hemisphere
  } else {
    h <- coord_parts[4] # hemisphere
  }

  if (h == "O") {h <- "W"}
  if (h == "L") {h <- "E"}

  dms <- sprintf("%sd%s'%s\" %s", d, m, s, h)
  dms <- sp::char2dms(dms)

  return(as.numeric(dms))
}
vparse_lat_lon <- Vectorize(FUN = parse_lat_lon, vectorize.args = "coord")


# functions ---------------------------------------------------------------

geocode_latlon <- function(muni) {
  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  output_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_latlon.rds")

  if (!file.exists(output_file)) {
    rds_dir <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/")
    if (!dir.exists(rds_dir)) {
      dir.create(rds_dir, recursive = TRUE)
    }

    message(sprintf("Geocoding %s / %s by lat/lon fields", muni_name, muni_uf))

    input_file <- paste0("../../data/urbanformbr/cnefe/db/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".rds")

    cnefe_df <- read_rds(input_file) %>% setDT()
    cnefe_df <- cnefe_df[!is.na(latitude), ]

    cnefe_df[, latitude := vparse_lat_lon(latitude, hemi = "S")]
    cnefe_df[, longitude := vparse_lat_lon(longitude, hemi = "W")]

    write_rds(cnefe_df, output_file, compress = "gz")
  } else {
    # message(sprintf("Skipping %s / %s", muni_name, muni_uf))
  }

}

geocode_logradouro <- function(muni) {
  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  output_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_logradouro.rds")

  if (!file.exists(output_file)) {
    rds_dir <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/")
    if (!dir.exists(rds_dir)) {
      dir.create(rds_dir, recursive = TRUE)
    }

    message(sprintf("Geocoding %s / %s by logradouros", muni_name, muni_uf))

    input_file <- paste0("../../data/urbanformbr/cnefe/db/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".rds")

    cnefe_df <- read_rds(input_file) %>% setDT()
    cnefe_df <- cnefe_df[is.na(latitude), ]

    # build logradouro identifier
    cnefe_df[, CD_GEO := paste0(code_tract, block_number, face_number)]

    # load logradouros
    gpkg_file <- paste0("../../data/urbanformbr/faces_de_logradouros/2010/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".gpkg")
    logradouros_sf <- st_read(gpkg_file)# %>% setDT()
    # logradouros_sf <- st_make_valid(logradouros_sf)
    # is_correct <- st_is(logradouros_sf, type = "MULTILINESTRING")
    # logradouros_sf <- logradouros_sf[is_correct, ]
    setDT(logradouros_sf)

    # find addresses with missing link in logradouros
    cnefe_missing <- cnefe_df[CD_GEO %nin% logradouros_sf$CD_GEO]
    cnefe_missing[, CD_GEO := NULL]
    rds_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_missing.rds")
    write_rds(cnefe_missing, rds_file, compress = "gz")

    # join cnefe to logradouros
    cnefe_df <- cnefe_df[CD_GEO %in% logradouros_sf$CD_GEO]
    if (nrow(cnefe_df) > 0) {
      cnefe_df[logradouros_sf, on = "CD_GEO", geometry := i.geom]

      cnefe_df[, point := map(geometry, st_sample, size = 1)]

      cnefe_df[ ,latitude := map_dbl(point, function(x) st_coordinates(x)[[2]])]
      cnefe_df[ ,longitude := map_dbl(point, function(x) st_coordinates(x)[[1]])]

      cnefe_df[, CD_GEO := NULL]
      cnefe_df[, point := NULL]
      cnefe_df[, geometry := NULL]

      write_rds(cnefe_df, output_file, compress = "gz")

    }

  } else {
    # message(sprintf("Skipping %s / %s", muni_name, muni_uf))
  }

}


build_gpkg <- function(muni) {
  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  output_file <- paste0("../../data/urbanformbr/cnefe/geo/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".gpkg")

  if (!file.exists(output_file)) {
    rds_dir <- paste0("../../data/urbanformbr/cnefe/geo/", muni_uf, "/")
    if (!dir.exists(rds_dir)) {
      dir.create(rds_dir, recursive = TRUE)
    }

    message(sprintf("Building geopackage for city %s / %s", muni_name, muni_uf))

    # load by lat lon
    rds_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_latlon.rds")
    cnefe_latlon_df <- read_rds(rds_file)

    # load by logradouros
    rds_file <- paste0("../../data/urbanformbr/cnefe/geo/partial/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, "_logradouro.rds")
    cnefe_log_df <- read_rds(rds_file)

    cnefe_df <- rbind(cnefe_log_df, cnefe_latlon_df)

    cnefe_sf = st_as_sf(cnefe_df, coords = c("longitude", "latitude"), crs = 4674, agr = "constant")

    st_write(cnefe_sf, output_file)

  } else {
    # message(sprintf("Skipping %s / %s", muni_name, muni_uf))
  }

}


geocode_muni_simple <- function(muni) {
  geocode_latlon(muni)
  geocode_logradouro(muni)
}

geocode_muni <- function(muni) {
  tryCatch(
    {
      geocode_latlon(muni)
      geocode_logradouro(muni)
      build_gpkg(muni)

      geocode_missing_by_logradouro_2019(muni)
      build_pkg_2019(muni)
    },
  error = function(cond) {
    message(cond)
  })
}

# apply function ----------------------------------------------------------

# future::plan(strategy = 'multisession', workers=10)

# geocode_muni(4301602)
codes <- unique(munis_df$code_muni)
walk(codes, geocode_muni)
walk(codes, geocode_muni_simple)

# furrr::future_walk(.x=codes,
#                    .f=geocode_muni,
#                    .progress =T)

# geocode_muni(4301602) # Bagé
# geocode_muni(2304400) # Fortaleza
# geocode_muni(3304557) # Rio de Janeiro
# muni <- 4301602 # Bagé
# muni <- 2304400 # Fortaleza
# muni <- 3304557 # Rio de Janeiro


# process_urban_area(2304400) # Fortaleza
# process_urban_area(4314407) # Pelotas
# urban_area_code <- 4314407
#
# rds_file <- paste0("../../data/cnefe_geo/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_missing.rds")
#
# pelotas_missing_df <- read_rds(rds_file)
#
#
# "https://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/malha_de_setores_censitarios/censo_2010/base_de_faces_de_logradouros_versao_2019/RS/rs_faces_de_logradouros_2019.zip"




