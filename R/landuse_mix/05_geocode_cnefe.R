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

output_dir <- tempdir()

parse_lat_lon <- function(coord) {
  if (is.na(coord)) { return(NA) }

  coord_parts <- strsplit(coord, split = " ") %>% unlist()
  d <- coord_parts[1] # degree
  m <- coord_parts[2] # minute
  s <- coord_parts[3] # second
  h <- coord_parts[4] # hemisphere

  if (h == "O") {h <- "W"}
  if (h == "L") {h <- "E"}

  dms <- sprintf("%sd%s'%s\" %s", d, m, s, h)
  dms <- sp::char2dms(dms)

  return(as.numeric(dms))
}
vparse_lat_lon <- Vectorize(FUN = parse_lat_lon, vectorize.args = "coord")


geocode_latlon <- function(urban_area_code) {
  if (!dir.exists("../../data/cnefe_geo/")) { dir.create("../../data/cnefe_geo/") }

  # extract urban area name and uf from data
  urban_area_name <- unique(subset(urban_areas, code_urban_concentration == urban_area_code)$name_urban_concentration)
  urban_area_name <- unlist(strsplit(urban_area_name, split = "/"))[1]
  urban_area_uf <- unique(subset(urban_areas, code_urban_concentration == urban_area_code & code_muni == urban_area_code)$abbrev_state)

  message(sprintf("Geocoding %s by lat/lon fields", urban_area_name))

  rds_file <- paste0("../../data/cnefe/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, ".rds")
  rds_file <- str_to_lower(rds_file)

  cnefe_df <- read_rds(rds_file) %>% setDT()

  cnefe_df <- cnefe_df[!is.na(latitude), ]

  cnefe_df[, latitude := vparse_lat_lon(latitude)]
  cnefe_df[, longitude := vparse_lat_lon(longitude)]

  rds_file <- paste0("../../data/cnefe_geo/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_latlon.rds")
  write_rds(cnefe_df, rds_file)
}

geocode_logradouro <- function(urban_area_code) {
  if (!dir.exists("../../data/cnefe_geo/")) { dir.create("../../data/cnefe_geo/") }

  # extract urban area name and uf from data
  urban_area_name <- unique(subset(urban_areas, code_urban_concentration == urban_area_code)$name_urban_concentration)
  urban_area_name <- unlist(strsplit(urban_area_name, split = "/"))[1]
  urban_area_uf <- unique(subset(urban_areas, code_urban_concentration == urban_area_code & code_muni == urban_area_code)$abbrev_state)

  message(sprintf("Geocoding %s by logradouros", urban_area_name))

    # load cnefe
  rds_file <- paste0("../../data/cnefe/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, ".rds")
  rds_file <- str_to_lower(rds_file)

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
  gpkg_file <- paste0("../../data/logradouros/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, ".gpkg")
  gpkg_file <- str_to_lower(gpkg_file)

  logradouros_sf <- st_read(gpkg_file) %>% setDT()

  # join cnefe to logradouros
  cnefe_missing <- cnefe_df[CD_GEO %nin% logradouros_sf$CD_GEO]
  cnefe_missing[, CD_GEO := NULL]
  rds_file <- paste0("../../data/cnefe_geo/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_missing.rds")
  write_rds(cnefe_missing, rds_file)



  cnefe_df <- cnefe_df[CD_GEO %in% logradouros_sf$CD_GEO]
  cnefe_df[logradouros_sf, on = "CD_GEO", geometry := i.geom]

  cnefe_df[, point := map(geometry, st_sample, size = 1)]

  cnefe_df[ ,latitude := map_dbl(point, function(x) st_coordinates(x)[[2]])]
  cnefe_df[ ,longitude := map_dbl(point, function(x) st_coordinates(x)[[1]])]

  cnefe_df[, CD_GEO := NULL]
  cnefe_df[, point := NULL]
  cnefe_df[, geometry := NULL]

  rds_file <- paste0("../../data/cnefe_geo/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_logradouros.rds")
  write_rds(cnefe_df, rds_file)
}

build_gpkg <- function(urban_area_code) {
  # extract urban area name and uf from data
  urban_area_name <- unique(subset(urban_areas, code_urban_concentration == urban_area_code)$name_urban_concentration)
  urban_area_name <- unlist(strsplit(urban_area_name, split = "/"))[1]
  urban_area_uf <- unique(subset(urban_areas, code_urban_concentration == urban_area_code & code_muni == urban_area_code)$abbrev_state)

  message(sprintf("Building CNEFE geopackage for city %s", urban_area_name))

    # load by lat lon
  rds_file <- paste0("../../data/cnefe_geo/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_latlon.rds")
  cnefe_latlon_df <- read_rds(rds_file)

  # load by logradouros
  rds_file <- paste0("../../data/cnefe_geo/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_logradouros.rds")
  cnefe_log_df <- read_rds(rds_file)

  cnefe_df <- rbind(cnefe_log_df, cnefe_latlon_df)

  cnefe_sf = st_as_sf(cnefe_df, coords = c("longitude", "latitude"), crs = 4674, agr = "constant")

  gpkg_file <- paste0("../../data/cnefe_geo/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_geo.gpkg")
  st_write(cnefe_sf, gpkg_file)
}

process_urban_area <- function(urban_area_code) {
  # extract urban area name and uf from data
  urban_area_name <- unique(subset(urban_areas, code_urban_concentration == urban_area_code)$name_urban_concentration)
  urban_area_name <- unlist(strsplit(urban_area_name, split = "/"))[1]
  urban_area_uf <- unique(subset(urban_areas, code_urban_concentration == urban_area_code & code_muni == urban_area_code)$abbrev_state)

  gpkg_file <- paste0("../../data/cnefe_geo/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, "_geo.gpkg")
  # gpkg_file <- str_to_lower(gpkg_file)

  if (!file.exists(gpkg_file)) {
    geocode_latlon(urban_area_code)
    geocode_logradouro(urban_area_code)
    build_gpkg(urban_area_code)
  }
}

# run functions -----------------------------------------------------------

codes <- unique(urban_areas$code_urban_concentration)
walk(codes, process_urban_area)

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


urban_areas %>% filter(code_urban_concentration == 2927408) %>%
  st_write("salvador_uc.gpkg")
  mapview()


