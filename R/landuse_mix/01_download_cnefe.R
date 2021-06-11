# description -------------------------------------------------------------

# this script downloads, unzips and saves data from CNEFE / IBGE datasets

# setup -------------------------------------------------------------------

source('R/setup.R')

list_files_ftp <- function(ftp_url) {
  list_files <- curl::new_handle()
  curl::handle_setopt(list_files, ftp_use_epsv = TRUE, dirlistonly = TRUE)

  con <- curl::curl(url = ftp_url, "r", handle = list_files)
  files <- readLines(con)
  close(con)

  return(files)
}

states_sf <- geobr::read_state()

# state_abbrev <- "AC"

download_cnefe <- function(state_abbrev) {
  # CNEFE FTP server download URL
  cnefe_ftp_server <- "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Cadastro_Nacional_de_Enderecos_Fins_Estatisticos/"


  files <- list_files_ftp(paste0(cnefe_ftp_server, state_abbrev, "/"))
  files <- files[nchar(files) > 6]

  download_dir = sprintf("../../data-raw/cnefe/%s/", state_abbrev)
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }

  download_urls <- paste0(cnefe_ftp_server, state_abbrev, "/", files)
  download_files <- paste0(download_dir, files)

  walk2(download_urls, download_files, function(url, file) {
    if (!file.exists(file)) {
      curl::curl_download(url = url, destfile = file, quiet = FALSE)
    }
  })
}

walk(states_sf$abbrev_state, download_cnefe)

















# old code ----------------------------------------------------------------


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

parse_lat_lon("07 21 19.4040 S")
parse_lat_lon("73 11 12.0180 O")
parse_lat_lon(NA)

# download raw data -------------------------------------------------------

## cnefe ------------------------------------------------------------------

### download cnefe land use data, located on an IBGE FTP server
states_sf <- geobr::read_state()

download_cnefe <- function(state_code, state_abbrev) {
  # make sure local directory exists
  download_dir = sprintf("../../data-raw/cnefe/")
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }

  # FTP server address
  cnefe_ftp_server <- "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Cadastro_Nacional_de_Enderecos_Fins_Estatisticos"

  # UF file address
  uf_data_url <- sprintf("%s/%s/%s.zip", cnefe_ftp_server, state_abbrev, state_code)

  # download cnefe data for state, if local file does not exist
  download_url <- paste0("../../data-raw/cnefe/", state_abbrev, ".zip")
  if (!file.exists(download_url)) {
    curl::curl_download(url = uf_data_url, destfile = download_url, quiet = FALSE)
  }
}

purrr::walk2(states_sf$code_state, states_sf$abbrev_state, download_cnefe)



process_cnefe(cnefe_files[6])

# gc()

acre_df <- readr::read_fwf("../../data-raw/cnefe/AC.zip", col_positions = col_positions, col_types = col_types)
setDT(acre_df)

map_dbl(acre_df$latitude, parse_lat_lon)
acre_df[, latitude := vparse_lat_lon(latitude)]
acre_df[!is.na(longitude), longitude := vparse_lat_lon(longitude)]

acre_df[, latitude := parzer::parse_lat(latitude)]

acre_df[!is.na(longitude)][1:100] %>% mapview(xcol="longitude", ycol="latitude", crs = 4674)

coord <- "07 21 19.4040 S"
coord <- "73 11 12.0180 O"

View()
logradouros <- rvest::read_html("https://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/malha_de_setores_censitarios/censo_2010/base_de_faces_de_logradouros_versao_2010/ES/")
logra_tables <- html_elements(logradouros, "table") %>%  html_table()



states <- RCurl::getURL(cnefe_ftp_server, verbose=TRUE, ftp.use.epsv=FALSE, dirlistonly=TRUE, crlf=TRUE)
states <- strsplit(states, split = "\r*\n") %>% unlist()
states <- states[nchar(states) == 2]

uf <- states[1]
uf <- "SP"
uf_url <- paste0(cnefe_ftp_server, uf, "/")

uf_data_url <- RCurl::getURL(uf_url, verbose=TRUE, ftp.use.epsv=FALSE, dirlistonly=TRUE, crlf=TRUE)
uf_data_url <- strsplit(uf_data_url, split = "\r*\n") %>% unlist()
uf_data_url <- uf_data_url[nchar(uf_data_url) == 6]





url_exists <- function(url) {
  return(RCurl::url.exists(url))
  # return(httr::status_code(httr::HEAD(url = url)) == 200)
}

file.exists(zip_file_url)
download.file(zip_file_url, tempfile())

# sub function to download data from specific urban area
download_cnefe_urban_area <- function(urban_area_code, urban_areas) {
  # extract urba area name and uf from data
  urban_area_name <- unique(subset(urban_areas, code_urban_concentration == urban_area_code)$name_urban_concentration)
  urban_area_name <- unlist(strsplit(urban_area_name, split = "/"))[1]
  urban_area_uf <- unique(subset(urban_areas, code_urban_concentration == urban_area_code & code_muni == urban_area_code)$abbrev_state)

  message(sprintf("Working on city %s / %s", urban_area_name, urban_area_uf))

  # codes of municipalities that belong to urban concentration area
  code_munis <- subset(urban_areas, code_urban_concentration == urban_area_code)$code_muni

  # download census tracts from all municipalities
  census_tracts <- lapply(code_munis, geobr::read_census_tract)
  census_tracts <- rbindlist(census_tracts)

  # get subdistricts from census tracts. CNEFE data is provided by subdistrict
  census_subdistricts <- unique(census_tracts$code_subdistrict)

  # CNEFE FTP server download URL
  cnefe_ftp_server <- "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Cadastro_Nacional_de_Enderecos_Fins_Estatisticos/"
  download_dir = sprintf("../../data-raw/cnefe/%s/%s/", urban_area_uf, urban_area_name)
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }

  # subdistrict <- census_subdistricts[1]
  # subdistrict <- 32053090500
  a <- lapply(census_subdistricts, function(subdistrict) {
    zip_file_url <- paste(cnefe_ftp_server, urban_area_uf, "/", subdistrict, ".zip", sep = "")
    download_url <- paste0("../../data-raw/cnefe/", urban_area_uf, "/", urban_area_name, "/", subdistrict, ".zip")
    txt_file_url <- paste("../../data-raw/cnefe/", urban_area_uf, "/", urban_area_name, "/", subdistrict, "TXT")

    message(sprintf("Subdistrict %s", subdistrict))
    if (url_exists(zip_file_url)) {
      message(sprintf("Downloading CNEFE %s", zip_file_url))
      if (!file.exists(download_url)) {
        curl::curl_download(url = zip_file_url, destfile = download_url, quiet = FALSE)

        unzip(download_url, exdir = download_dir)
      }
    }
  })

  logradouros_ftp_server <- "https://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/malha_de_setores_censitarios/censo_2010/base_de_faces_de_logradouros_versao_2010/"
  download_dir = sprintf("../../data-raw/logradouros/%s/%s/", urban_area_uf, urban_area_name)
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }


  a <- lapply(census_subdistricts, function(subdistrict) {
    zip_file_url <- paste0(logradouros_ftp_server, urban_area_uf, "/", subdistrict, ".zip")
    download_url <- paste0("../../data-raw/logradouros/", urban_area_uf, "/", urban_area_name, "/", subdistrict, ".zip")

    message(sprintf("Subdistrict %s", subdistrict))
    if (url_exists(zip_file_url)) {
      message(sprintf("Downloading logradouros %s", zip_file_url))
      if (!file.exists(download_url)) {
        curl::curl_download(url = zip_file_url, destfile = download_url, quiet = FALSE)

        unzip(download_url, exdir = download_dir)
      }
    }
  })



}

download_cnefe <- function(urban_areas) {


  lapply(unique(urban_areas$code_urban_concentration), download_cnefe_urban_area, urban_areas)

}

urban_concentration_areas <- geobr::read_urban_concentrations()

download_cnefe(urban_concentration_areas)

urban_area_code <- 4301602
urban_area_code <- 4322400
urban_area_code <- 4302105
urban_area_code <- 4313409
urban_area_code <- 3205309

urban_areas <- urban_concentration_areas
download_cnefe_urban_area(urban_area_code, urban_concentration_areas)

colWidths <- c(2,5,2,2,4,1,20,30,60,8,7,20,10,20,10,20,10,20,10,20,10,20,10,15,15,60,60,2,40,1,30,3,3,8)

colNames<-c( "UF"         , "Municipality"  , "District"   , "SubDistrict", "Sector"     , "UrbanRural",
             "Place"      , "Address1"      , "Address2"   , "HouseNumber", "Modifier"   ,
             "Complement1", "Value1"        , "Complement2", "Value2"     , "Complement3", "Value3"    ,
             "Complement4", "Value4"        , "Complement5", "Value5"     , "Complement6", "Value6"    ,
             "Latitude"   , "Longitude"     , "Borough"    , "Nil"        , "LandUseId"  , "LandUse"   ,
             "Multiple"   , "CollectiveName", "BlockNumber", "FaceNumber" , "PostCode"
)




# Faces de Logradouros ----------------------------------------------------

url_exists("https://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/malha_de_setores_censitarios/censo_2010/base_de_faces_de_logradouros_versao_2010/AC/12000130500.zip")

url_exists("https://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/malha_de_setores_censitarios/censo_2010/base_de_faces_de_logradouros_versao_2010/ES/32052000506.zip")

h = new_handle(dirlistonly=TRUE)
con <- curl::curl("https://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/malha_de_setores_censitarios/censo_2010/base_de_faces_de_logradouros_versao_2010/ES/", "r", h)
tbl = read.table(con, stringsAsFactors=TRUE, fill=TRUE)
tbl %>% View()
close(con)

logradouros <- rvest::read_html("https://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/malha_de_setores_censitarios/censo_2010/base_de_faces_de_logradouros_versao_2010/ES/")
logra_tables <- html_elements(logradouros, "table") %>%  html_table()
logra_tables <- logra_tables[[1]]


state_abbrev <- "RS"
logradouros_url <- "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Cadastro_Nacional_de_Enderecos_Fins_Estatisticos/RS/"
list.files(logradouros_url)

curl::curl(logradouros_url)
logradouros_html <- rvest::read(paste0(logradouros_url, state_abbrev))
logradouros_df <- html_elements(logradouros_html, "table") %>% html_table()
logradouros_df <- logradouros_df[[1]]
logradouros_df <- logradouros_df[str_detect(logradouros_df$Name, pattern = "zip"), ]

ftp_base <- "ftp://"
list_files <- curl::new_handle()
curl::handle_setopt(list_files, ftp_use_epsv = TRUE, dirlistonly = TRUE)

con <- curl::curl(url = logradouros_url, "r", handle = list_files)
files <- readLines(con)
close(con)


