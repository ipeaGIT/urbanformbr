# description -------------------------------------------------------------

# Esse script processa os dados brutos do CNEFE e salva os resultados em
# formato rds, por município

# setup -------------------------------------------------------------------

source('R/setup.R')

library("geobr")

# cnefe file structure
col_widths <- c(2,5,2,2,4,1,20,30,60,8,7,20,10,20,10,20,10,20,10,20,10,20,10,15,15,60,60,2,40,1,30,3,3,8)
col_names<-c("uf"         , "municipality"   , "district"    , "sub_district", "tract"      , "urban_rural",
             "place"      , "address_1"      , "address_2"   , "house_number", "modifier"    ,
             "complement1", "value_1"        , "complement_2", "value_2"     , "complement_3", "value_3"    ,
             "complement4", "value_4"        , "complement_5", "value_5"     , "complement_6", "value_6"    ,
             "latitude"   , "longitude"      , "borough"     , "nil"         , "landuse_id"  , "landuse_description"   ,
             "multiple"   , "collective_name", "block_number", "face_number" , "post_code"
)
col_positions <- fwf_widths(col_widths, col_names)
col_types <- paste(rep("c", 34), collapse = "")

# list of Brazilian municipalities
munis_df <- geobr::lookup_muni("all")

# muni <- 4301602 # Bagé
muni <- 2304400 # Fortaleza

# localização da base de dados do CNEFE
### dados baixados pelo script 01_download_cnefe.R
# cnefe_folder_location <- "../../data/cnefe/"

### localização dos dados no servidor do IPEA
cnefe_folder_location <- "//storage6/bases/DADOS/PUBLICO/CNEFE"

output_dir <- tempdir()

# function ----------------------------------------------------------------

process_muni <- function(muni) {

  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  rds_file <- paste0("../../data/urbanformbr/cnefe/db/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".rds")

  if (!file.exists(rds_file)) {
    rds_dir <- paste0("../../data/urbanformbr/cnefe/db/", muni_uf, "/")
    if (!dir.exists(rds_dir)) {
      dir.create(rds_dir, recursive = TRUE)
    }

    message(sprintf("Working on city %s / %s", muni_name, muni_uf))

    zip_files <- list.files(path = paste0(cnefe_folder_location, "/", muni_uf, "/"), pattern = as.character(muni),
                              full.names = TRUE)

    lapply(zip_files, unzip, exdir = output_dir)
    txt_files <- list.files(path = output_dir, pattern = ".TXT$", full.names = TRUE)

    cnefe_df <- map_df(txt_files, read_fwf, col_positions = col_positions, col_types = col_types)
    setDT(cnefe_df)

    #add leading zeroes to geography codes
    cnefe_df[, municipality := sprintf("%05d", as.numeric(municipality))]
    cnefe_df[, district := sprintf("%02d", as.numeric(district))]
    cnefe_df[, sub_district := sprintf("%02d", as.numeric(sub_district))]
    cnefe_df[, tract := sprintf("%04d", as.numeric(tract))]

    cnefe_df[, code_tract := paste0(uf, municipality, district, sub_district, tract)]

    write_rds(cnefe_df, rds_file, compress = "gz")
    file.remove(txt_files)

  } else {
    message(sprintf("Skipping city %s / %s", muni_name, muni_uf))
  }
}


# apply function ----------------------------------------------------------

# process_urban_area(4301602)
codes <- unique(munis_df$code_muni)
walk(codes, process_muni)

