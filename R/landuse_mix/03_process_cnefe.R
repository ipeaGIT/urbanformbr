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

process_urban_area <- function(urban_area_code) {
  if (!dir.exists("../../data/cnefe/")) { dir.create("../../data/cnefe/") }

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

  # extract urban area name and uf from data
  urban_area_name <- unique(subset(urban_areas, code_urban_concentration == urban_area_code)$name_urban_concentration)
  urban_area_name <- unlist(strsplit(urban_area_name, split = "/"))[1]
  urban_area_uf <- unique(subset(urban_areas, code_urban_concentration == urban_area_code & code_muni == urban_area_code)$abbrev_state)

  rds_file <- paste0("../../data/cnefe/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, ".rds")
  rds_file <- str_to_lower(rds_file)

  if (!file.exists(rds_file)) {
    message(sprintf("Working on city %s / %s", urban_area_name, urban_area_uf))

    # codes of municipalities that belong to urban concentration area
    code_munis <- subset(urban_areas, code_urban_concentration == urban_area_code)$code_muni
    code_ufs <- subset(urban_areas, code_urban_concentration == urban_area_code)$abbrev_state

    walk2(code_munis, code_ufs, function(muni, uf) {
      zip_files <- list.files(path = paste0("../../data-raw/cnefe/", uf), pattern = as.character(muni),
                              full.names = TRUE)
      lapply(zip_files, unzip, exdir = output_dir)
    })

    txt_files <- list.files(path = output_dir, pattern = ".TXT$", full.names = TRUE)
    cnefe_df <- map_df(txt_files, read_fwf, col_positions = col_positions, col_types = col_types)
    write_rds(cnefe_df, rds_file)

    file.remove(list.files(output_dir, pattern = ".TXT$", full.names = TRUE))
  }
}


# process_urban_area(4301602)
codes <- unique(urban_areas$code_urban_concentration)
walk(codes, process_urban_area)


### parse fixed width files and convert them to csv, with correct column names


cnefe_files <- list.files("../../data-raw/cnefe", pattern = ".zip", full.names = TRUE)

process_cnefe <- function(f) {
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

  message(sprintf("Processing file %s", f))

  f2 <- str_replace(f, ".zip", ".rds")
  if (!file.exists(f2)) {
    tryCatch({
      cnefe_df <- readr::read_fwf(f, col_positions = col_positions, col_types = col_types)

      write_rds(cnefe_df, f2, compress = "gz")
    },
    finally = { message(sprintf("Error processing file ", f))})
  }
}

# Apply function sequentially
purrr::walk(cnefe_files, process_cnefe)
# Apply function in parallel
furrr::future_walk(cnefe_files, process_cnefe, .progress =T)
