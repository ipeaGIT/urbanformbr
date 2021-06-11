# description -------------------------------------------------------------

# this script process the downloaded logradouros data, and aggregates them by
# urban concentration area

# setup -------------------------------------------------------------------

source('R/setup.R')

urban_areas <- geobr::read_urban_concentrations()

# urban_area_code <- urban_areas$code_urban_concentration[[1]]
# urban_area_code <- 2211001

output_dir <- tempdir()

process_urban_area <- function(urban_area_code) {

  if (!dir.exists("../../data/logradouros/")) { dir.create("../../data/logradouros/") }

  # extract urban area name and uf from data
  urban_area_name <- unique(subset(urban_areas, code_urban_concentration == urban_area_code)$name_urban_concentration)
  urban_area_name <- unlist(strsplit(urban_area_name, split = "/"))[1]
  urban_area_uf <- unique(subset(urban_areas, code_urban_concentration == urban_area_code & code_muni == urban_area_code)$abbrev_state)

  sf_file <- paste0("../../data/logradouros/", urban_area_code, "_", urban_area_name, "_", urban_area_uf, ".gpkg")
  sf_file <- str_to_lower(sf_file)
  if (!file.exists(sf_file)) {
    message(sprintf("Working on city %s / %s", urban_area_name, urban_area_uf))

    # codes of municipalities that belong to urban concentration area
    code_munis <- subset(urban_areas, code_urban_concentration == urban_area_code)$code_muni
    code_ufs <- subset(urban_areas, code_urban_concentration == urban_area_code)$abbrev_state


    walk2(code_munis, code_ufs, function(muni, uf) {
      zip_files <- list.files(path = paste0("../../data-raw/logradouros/", uf), pattern = as.character(muni),
                              full.names = TRUE)
      lapply(zip_files, unzip, exdir = output_dir)
    })

    shp_files <- list.files(path = output_dir, pattern = "face.shp$", full.names = TRUE)
    faces_sf <- map_df(shp_files, st_read)
    st_write(faces_sf, sf_file)

    file.remove(list.files(output_dir, pattern = "face.shp", full.names = TRUE))
  }
}

# process_urban_area(4301602)
# process_urban_area(2211001)

codes <- unique(urban_areas$code_urban_concentration)
walk(codes, process_urban_area)





