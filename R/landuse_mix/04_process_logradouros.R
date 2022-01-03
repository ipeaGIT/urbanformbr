# description -------------------------------------------------------------

# this script process the downloaded logradouros data, and aggregates them by
# urban concentration area

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# list of Brazilian municipalities
munis_df <- geobr::lookup_muni("all")

# muni <- 4301602 # Bagé
# muni <- 2304400 # Fortaleza

output_dir <- tempdir()

process_muni_2010 <- function(muni) {

  # extract urban area name and uf from data
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_state)

  gpkg_file <- paste0("../../data/urbanformbr/faces_de_logradouros/2010/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".gpkg")


  if (!file.exists(gpkg_file)) {
    gpkg_dir <- paste0("../../data/urbanformbr/faces_de_logradouros/2010/", muni_uf, "/")
    if (!dir.exists(gpkg_dir)) {
      dir.create(gpkg_dir, recursive = TRUE)
    }

    message(sprintf("Working on city %s / %s", muni_name, muni_uf))

    zip_files <- list.files(path = paste0("../../data-raw/faces_de_logradouros/2010/", muni_uf, "/"),
                            pattern = as.character(muni),
                            full.names = TRUE)
    lapply(zip_files, unzip, exdir = output_dir)

    shp_files <- list.files(path = output_dir, pattern = "face.shp$", full.names = TRUE)
    faces_sf <- map_df(shp_files, st_read)
    st_write(faces_sf, gpkg_file)

    file.remove(list.files(output_dir, pattern = as.character(muni), full.names = TRUE))
  }

}

process_state_2019 <- function(uf) {
  zip_file <- paste0("../../data-raw/faces_de_logradouros/2019/", uf, "/", str_to_lower(uf), "_faces_de_logradouros_2019.zip")
  gpkg_dir <- paste0("../../data/urbanformbr/faces_de_logradouros/2019/", uf, "/")
  if (!dir.exists(gpkg_dir)) {
    dir.create(gpkg_dir, recursive = TRUE)
  }

  unzip(zip_file, exdir = output_dir)

  shp_files <- list.files(path = output_dir, pattern = ".shp$", full.names = TRUE)
  shp_names <- list.files(path = output_dir, pattern = ".shp$", full.names = FALSE)

  walk(1:length(shp_files) , function(index) {
    muni <- substr(shp_names[index], 1, 7)

    muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni)

    gpkg_file <- paste0("../../data/urbanformbr/faces_de_logradouros/2019/", uf, "/", muni, "_", muni_name, "_", uf, ".gpkg")
    if (!file.exists(gpkg_file)) {
      message(sprintf("Working on city %s / %s", muni_name, uf))
      faces_sf <- st_read(shp_files[index])

      st_write(faces_sf, gpkg_file)
    } else {
      message(sprintf("Skipping city %s / %s", muni_name, uf))
    }

  })

  file.remove(list.files(output_dir, pattern = "faces_de_logradouros_2019", full.names = TRUE))

}

# process_urban_area(4301602)
# process_urban_area(2211001)

codes <- unique(munis_df$code_muni)
walk(codes, process_muni_2010)

# process_state_2019("RS")

ufs <- unique(munis_df$abrev_state)
walk(ufs, process_state_2019)



