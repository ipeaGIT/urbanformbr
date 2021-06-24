# description -------------------------------------------------------------

# Este scripot faz o download da base de dados de  faces de logradouros do
# IBGE, referentes ao censo 2010 e à atualização de 2019 feita em preparação
# para o próxiom censo

# setup -------------------------------------------------------------------

source('R/setup.R')

library("curl")
library("rvest")


# functions ---------------------------------------------------------------

download_logradouros_2010 <- function(state_abbrev) {
  logradouros_url <- "https://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/malha_de_setores_censitarios/censo_2010/base_de_faces_de_logradouros_versao_2010/"
  logradouros_html <- rvest::read_html(paste0(logradouros_url, state_abbrev))
  logradouros_df <- html_elements(logradouros_html, "table") %>% html_table()
  logradouros_df <- logradouros_df[[1]]
  logradouros_df <- logradouros_df[stringr::str_detect(logradouros_df$Name, pattern = "zip"), ]

  download_urls <- paste0(logradouros_url, state_abbrev, "/", logradouros_df$Name)

  # make sure local directory exists
  download_dir = sprintf("../../data-raw/faces_de_logradouros/2010/%s", state_abbrev)
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }

  download_files <- paste0(download_dir, "/", logradouros_df$Name)

  walk2(download_urls, download_files, function(url, file) {
    if (!file.exists(file)) {
      curl::curl_download(url = url, destfile = file, quiet = FALSE)
    }
  })
}

download_logradouros_2019 <- function(state_abbrev) {
  logradouros_url <- "https://geoftp.ibge.gov.br/recortes_para_fins_estatisticos/malha_de_setores_censitarios/censo_2010/base_de_faces_de_logradouros_versao_2019/"

  download_url <- paste0(logradouros_url,
                         state_abbrev, "/",
                         str_to_lower(state_abbrev),
                         "_faces_de_logradouros_2019.zip")

  # make sure local directory exists
  download_dir = sprintf("../../data-raw/faces_de_logradouros/2019/%s", state_abbrev)
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }

  download_file <- paste0(download_dir, "/",
                          str_to_lower(state_abbrev),
                          "_faces_de_logradouros_2019.zip")

  if (!file.exists(download_file)) {
    curl::curl_download(url = download_url, destfile = download_file, quiet = FALSE)
  }
}


# apply functions ---------------------------------------------------------

states_sf <- geobr::read_state()

walk(states_sf$abbrev_state, download_logradouros_2010)
walk(states_sf$abbrev_state, download_logradouros_2019)

