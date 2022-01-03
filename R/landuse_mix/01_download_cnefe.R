# description -------------------------------------------------------------

# Este script faz o download da base de dados do CNEFE / IBGE, referente ao
# censo de 2010

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')


# functions ---------------------------------------------------------------

## Função para listar arquivos em um servidor FTP
list_files_ftp <- function(ftp_url) {
  list_files <- curl::new_handle()
  curl::handle_setopt(list_files, ftp_use_epsv = TRUE, dirlistonly = TRUE)

  con <- curl::curl(url = ftp_url, "r", handle = list_files)
  files <- readLines(con)
  close(con)

  return(files)
}


## Função para baixar os dados de um Estado
download_cnefe_uf <- function(state_abbrev) {
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


# apply functions ---------------------------------------------------------

### Nota: os dados do CNEFE estão disponíveis na rede interna do IPEA, e não
### precisam ser baixados novamente.

# states_sf <- geobr::read_state()
# walk(states_sf$abbrev_state, download_cnefe_uf)
#
# download_cnefe_uf("RS")





