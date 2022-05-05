# description -------------------------------------------------------------

# this script downloads, unzips and saves data from the GHSL datasets:
## GHS-FUA R2019A: Functional Urban Areas (derived from UCDB R2019A)
## GHS-UCDB R2019A: Urban Centre Database
## GHS-BUILT R2018A: Built-Up Area grid, Landsat
## GHS-POP R2019A: Population grid
## GHS-SMOD R2019A: Settlement layers (degree of urbanization)...
#...from GHS-POP and BUILT

# OBs.: layers's temporal dimensions
## GHS-FUA R2019A: 2015
## GHS-UCDB R2019A: multitemporal (1975-1990-2000-2014)
## GHS-BUILT R2018A: multitemporal (1975-1990-2000-2014)
## GHS-POP R2019A: multitemporal (1975-1990-2000-2015)
## GHS-SMOD R2019A: multitemporal (1975-1990-2000-2015)

# Obs.2: some layers have multiple resolutions available

# steps:
# i. download data
# ii. unzip and save data at //storage6/usuarios/Proj_acess_oport/data-raw

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# directory ---------------------------------------------------------------

if (!dir.exists("../../data-raw/ghsl")){
  dir.create("../../data-raw/ghsl")
}

ghsl_dir <- "../../data-raw/ghsl"


# download, unzip and save data-raw ---------------------------------------

# create temp file
temp <- tempfile()

# fua ucdb ----------------------------------------------------------------
# in comments since it has already been done

# * fua -------------------------------------------------------------------


#download.file('http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_FUA_UCDB2015_GLOBE_R2019A/V1-0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.zip',
#              temp)
#unzip(zipfile = temp, exdir = paste0(ghsl_dir,"/fua"))


# * ucdb ------------------------------------------------------------------

#download.file('http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_STAT_UCDB2015MT_GLOBE_R2019A/V1-2/GHS_STAT_UCDB2015MT_GLOBE_R2019A.zip',
#              temp)
#unzip(zipfile = temp, exdir = paste0(ghsl_dir,"/ucdb"))


# * manage directory ------------------------------------------------------

#file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0_web.pdf',
#          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0_web.pdf')
#file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.csv',
#          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.csv')
#file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg',
#          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg')
#file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.xls',
#          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.xls')
#file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2_short.csv',
#          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2_short.csv')
#file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2_short_pnt.gpkg',
#          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2_short_pnt.gpkg')
#file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/readme_V1_2.txt',
#          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/readme_V1_2.txt')

# remover diretorio (fazer manualmente)
#file.remove('//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A', recursive = T)


# built pop smod  ---------------------------------------------------------

# * define function (250 & 1K) ---------------------------------------------

f_download_unzip <- function(base, ano, resolucao){

  if (base == 'SMOD' & resolucao == 250){

    message("Base 'SMOD' suporta apenas resolucao == '1K' ")

  } else if (base == 'BUILT' & ano == 2015) {

    message("Base 'BUILT' suporta apenas ano == 1975,1990,2000,2014 ")

  } else {

    temp = tempfile()

    f_endereco <- function(base, ano, resolucao){

      root = 'http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL'

      #if (resolucao %in% c(250,'1K')) {

      if (base == 'BUILT') {

        ghs_type = paste0('GHS_',base,'_LDSMT_GLOBE_R2018A')

        globe = paste0('GHS_',base,'_LDS',ano,'_GLOBE_R2018A_54009_', resolucao)

        v = 'V2-0'

        ext = paste0(globe,'_','V2_0.zip')

        endereco = paste(root,ghs_type,globe,v,ext,sep = '/')

        return(endereco)

      } else if (base == 'POP') {

        ghs_type = paste0('GHS_',base,'_MT_GLOBE_R2019A')

        globe = paste0('GHS_',base,'_E',ano,'_GLOBE_R2019A_54009_', resolucao)

        v = 'V1-0'

        ext = paste0(globe,'_','V1_0.zip')

        endereco = paste(root,ghs_type,globe,v,ext,sep = '/')

        return(endereco)

      } else if(base == 'SMOD') {

        ghs_type = paste0('GHS_',base,'_POP_GLOBE_R2019A')

        globe = paste0('GHS_',base,'_POP',ano,'_GLOBE_R2019A_54009_', resolucao)

        v = 'V2-0'

        ext = paste0(globe,'_','V2_0.zip')

        endereco = paste(root,ghs_type,globe,v,ext,sep = '/')

        return(endereco)

      } else {

        message("Inserir uma das seguintes bases: BUILT, POP ou SMOD ")

      }

      #}

    }

  }

  endereco = f_endereco(base, ano, resolucao)

  download.file(endereco, temp)

  unzip(zipfile = temp, exdir = paste0(ghsl_dir, "/", base))

}


# * parallel processing ---------------------------------------------------

future::plan(future::multisession)
options(future.globals.maxSize = Inf)

# * built -----------------------------------------------------------------

# res 30m
download.file('http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_LDSMT_GLOBE_R2018A/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30/V2-0/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30_V2_0.zip',
              temp)
unzip(zipfile = temp, exdir = paste0(ghsl_dir,"/BUILT"))

#ATTENTION: BUILT res 250 or 1K: USE YEAR 2014 (not 2015)

# res 250
furrr::future_walk(.x = c(1975,1990,2000,2014), function(x)
  f_download_unzip(base = 'BUILT',ano = x, resolucao = 250)
)

# res 1k
furrr::future_walk(.x = c(1975,1990,2000,2014), function(x)
  f_download_unzip(base = 'BUILT',ano = x, resolucao = '1K')
)

# * pop -------------------------------------------------------------------

# res 250
furrr::future_walk(.x = c(1975,1990,2000,2015), function(x)
  f_download_unzip(base = 'POP',ano = x, resolucao = 250)
)

# res 1k
furrr::future_walk(.x = c(1975,1990,2000,2015), function(x)
  f_download_unzip(base = 'POP',ano = x, resolucao = '1K')
)

# * smod ------------------------------------------------------------------
furrr::future_walk(.x = c(1975,1990,2000,2015), function(x)
  f_download_unzip(base = 'SMOD',ano = x, resolucao = '1K'))

