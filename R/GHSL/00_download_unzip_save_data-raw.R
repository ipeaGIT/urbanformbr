# description -------------------------------------------------------------

# this script downloads, unzips and saves data from two GHSL datasets:
# Functional Urban Areas (FUA) and Urban Centre Database (UCDB)

# steps:
# i. download data
# ii. unzip and save data at //storage6/usuarios/Proj_acess_oport/data-raw

# setup -------------------------------------------------------------------

# load setup file once is crated
#source('./R/00_setup.R')

#TEMPORARY LIBRARY
parallel::detectCores()
options(Ncpus = 4)

library(data.table)
library(sf)
library(tidyverse)
library(janitor)
library(here)


# directory ---------------------------------------------------------------

if (!dir.exists("//storage6/usuarios/Proj_acess_oport/data-raw/ghsl")){
  dir.create("//storage6/usuarios/Proj_acess_oport/data-raw/ghsl")
}

ghsl_dir <- "//storage6/usuarios/Proj_acess_oport/data-raw/ghsl"


# download, unzip and save data-raw ---------------------------------------

# create temp file
temp <- tempfile()

# * fua -------------------------------------------------------------------

download.file('http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_FUA_UCDB2015_GLOBE_R2019A/V1-0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.zip',
              temp)
unzip(zipfile = temp, exdir = paste0(ghsl_dir,"/fua"))


# * ucdb ------------------------------------------------------------------

download.file('http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_STAT_UCDB2015MT_GLOBE_R2019A/V1-2/GHS_STAT_UCDB2015MT_GLOBE_R2019A.zip',
              temp)
unzip(zipfile = temp, exdir = paste0(ghsl_dir,"/ucdb"))


file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0_web.pdf',
          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_0_web.pdf')
file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.csv',
          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.csv')
file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg',
          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg')
file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.xls',
          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.xls')
file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2_short.csv',
          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2_short.csv')
file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2_short_pnt.gpkg',
          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2_short_pnt.gpkg')
file.copy(from = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A/readme_V1_2.txt',
          to = '//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/readme_V1_2.txt')

# remover diretorio (fazer manualmente)
#file.remove('//storage6/usuarios/Proj_acess_oport/data-raw/ghsl/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A', recursive = T)
