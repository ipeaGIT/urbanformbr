
# description -------------------------------------------------------------

# this script downloads, unzips and saves data from the GHSL multitemporal
# dataset-tile used to create a map at urbanformbr report
# uca from Belo Horizonte is used

# tile containing belo horizonte: ID 10_13
# link
# http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_LDSMT_GLOBE_R2018A/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30/V2-0/tiles/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30_V2_0_9_13.zip

# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")

# create tempfile ---------------------------------------------------------

temp <- tempfile()


# function ----------------------------------------------------------------

f_download_unzip <- function(tile_id){

  temp = tempfile()

  endereco = paste0("http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_LDSMT_GLOBE_R2018A/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30/V2-0/tiles/GHS_BUILT_LDSMT_GLOBE_R2018A_3857_30_V2_0_",tile_id,".zip")

  download.file(endereco, temp)

  if (!dir.exists("../../data-raw/ghsl/BUILT/multitemporal_30m")){
    dir.create("../../data-raw/ghsl/BUILT/multitemporal_30m")
  }

  unzip(zipfile = temp, exdir ="../../data-raw/ghsl/BUILT/multitemporal_30m")

}


# run function ------------------------------------------------------------
f_download_unzip()


