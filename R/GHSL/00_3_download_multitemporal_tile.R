
# description -------------------------------------------------------------

# this script downloads, unzips and saves data from the GHSL multitemporal
# dataset-tile used to create a map at urbanformbr report
# uca from Belo Horizonte is used

# setup -------------------------------------------------------------------


# create tempfile ---------------------------------------------------------

temp <- tempfile()


# function ----------------------------------------------------------------
666666666666 ATUALIZAR FUNCAO
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

# run function ------------------------------------------------------------

furrr::future_walk(.x = c(1975,1990,2000,2014), function(x)
  f_download_unzip(base = 'BUILT',ano = x, resolucao = 250)
)


