Sys.setenv(TZ='UTC') # Fuso horario local

# carregar bibliotecas
list.of.packages <- c('ggplot2'      # visualizacao de dados
                      ,'ggthemes'     # temas para visualizacao de dados
                      ,'sf'           # leitura e manipulacao de dados espaciais
                      ,'data.table'   # manipulacao de dados
                      ,'geobr'        # dados espaciais do brasil
                      ,'pbapply'      # progress bar
                      ,'readr'        # rapida leitura de dados
                      ,'tidyr'        # manipulacao de dados
                      ,'stringr'      # operacoes em strings
                      ,'lubridate'    # dados em data/horario
                      ,'mapview'      # visualizacao interativa dos dados
                      ,'RColorBrewer' # paleta de cores
                      ,'extrafont'    # fontes de texto
                      #loadfonts(

                      ,'gganimate'  # install.packages("gganimate"
                      ,'ggrepel'
                      ,'ggforce'

                      ,'stars'
                      ,'raster'
                      ,'rgdal'
                      ,'exactextractr'

                      ,'future'
                      ,'furrr'
                      ,'purrr'
                      ,'dplyr'
                      ,'hrbrthemes'
                      ,'beepr'
                      ,'datapasta'
                      ,'patchwork'
                      ,'sidrar'
                      ,'devtools'
                      ,'janitor'
                      ,'lemon'
                      ,'rio'
                      ,'scales'
                      ,'rlist'
                      ,'survey'
                      ,'srvyr'
                      # devtools::install_github(""
                      #microdadosBrasil
                      ,'Hmisc'
                      ,'forcats'
                      ,'gridExtra'
                      ,'ggplotify'
                      ,'grid'
                      ,'PNADcIBGE'
                      ,'ggtext'
                      ,'cowplot'
                      ,'gganimate'  # install.packages("gganimate"
                      ,'ggrepel'
                      ,'ggnewscale'
                      ,'magrittr'
                      ,'maptools'
                      ,'rgeos'
                      ,'XLConnect'
                      ,'geodist')


# install packages----
new_packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new_packages)>1){install.packages(new_packages) }

# load libraries ----
lapply(list.of.packages, require, character.only = TRUE)

# ceramic
devtools::install_github("lucasmation/microdadosBrasil",force = FALSE)
library(microdadosBrasil)


# options
mapviewOptions(platform = 'mapdeck')

# disable scientific notation
options(scipen=10000)


# Use GForce Optimisations in data.table operations
# details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
options(datatable.optimize=Inf)

# set number of threads used in data.table
data.table::setDTthreads(percent = 100)



## usefull support functions

`%nin%` = Negate(`%in%`)
`%nlike%` = Negate(`%like%`)


# remove vectors
rm(list.of.packages)
rm(new_packages)

# Clean environment and memory

gc(reset = TRUE)
