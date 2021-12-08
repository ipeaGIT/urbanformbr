# description -------------------------------------------------------------

# this script prepares the dataframe that will be used in PCA
#..using multiple data (GHSL,census,OSM,etc).
# the dataframe is based on identifying urban concentration areas (uca) defined by IBGE
# three variables identifies each urban concentration areas:
# code_urban_concentration: code of the main municipality within each uca
# name_urban_concentration: correct name of the main municipality within each uca
# name_uca_case: name of main muni without special characters or space

# the code for each municipality within each uca is also added (code_munis_uca)

# OBS:
# run script `urban_shapes` before running this script for creating urban shapes df

# this script excludes santa_cruz_do_sul_rs (4316808) excluded at GHSL/04_1 for not
#..having 20%+ built area in 1975 (i.e. not meeting urban extent cutoff criteria)

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# define function ---------------------------------------------------------
f_prepare_df <- function(){

  # * read urban shapes -----------------------------------------------------
  urban_shapes <- readr::read_rds('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds')

  # select only identification variables and setDT
  urban_shapes <-  data.table::setDT(urban_shapes)[
    ,
    .(code_urban_concentration, name_urban_concentration, name_uca_case)
  ]

  # remove areas in the international border --------------------------------

  # Internacional de Uruguaiana/Brasil (RS) 4322400
  # Internacional de Foz do Iguaçu/Brasil - Ciudad del Este/Paraguai (PR) 4108304
  # Internacional de Corumba/Brasil (MS) 5003207

  # and Santa Cruz do Sul/RS because of min. urban area in 1975
  # Santa Cruz do Sul/RS 4316808
  to_be_removed <- c(4322400, 4108304, 5003207, 4316808)
  urban_shapes <- subset(urban_shapes, code_urban_concentration %nin% to_be_removed)


  # * read urban concentration ibge -----------------------------------------
  uca <- geobr::read_urban_concentrations(simplified = F)
  uca <- data.table::setDT(uca)[code_urban_concentration %in% urban_shapes$code_urban_concentration]

  uca <- uca %>%
    dplyr::group_by(code_urban_concentration) %>%
    dplyr::summarise(code_muni_uca = list(code_muni))

  #uca <- uca[
  #  ,
  #  lapply(.SD, list),
  #  by = .(code_urban_concentration),
  #  .SDcols = c("code_muni")
  #]


  # * join data from uca to urban shapes ------------------------------------
  urban_shapes[
    data.table::setDT(uca),
    `:=`(
      code_muni_uca = i.code_muni_uca
    ),
    on = c('code_urban_concentration' = 'code_urban_concentration')
  ]



  # * save data -------------------------------------------------------------

  saveRDS(
    object = urban_shapes,
    file = '//storage6/usuarios/Proj_acess_oport/data/urbanformbr/pca_regression_df/pca_regression_df.rds',
    compress = 'xz'
    )

}

# run function ------------------------------------------------------------
f_prepare_df()

