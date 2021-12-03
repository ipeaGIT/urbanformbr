# description -------------------------------------------------------------

# this script classifies uca according it being an isolated uca (only one muni in
# the uca) or multiple munis in the uca

# setup -------------------------------------------------------------------

source('R/setup.R')


# read data ---------------------------------------------------------------

# read df with code and names of urban concentration, and codes of each muni
#..belonging to them
df_classify_isolated <- readr::read_rds("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/pca_regression_df/pca_regression_df.rds")

# count the number of munis in each urban concentration
df_classify_isolated <- df_classify_isolated %>%
  dplyr::mutate(
    isolated_muni = data.table::fcase(
      purrr::map_int(code_muni_uca, length) == 1L, 1L,
      default = 0L
    )
  )

df_classify_isolated <- df_classify_isolated %>%
  dplyr::select(code_urban_concentration,isolated_muni)

# save data ---------------------------------------------------------------

saveRDS(
  object = df_classify_isolated,
  file = '../../data/urbanformbr/pca_regression_df/classify_uca_isolated.rds',
  compress = 'xz'
)



