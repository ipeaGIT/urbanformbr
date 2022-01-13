# description -------------------------------------------------------------

# this script merge all the metrics dfs, adds sufix according to variable type and
# save a full database, to be filtered at factor/cluster and regression analysis

#6666666666666 conferir se todas as bases fizeram o filtro.
# caso contrario, adicionar linha analoga abaixo
 # filter only 184 from our df
#  df_energy <- subset(df_energy, code_urban_concentration %in% df_reference$code_urban_concentration)

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# read and clean data ---------------------------------------------------------------

# * prep/reference data -------------------------------------------------------------
df_reference <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds") %>%
  sf::st_drop_geometry() %>%
  select(-pop_ibge_total_2010)

# * pop growth ------------------------------------------------------------

df_pop_growth <- data.table::fread("../../data/urbanformbr/consolidated_data/urban_growth_population.csv")
df_pop_growth[, name_uca_case := NULL]

# * fleet ---------------------------------------------------------------

df_fleet <- data.table::fread("../../data/urbanformbr/consolidated_data/denatran_fleet_metrics.csv")

# * energy ------------------------------------------------------------------

df_energy <- readr::read_rds("../../data/urbanformbr/consolidated_data/anp_energy-2010_metrics.rds")

# filter 182 ucas
df_energy <- subset(df_energy, code_urban_concentration %in% df_reference$code_urban_concentration)

# df_energy[, tep := as.numeric(tep)]
# rename
setnames(x = df_energy
         , old = 'tep'
         , new = 'energy_per_capita')

# * emissions ------------------------------------------------------------------

# df_emissions <- readr::read_rds("../../data/urbanformbr/pca_regression_df/co2_per_capita2010.rds")
#
# # filter only 184 from our df
# df_emissions <- subset(df_emissions, code_urban_concentration %in% df_reference$code_urban_concentration)


# * censo -----------------------------------------------------------------
df_censo <- data.table::fread("../../data/urbanformbr/consolidated_data/censo_metrics.csv")
df_censo[, name_uca_case := NULL]

# * experienced density ---------------------------------------------------
df_exp_density <- data.table::fread("../../data/urbanformbr/consolidated_data/ghsl_experienced_density_metrics.csv")
df_exp_density[, name_uca_case := NULL]

# * landuse metrics -------------------------------------------------------
df_landuse <- data.table::fread("../../data/urbanformbr/consolidated_data/landuse_mix.csv")
df_landuse[, name_uca_case := NULL]

# * street metrics --------------------------------------------------------
df_street <- data.table::fread("../../data/urbanformbr/consolidated_data/streets_metrics_new_23_12_2021_v3.csv")
df_street <- subset(df_street, name_urban_concentration %in% df_reference$name_urban_concentration)

# df_street <- df_street %>%
#   dplyr::rename(street_orientation_irregularity = entropy)

# * fragmentation compacity -----------------------------------------------
df_frag_comp <- data.table::fread("../../data/urbanformbr/consolidated_data/fragmentation_compacity.csv")

# * topography ------------------------------------------------------------
df_topo <- data.table::fread("../../data/urbanformbr/consolidated_data/topography_metrics.csv")
df_topo[, name_uca_case := NULL]

# * tma (public transport) --------------------------------------------------
df_tma <- data.table::fread("../../data/urbanformbr/consolidated_data/classify_tma_public_transport.csv")

# * factors ---------------------------------------------------------------
df_factors <- data.table::fread("../../data/urbanformbr/consolidated_data/factor_analysis_metrics.csv")


# merge data --------------------------------------------------------------
df_merge <- dplyr::left_join(df_reference, df_factors) %>%
  dplyr::left_join(df_tma) %>%
  dplyr::left_join(df_pop_growth) %>%
  dplyr::left_join(df_fleet) %>%
  dplyr::left_join(df_energy) %>%
  dplyr::left_join(df_censo) %>%
  dplyr::left_join(df_exp_density) %>%
  dplyr::left_join(df_landuse) %>%
  dplyr::left_join(df_frag_comp) %>%
  dplyr::left_join(df_topo)



df_merge <- dplyr::left_join(
  df_merge,
  df_street,
  by = c('name_urban_concentration' = 'name_urban_concentration')
)

# * reorder columns -------------------------------------------------------
df_merge <- df_merge %>%
  dplyr::relocate(
    c(energy_per_capita, wghtd_mean_commute_time),
    .after = name_uca_case
  )

# * CREATE street pop ------------------------------------------------------------
df_merge <- df_merge %>%
  dplyr::mutate(street_pop = street_length / pop_2010)

# * add prefix (dependent & independent variable) -------------------------

df_merge <- df_merge %>%
  # id
  dplyr::rename_with(
    .cols = code_urban_concentration:name_uca_case,
    function(x){paste0("i_", x)}
  ) %>%
  # dependent variables (y)
  dplyr::rename_with(
    .cols = energy_per_capita:wghtd_mean_commute_time,
    function(x){paste0("y_", x)}
  ) %>%
  # factors
  dplyr::rename_with(
    .cols = compact_contig_inter_dens,
    function(x){paste0("f_", x)}
  ) %>%
  # dummy variables
  dplyr::rename_with(
    .cols = tma,
    function(x){paste0("d_", x)}
  ) %>%
  # independent variables (x)
  dplyr::rename_with(
    .cols = upward_pop_growth_1990_2014:length(.),
    function(x){paste0("x_", x)}
  )


# check data --------------------------------------------------------------

data.table::setDT(df_merge)

glimpse(df_merge)

# check if there is any missing values
any(is.na(df_merge))
# complete.cases to avoid missing values
#df_merge <- df_merge[complete.cases(df_merge)]

# save data ---------------------------------------------------------------

data.table::fwrite(
  x = df_merge
  , file = "../../data/urbanformbr/consolidated_data/urbanformbr_metrics_full.csv"
  , sep = ";"
  , append = F
)


##############################################################################

# old variables -----------------------------------------------------------
# * pop censo -----------------------------------------------------------

#df_pop_censo <- readr::read_rds("../../data/urbanformbr/pca_regression_df/1970-2015_pop.rds")

# filter only 184 from our df
#df_pop_censo <- subset(df_pop_censo, code_urban_concentration %in% df_reference$code_urban_concentration)

#df_pop_censo <- df_pop_censo %>%
#  tidyr::pivot_wider(
#    names_from = c("ano"),
#    values_from = c("pop"),
#    names_prefix = "pop_"
#  )

#df_pop_censo <- df_pop_censo %>%
#  dplyr::select(-pop_1970)


# * pop ghsl ------------------------------------------------------------
#df_pop_ghsl <- readr::read_rds('../../data/urbanformbr/ghsl/results/uca_pop_100000_built_up_area_population_results.rds')

#df_pop_ghsl <- df_pop_ghsl %>%
#  sf::st_drop_geometry() %>%
#  dplyr::select(code_urban_concentration, pop1975, pop2015) %>%
#  rename(
#    pop_ghsl_1975 = pop1975,
#    pop_ghsl_2015 = pop2015
#    ) %>%
#  dplyr::filter(code_urban_concentration %in% df_reference$code_urban_concentration)


#data.table::setDT(df_pop_ghsl)[
#  ,
#  pop_geom_growth_1975_2015 := ( (pop_ghsl_2015 / pop_ghsl_1975) ^ (1/40) ) - 1,
#  by = .(code_urban_concentration)
#]

#df_pop_ghsl <- df_pop_ghsl %>%
#  dplyr::select(-c(pop_ghsl_2015,pop_ghsl_1975))

# * merge pop data ------------------------------------------------------
#df_pop <- dplyr::left_join(
#  df_pop_censo, df_pop_growth,
#  by = c("code_urban_concentration" = "code_urban_concentration")
#) #%>%
#dplyr::select(-pop_geom_growth_1975_2015 )

# remove pop_geom_growth_1975_2015 derived from ghsl since we already have
# a pop growth variable estimated at experienced density dataset

# * pib -------------------------------------------------------------------
#df_pib <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pib.rds") %>%
#  dplyr::select(-c(valor,pop))

# filter only 184 from our df
#df_pib <- subset(df_pib, code_urban_concentration %in% df_reference$code_urban_concentration)

#df_pib <- df_pib %>%
#  tidyr::pivot_wider(
#    names_from = c("ano"),
#    values_from = c("pib_capita"),
#    names_prefix = "pib_capita_"
#  )

#df_pib <- df_pib %>%
#  dplyr::select(code_urban_concentration, pib_capita_2010)


# * urban form cluster -------------------------------------------------------------------
#df_cluster <- fread('../../data/urbanformbr/pca_regression_df/cluster_output/cluster_1.csv')
#head(df_cluster)

# select and rename columns
#df_cluster <- dplyr::select(df_cluster, c('i_code_urban_concentration', 'cluster4'))
#setnames(df_cluster, 'i_code_urban_concentration', 'code_urban_concentration')

# * * isolated uca --------------------------------------------------------
#df_classify_isolated <- readr::read_rds('../../data/urbanformbr/pca_regression_df/classify_uca_isolated.rds')


