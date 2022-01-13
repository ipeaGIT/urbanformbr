# description -------------------------------------------------------------

# this script estimates factor analysis for all urban form and street network
# variables

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# read data ---------------------------------------------------------------


# * urban shapes ----------------------------------------------------------
df_shapes <- read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")
df_shapes <- df_shapes %>%
  sf::st_drop_geometry() %>%
  select(-pop_ibge_total_2010)

# * ghsl density ----------------------------------------------------------
df_density <- data.table::fread("../../data/urbanformbr/consolidated_data/ghsl_experienced_density_metrics.csv")

df_density <- df_density %>%
  dplyr::select(code_urban_concentration, density_pop_02km_2014)

# * land use mix ----------------------------------------------------------
df_land_mix <- data.table::fread("../../data/urbanformbr/consolidated_data/landuse_mix.csv")
df_land_mix[, name_uca_case:=NULL]

# * fragmentation compacity -----------------------------------------------
df_fragmentation <- data.table::fread("../../data/urbanformbr/consolidated_data/fragmentation_compacity.csv")

df_fragmentation <- df_fragmentation %>%
  dplyr::select(code_urban_concentration,ends_with("2014"))

# * street metrics --------------------------------------------------------
df_street <- data.table::fread(file = "../../data/urbanformbr/consolidated_data/streets_metrics_new_23_12_2021_v3.csv")

df_street <- df_street %>%
  select(name_urban_concentration, intersection_density_km, circuity_avg,
         normalized_closeness_centrality_avg
         #, street_length
         )

df_street <- subset(df_street, name_urban_concentration %in% df_shapes$name_urban_concentration)


# * censo -----------------------------------------------------------------
#
# df_censo <- data.table::fread("../../data/urbanformbr/consolidated_data/censo_metrics.csv")
#
# df_censo <- df_censo %>%
#   select(code_urban_concentration, pop_2010)

# merge data --------------------------------------------------------------
df_factor <- dplyr::left_join(df_shapes, df_density) %>%
  dplyr::left_join(df_land_mix) %>%
  dplyr::left_join(df_fragmentation) %>%
  dplyr::left_join(df_street) #%>%
  # dplyr::left_join(df_censo)

# create variable --------------------------------------------------------

# setDT(df_factor)[, street_pop := street_length / pop_2010]
#
# df_factor <- df_factor %>%
#   select(-c(street_length, pop_2010))


# prep data ---------------------------------------------------------------

#### change datatable to dataframe for converting one id column to row.names
df_input <- df_factor %>%
  select(-c(code_urban_concentration, name_urban_concentration))

#df_input_df <- df_input %>%
#  tibble::column_to_rownames("i_name_urban_concentration")

df_input <- data.frame(df_input)

df_input <- data.frame(
  df_input[,-1],
  row.names = df_input[,1]
)

# factor analysis ---------------------------------------------------------


# * psych::principal ------------------------------------------------------

# ref: Manly, Alberto, 2017: Multivariate Statistics: A Primer

#r_pca <- FactoMineR::PCA(
#  X = df_input_df, scale.unit = T, ncp = ncol(df_input_df), graph = F
#)

# determine number of factors: eigenvalues
#(r_eigenvalue <- factoextra::get_eigenvalue(r_pca))
# 4 eigenvalues greater than unity -> 4 factors considered
#factoextra::fviz_eig(r_pca, addlabels = T, )

# check communalities and factor loadings

#(r_factor_none <- psych::principal(
#  r = df_input_df, nfactors = ncol(df_input_df), rotate = "none"
#)
#)

#(r_factor_varimax <- psych::principal(
#  r = df_input_df, nfactors = ncol(df_input_df), rotate = "varimax"
#)
#)

# * psych::fa -------------------------------------------------------------


# comando final utilizado
(r_factor_varimax <- psych::fa(
  df_input, rotate = "varimax", nfactors = ncol(df_input), fm = "pa", SMC = F
  )
)


# export factors ----------------------------------------------------------

df_factor_results <- as.data.frame(r_factor_varimax$scores)

data.table::setDT(df_factor_results,keep.rownames = T)

setnames(
  df_factor_results,
  old = c("rn","PA1"),
  new = c("name_uca_case","compact_contig_inter_dens")
  )

df_factor_results <- df_factor_results[, c(1:2)]

df_factor_results <- dplyr::left_join(
  df_factor_results, df_shapes %>% select(code_urban_concentration,name_uca_case)
)

df_factor_results <- df_factor_results %>%
  select(-name_uca_case) %>%
  relocate(code_urban_concentration, .before = compact_contig_inter_dens)


# save results ------------------------------------------------------------

data.table::fwrite(
  x = df_factor_results
  , file = "../../data/urbanformbr/consolidated_data/factor_analysis_metrics.csv"
  , append = F
)



# correlogram -------------------------------------------------------------
#
# GGally::ggpairs(
#   df_input_df,
#   columnLabels = c(
#     #"Y: Energy per capita", "Y: Commute time",
#     "Urban size", "Density pop 01km", "Density built 01km", "Land use mix",
#     "Prop largest patch", "Compacity", "Intersection dens", "Circuity avg",
#     "Entropy","Betweeness centr", "Closeness centr", "Degree centr","Street length"
#   )
#   )
#
# GGally::ggcorr(df_input_df)


