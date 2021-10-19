# description -------------------------------------------------------------

# this script estimates factor analysis for all urban form and street network
# variables

# setup -------------------------------------------------------------------

source('R/setup.R')

# read data ---------------------------------------------------------------
df_raw <- readr::read_rds('../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds')

# select variables --------------------------------------------------------

df_dens <- df_raw %>%
  mutate(
    #x_densidade_total = x_pop_2010 / as.numeric(x_urban_extent_size_2014),
    x_street_pop = x_street_length / x_pop_2010
    )

df_select <- df_dens %>%
  dplyr::select(
    dplyr::matches("^(i)")
    #,dplyr::matches("^(y)")
    , x_urban_extent_size_2014
    , dplyr::matches("01km_total")
    #, x_density_built_01km_total_2014
    , x_density_pop_01km_total_2014
    , x_land_use_mix
    , x_proportion_largest_patch
    , x_compacity
    , x_intersection_density_km
    , x_circuity_avg
    , x_entropy
    #, x_betweenness_centrality_avg
    , x_closeness_centrality_avg
    #, x_degree_centrality_avg
    #, x_street_pop
    #, x_coverage
  )


# prep data ---------------------------------------------------------------

# change variable class
df_select[
  ,
  x_urban_extent_size_2014 := as.numeric(x_urban_extent_size_2014)
]


#### change datatable to dataframe for converting one id column to row.names
df_select <- df_select %>%
  select(-c(i_code_urban_concentration, i_name_urban_concentration))

#df_select_df <- df_select %>%
#  tibble::column_to_rownames("i_name_urban_concentration")

df_select_df <- data.frame(df_select)

df_select_df <- data.frame(
  df_select_df[,-1],
  row.names = df_select_df[,1]
)

# factor analysis ---------------------------------------------------------


# * psych::principal ------------------------------------------------------

# ref: Manly, Alberto, 2017: Multivariate Statistics: A Primer

#r_pca <- FactoMineR::PCA(
#  X = df_select_df, scale.unit = T, ncp = ncol(df_select_df), graph = F
#)

# determine number of factors: eigenvalues
#(r_eigenvalue <- factoextra::get_eigenvalue(r_pca))
# 4 eigenvalues greater than unity -> 4 factors considered
#factoextra::fviz_eig(r_pca, addlabels = T, )

# check communalities and factor loadings

#(r_factor_none <- psych::principal(
#  r = df_select_df, nfactors = ncol(df_select_df), rotate = "none"
#)
#)

#(r_factor_varimax <- psych::principal(
#  r = df_select_df, nfactors = ncol(df_select_df), rotate = "varimax"
#)
#)

# * psych::fa -------------------------------------------------------------


# comando final utilizado
(r_factor_varimax <- psych::fa(
  df_select_df, rotate = "varimax",nfactors = ncol(df_select_df), fm = "pa", SMC = F
  )
)


# export factors ----------------------------------------------------------

df_factors <- as.data.frame(r_factor_varimax$scores)

data.table::setDT(df_factors,keep.rownames = T)

setnames(
  df_factors,
  old = c("rn","PA1"),
  new = c("name_uca_case","compact_contig_inter_dens")
  )

df_factors <- df_factors[, c(1:2)]

readr::write_rds(
  df_factors,
  "../../data/urbanformbr/pca_regression_df/factors_morphology.rds",
  compress = "gz"
  )

# correlogram -------------------------------------------------------------

GGally::ggpairs(
  df_select_df,
  columnLabels = c(
    #"Y: Energy per capita", "Y: Commute time",
    "Urban size", "Density pop 01km", "Density built 01km", "Land use mix",
    "Prop largest patch", "Compacity", "Intersection dens", "Circuity avg",
    "Entropy","Betweeness centr", "Closeness centr", "Degree centr","Street length"
  )
  )

GGally::ggcorr(df_select_df)


