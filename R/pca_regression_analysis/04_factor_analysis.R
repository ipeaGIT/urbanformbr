# description -------------------------------------------------------------

# this script estimates multiple factor analysis for all variables of urbanform
# FAZER SO COM URBANFORM?

# setup -------------------------------------------------------------------

source('R/setup.R')

# read data ---------------------------------------------------------------
df_raw <- readr::read_rds('../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds')

# select variables --------------------------------------------------------
df_select <- df_raw %>%
  dplyr::select(
    dplyr::matches("^(i)"),
    #d_tma, # conferir especificidade variavel binaria
    #x_urban_extent_size_2014, #built total total -> substituir?
    x_built_total_total_2014,
    #x_prop_built_consolidated_area_2014, # proxy para compacidade
    x_density_pop_05km_total_2014,
    x_land_use_mix,
    x_proportion_largest_patch,
    #x_n_large_patches,
    x_compacity,
    #x_k_avg,
    x_intersection_density_km,
    x_circuity_avg#,
    #x_sd_elevation, # excluir vars forma fisica
    #x_mean_slope
  )

df_area <- readr::read_rds("../../data/urbanformbr/pca_regression_df/area.rds") %>%
  dplyr::select(code_muni, saturation_total_area_fixed_2014) %>%
  dplyr::rename(
    i_code_urban_concentration = code_muni,
    x_coverage = saturation_total_area_fixed_2014
    )

df_select <- data.table::merge.data.table(
  df_select, df_area, by = "i_code_urban_concentration")


# prep data ---------------------------------------------------------------

# change variable class
df_select[
  ,
  x_urban_extent_size_2014 := as.numeric(x_urban_extent_size_2014)
]

# after properly classifying each group, reorder columns so that each group is ordered
#..in the dataset
# reorder
df_select <- df_select %>%
  relocate(x_coverage, .after = x_compacity)# %>%
  #relocate(d_tma, .after = x_mean_slope)

#### change datatable to dataframe for converting one id column to row.names
df_select <- df_select %>%
  select(-c(i_code_urban_concentration, i_name_uca_case))

#df_select_df <- df_select %>%
#  tibble::column_to_rownames("i_name_urban_concentration")

df_select_df <- data.frame(df_select)

df_select_df <- data.frame(
  df_select_df[,-1],
  row.names = df_select_df[,1]
)

# factor analysis ---------------------------------------------------------

# * PCA -------------------------------------------------------------------
r_pca <- FactoMineR::PCA(
  X = df_select_df, scale.unit = T, ncp = 8, graph = F
)

# determine number of factors: eigenvalues
r_eigenvalue <- factoextra::get_eigenvalue(r_pca)
# 3 eigenvalues greater than unity -> 3 factors considered
factoextra::fviz_eig(r_pca, addlabels = T, )

# check communalities and factor loadings ---------------------------------
r_factor <- psych::principal(
  r = df_select_df, nfactors = 3, rotate = "none"
)

r_factor_varimax <- psych::principal(
  r = df_select_df, nfactors = 3, rotate = "varimax"
)

r_factor_none_8 <- psych::principal(
  r = df_select_df, nfactors = 8, rotate = "none"
)

r_factor_varimax_8 <- psych::principal(
  r = df_select_df, nfactors = 8, rotate = "varimax"
)

# * factor rotation (if necessary) ----------------------------------------


# factor labelling --------------------------------------------------------






# MULTIPLE factor analysis ---------------------------------------------------------


# organize the variables into groups -> ALINHAR GRUPOS

## g0: id -> 3 VARS
# i_code_urban_concentration
# i_namee_urban_concentration
# i_name_uca_case

## g1: urban form metrics -> 6 VARS
# x_urban_extent_size_2014
# x_prop_built_consolidated_area_2014
# x_density_pop_05km_total_2014
# x_land_use_mix
# x_compacity
# x_coverage

## g2: urban network infrastructure -> 3 VARS
# x_k_avg
# x_intersection_density_km
# x_circuity_avg

## g3: physical characteristics -> 2 VARS
# x_sd_elevation
# x_mean_slope

## g4: categorial variables -> 1 VAR
# d_tma

# make d_tma factor for classifying as a categorical variable at MFA



# define R objects for factor analysis package
df_select_df <- df_select_df %>%
  dplyr::mutate(
    d_tma = factor(
      d_tma, levels = c(0,1), labels = c("Não TMA", "TMA")
    )
  )

# groups
grupo <- c(6,3,2,1)
nome_grupo <- c("form","network","physical","cat")
tipo <- c("s","s","s","n")


# run factor analysis -----------------------------------------------------

res.mfa <- FactoMineR::MFA(
  base = df_select_df,
  group = grupo,
  name.group = nome_grupo,
  type = tipo,
  graph = F
)


# results --------------------------------------------------------------------
eig.val <- factoextra::get_eigenvalue(res.mfa)
head(eig.val)

group <- factoextra::get_mfa_var(res.mfa, "group")
head(group$coord)
head(group$contrib)


# * plots -----------------------------------------------------------------

factoextra::fviz_screeplot(res.mfa)

factoextra::fviz_mfa(res.mfa, "group")

factoextra::fviz_contrib(res.mfa, "group", axes = 1) +
factoextra::fviz_contrib(res.mfa, "group", axes = 2)

factoextra::fviz_mfa_var(
  X = res.mfa, choice = "quanti.var", palette = "jco",
  repel = T, legend = "bottom", geom = c("arrow","text")
)


factoextra::fviz_contrib(
  res.mfa, choice = "quanti.var", axes = 1, top = 10, palette = "jco"
    ) +
  factoextra::fviz_contrib(
    res.mfa, choice = "quanti.var", axes = 2, top = 10, palette = "jco"
  )


factoextra::fviz_mfa_var(
  X = res.mfa, choice = "quanti.var", col.var = "contrib",
  gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
  repel = T, legend = "bottom", geom = c("point","text")
)

factoextra::fviz_mfa_var(
  X = res.mfa, choice = "quanti.var", col.var = "cos2",
  gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
  repel = T, legend = "bottom", geom = c("point","text")
)
fviz_cos2(res.mfa,choice = "quanti.var",axes = 1)



# individuals
fviz_mfa_ind(res.mfa, partial = "all")


fviz_mfa_axes(res.mfa)
