# description -------------------------------------------------------------

# this script merge all the dfs containing variables to be used at exploratory
#..and regression analysis, created at R/pca_regression_analysis/00_x_...

# setup -------------------------------------------------------------------
rm(list=ls())
source('R/setup.R')

# read and clean data ---------------------------------------------------------------


  # * prep data -------------------------------------------------------------
  df_prep <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df.rds") %>%
    dplyr::select(-c(code_muni_uca))



  # * pop -------------------------------------------------------------------

  # * * pop growth -----------------------------------------------------------

  df_pop_growth <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pop_growth.rds")
  df_pop_growth[,c("pop2000","pop2015") := NULL]
  df_pop_growth[,code_urban_concentration := as.integer(code_urban_concentration)]
  data.table::setnames(df_pop_growth,"r15_00","pop_growth_15_00")


  # * * pop censo -----------------------------------------------------------

  df_pop_censo <- readr::read_rds("../../data/urbanformbr/pca_regression_df/1970-2015_pop.rds")

  # filter only 184 from our df
  df_pop_censo <- subset(df_pop_censo, code_urban_concentration %in% df_prep$code_urban_concentration)

  df_pop_censo <- df_pop_censo %>%
    tidyr::pivot_wider(
      names_from = c("ano"),
      values_from = c("pop"),
      names_prefix = "pop_"
    )

  df_pop_censo <- df_pop_censo %>%
    dplyr::select(-pop_1970)


# * * pop ghsl ------------------------------------------------------------
  #df_pop_ghsl <- readr::read_rds('../../data/urbanformbr/ghsl/results/uca_pop_100000_built_up_area_population_results.rds')

  #df_pop_ghsl <- df_pop_ghsl %>%
  #  sf::st_drop_geometry() %>%
  #  dplyr::select(code_urban_concentration, pop1975, pop2015) %>%
  #  rename(
  #    pop_ghsl_1975 = pop1975,
  #    pop_ghsl_2015 = pop2015
  #    ) %>%
  #  dplyr::filter(code_urban_concentration %in% df_prep$code_urban_concentration)


  #data.table::setDT(df_pop_ghsl)[
  #  ,
  #  pop_geom_growth_1975_2015 := ( (pop_ghsl_2015 / pop_ghsl_1975) ^ (1/40) ) - 1,
  #  by = .(code_urban_concentration)
  #]

  #df_pop_ghsl <- df_pop_ghsl %>%
  #  dplyr::select(-c(pop_ghsl_2015,pop_ghsl_1975))

  # * * merge pop data ------------------------------------------------------
  #df_pop <- dplyr::left_join(
  #  df_pop_censo, df_pop_growth,
  #  by = c("code_urban_concentration" = "code_urban_concentration")
  #) #%>%
    #dplyr::select(-pop_geom_growth_1975_2015 )

  # remove pop_geom_growth_1975_2015 derived from ghsl since we already have
  # a pop growth variable estimated at experienced density dataset


  # * fuel ------------------------------------------------------------------

  df_fuel <- readr::read_rds("../../data/urbanformbr/pca_regression_df/fuel.rds") %>%
    dplyr::select(code_urban_concentration,year,fuel_consumption_total)

  # filter only 184 from our df
  df_fuel <- subset(df_fuel, code_urban_concentration %in% df_prep$code_urban_concentration)

  df_fuel <- df_fuel %>%
    dplyr::arrange(year)

  df_fuel <- df_fuel %>%
    tidyr::pivot_wider(
    names_from = c("year"),
    values_from = c("fuel_consumption_total"),
    names_prefix = "fuel_consumption_total_"
  )

  df_fuel <- df_fuel %>%
    dplyr::select(code_urban_concentration, fuel_consumption_total_2010)




  # * energy ------------------------------------------------------------------

  df_energy <- readr::read_rds("../../data/urbanformbr/pca_regression_df/tep_energy_2010.rds") %>%
    dplyr::select(code_urban_concentration, tep )

  # filter only 184 from our df
  df_energy <- subset(df_energy, code_urban_concentration %in% df_prep$code_urban_concentration)

  df_energy[, tep := as.numeric(tep)]
  # df_energy[ tep ==0]

  # rename
  setnames(df_energy, 'tep', 'fuel_energy_per_capita')



  # * pib -------------------------------------------------------------------
  #df_pib <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pib.rds") %>%
  #  dplyr::select(-c(valor,pop))

  # filter only 184 from our df
  #df_pib <- subset(df_pib, code_urban_concentration %in% df_prep$code_urban_concentration)

  #df_pib <- df_pib %>%
  #  tidyr::pivot_wider(
  #    names_from = c("ano"),
  #    values_from = c("pib_capita"),
  #    names_prefix = "pib_capita_"
  #  )

  #df_pib <- df_pib %>%
  #  dplyr::select(code_urban_concentration, pib_capita_2010)


  # * area coverage ---------------------------------------------------------
  df_area <- readr::read_rds("../../data/urbanformbr/pca_regression_df/area.rds") %>%
    dplyr::select(code_muni,urban_extent_size_2014) %>%
    dplyr::rename(code_urban_concentration = code_muni)

  # * censo -----------------------------------------------------------------
  df_censo <- readr::read_rds("../../data/urbanformbr/pca_regression_df/censo.rds") %>%
    dplyr::select(-name_uca_case)

  # * experienced density ---------------------------------------------------
  df_exp_density <- readr::read_rds("../../data/urbanformbr/pca_regression_df/exp_density_ghsl.rds") %>%
    dplyr::select(
      -c(name_uca_case),
      -ends_with("1975"),
      -c(pop_total_total_1975:pop_total_expansao_2014),
      -dplyr::matches("(abs_diff|expansao|consolidada)"),
      -dplyr::matches("growth")
      ) %>%
    dplyr::rename(
      code_urban_concentration = code_muni,
      density_pop_05km_total_2014 = density_pop_05km2_total_2014,
      density_pop_10km_total_2014 = density_pop_10km2_total_2014,
      density_built_05km_total_2014 = density_built_05km2_total_2014,
      density_built_10km_total_2014 = density_built_10km2_total_2014
      )

  # * landuse metrics -------------------------------------------------------
  df_landuse <- data.table::fread("../../data/urbanformbr/pca_regression_df/landuse_mix_metrics.csv") %>%
    dplyr::select(-c(name_uca_case,theil_h,entropy)) %>%
    dplyr::rename(code_urban_concentration = code_muni)

  df_landuse[
    ,
    `:=`(
      land_use_mix = 1 - dissimilarity,
      land_use_mix_5km = 1 - dissimilarity_5km,
      land_use_mix_10km = 1 - dissimilarity_10km,
      land_use_mix_15km = 1 - dissimilarity_15km
    )
  ]

  df_landuse <- df_landuse %>%
    select(-starts_with("dissimilarity"))


  # * street metrics --------------------------------------------------------
  df_street <- data.table::fread("../../data/urbanformbr/pca_regression_df/streets_metrics.csv") %>%
    dplyr::select(-intersection_count)

  df_street <- subset(df_street, name_urban_concentration %in% df_prep$name_urban_concentration)

  # * fragmentation compacity -----------------------------------------------
  df_frag_comp <- data.table::fread("../../data/urbanformbr/pca_regression_df/fragmentation_compacity.csv") %>%
    select(-c(name_uca_case,n_patches,avg_cell_distance_w_pop,
              ratio_circle,ratio_circle_large)) %>%
    dplyr::rename(code_urban_concentration = code_muni)

  # * topography ------------------------------------------------------------
  df_topo <- readr::read_rds("../../data/urbanformbr/pca_regression_df/topography.rds") %>%
    dplyr::select(-c(name_uca_case,mean_ruggedness)) %>%
    dplyr::rename(code_urban_concentration = code_muni)

  # * classify uca ----------------------------------------------------------

  # * * tma (transporte media alta capacidade) ------------------------------
  df_classify_tma <- readr::read_rds("../../data/urbanformbr/pca_regression_df/classify_uca_tma.rds")

  # * * large uca -----------------------------------------------------------

  df_classify_uca_large <- readr::read_rds('../../data/urbanformbr/pca_regression_df/classify_uca_large_pop.rds') %>%
  dplyr::select(-pop_uca_2010)

  # filter ucas
  df_classify_uca_large <- subset(
    df_classify_uca_large,
    code_urban_concentration %in% df_prep$code_urban_concentration
    )

  # * * isolated uca --------------------------------------------------------
  #df_classify_isolated <- readr::read_rds('../../data/urbanformbr/pca_regression_df/classify_uca_isolated.rds')


# merge data --------------------------------------------------------------


  df_merge <- dplyr::left_join(
    df_prep,
    df_pop_censo
  ) %>%
    dplyr::left_join(
      df_pop_growth
      ) %>%
    #dplyr::left_join(
    #  df_fleet
    #) %>%
    dplyr::left_join(
      df_fuel
    ) %>%
    dplyr::left_join(
      df_energy
    ) %>%
    #dplyr::left_join(
    #  df_pib
    #) %>%
    dplyr::left_join(
      df_area
    ) %>%
    dplyr::left_join(
      df_censo
    ) %>%
    dplyr::left_join(
      df_exp_density
    ) %>%
    dplyr::left_join(
      df_landuse
    )

  df_merge <- dplyr::left_join(
    df_merge,
    df_street,
    by = c('name_urban_concentration' = 'name_urban_concentration')
  )

  df_merge <- dplyr::left_join(
    df_merge,
    df_frag_comp,
    by = c('code_urban_concentration' = 'code_urban_concentration')
  )

  df_merge <- dplyr::left_join(
    df_merge, df_topo,
    by = c('code_urban_concentration' = 'code_urban_concentration')
  )

  df_merge <- dplyr::left_join(
    df_merge, df_classify_uca_large,
    by = c('code_urban_concentration' = 'code_urban_concentration')
  ) %>%
    #dplyr::left_join(df_classify_isolated) %>%
    dplyr::left_join(df_classify_tma)

  # calculate fuel consumption per capita
  df_merge <- df_merge %>%
    mutate(fuel_consumption_per_capita_2010 = fuel_consumption_total_2010 / pop_2015)

  df_merge$fuel_energy_per_capita


  # * reorder columns -------------------------------------------------------
  df_merge <- df_merge %>%
    dplyr::relocate(
      c(fuel_consumption_per_capita_2010, fuel_consumption_total_2010, fuel_energy_per_capita, wghtd_mean_commute_time),
      .after = name_uca_case
      )

  df_merge <- df_merge %>%
    dplyr::relocate(
      c(large_uca_pop:tma),
      .after = wghtd_mean_commute_time
    )

  # * add prefix (dependent & independent variable) -------------------------

  df_merge <- df_merge %>%
    # id
    dplyr::rename_with(
      .cols = code_urban_concentration:name_uca_case,
      function(x){paste0("i_", x)}
    ) %>%
    # dummy variables
    dplyr::rename_with(
      .cols = large_uca_pop:tma,
      function(x){paste0("d_", x)}
    ) %>%
    # dependent variables (y)
    dplyr::rename_with(
      .cols = fuel_consumption_per_capita_2010:wghtd_mean_commute_time,
      function(x){paste0("y_", x)}
    ) %>%
    # independent variables (x)
    dplyr::rename_with(
      .cols = pop_2015:length(.),
      function(x){paste0("x_", x)}
    )

head(df_merge)
# save data ---------------------------------------------------------------

  # check if there is any missing values
  any(is.na(df_merge))
  # complete.cases to avoid missing values
  #df_merge <- df_merge[complete.cases(df_merge)]


  readr::write_rds(
    x = df_merge,
    file = '../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds',
    compress = 'gz'
  )

