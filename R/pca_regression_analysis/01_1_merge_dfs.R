# description -------------------------------------------------------------

# this script merge all the dfs containing variables to be used at exploratory
#..and regression analysis, created at R/pca_regression_analysis/00_x_...

# setup -------------------------------------------------------------------
rm(list=ls())
source('R/setup.R')

# read and clean data ---------------------------------------------------------------


  # * prep data -------------------------------------------------------------
  df_prep <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pca_regression_df.rds") %>%
    # QUAL COLUNA DE NOME REMOVER?
    dplyr::select(-c(code_muni_uca))


  # * fleet -----------------------------------------------------------------
  # CONSERTAR: REMOVER VALORES REPETIDOS PARA UCA

  df_fleet <- readr::read_rds("../../data/urbanformbr/pca_regression_df/fleet_and_pop.rds") %>%
    janitor::clean_names() %>%
    dplyr::select(-c(uf,total_autos,total_motos,pop))

  # filter only 184 from our df
  df_fleet <- subset(df_fleet, code_urban_concentration %in% df_prep$code_urban_concentration)

  df_fleet <- df_fleet %>%
   tidyr::pivot_wider(
     names_from = c("ano"),
    values_from = c('autos_per_pop','motos_per_pop','motorization_rate')
   )

  # * pop -------------------------------------------------------------------

  df_pop <- readr::read_rds("../../data/urbanformbr/pca_regression_df/1970-2015_pop.rds")

  # filter only 184 from our df
  df_pop <- subset(df_pop, code_urban_concentration %in% df_prep$code_urban_concentration)

  df_pop <- df_pop %>%
    tidyr::pivot_wider(
      names_from = c("ano"),
      values_from = c("pop"),
      names_prefix = "pop_"
    )

  data.table::setDT(df_pop)[
    ,
    pop_geom_growth_1970_2015 := ( (pop_2015 / pop_1970) ^ (1/45) ) - 1,
    by = .(code_urban_concentration)
  ]


  # * fuel ------------------------------------------------------------------

  df_fuel <- readr::read_rds("../../data/urbanformbr/pca_regression_df/fuel.rds") %>%
    dplyr::select(code_urban_concentration,year,fuel_consumption_per_capita)

  # filter only 184 from our df
  df_fuel <- subset(df_fuel, code_urban_concentration %in% df_prep$code_urban_concentration)

  df_fuel <- df_fuel %>%
    dplyr::arrange(year)

  df_fuel <- df_fuel %>%
    tidyr::pivot_wider(
    names_from = c("year"),
    values_from = c("fuel_consumption_per_capita"),
    , names_prefix = "fuel_consum_capita_"
  )

  # * pib -------------------------------------------------------------------
  df_pib <- readr::read_rds("../../data/urbanformbr/pca_regression_df/pib.rds") %>%
    dplyr::select(-c(valor,pop))

  # filter only 184 from our df
  df_pib <- subset(df_pib, code_urban_concentration %in% df_prep$code_urban_concentration)



  df_pib <- df_pib %>%
    tidyr::pivot_wider(
      names_from = c("ano"),
      values_from = c("pib_capita"),
      names_prefix = "pib_capita_"
    )

  # * area coverage ---------------------------------------------------------
  df_area <- readr::read_rds("../../data/urbanformbr/pca_regression_df/area.rds") %>%
    dplyr::select(-name_uca_case) %>%
    dplyr::rename(code_urban_concentration = code_muni)

  # * censo -----------------------------------------------------------------
  df_censo <- readr::read_rds("../../data/urbanformbr/pca_regression_df/censo.rds") %>%
    dplyr::select(-name_uca_case)

  # * experienced density ---------------------------------------------------
  df_exp_density <- readr::read_rds("../../data/urbanformbr/pca_regression_df/exp_density_ghsl.rds") %>%
    dplyr::select(-name_uca_case) %>%
    dplyr::rename(code_urban_concentration = code_muni)

  # * landuse metrics -------------------------------------------------------
  df_landuse <- data.table::fread("../../data/urbanformbr/pca_regression_df/landuse_mix_metrics.csv") %>%
    dplyr::select(-name_uca_case) %>%
    dplyr::rename(code_urban_concentration = code_muni)


# merge data --------------------------------------------------------------

  # ADICIONAR df_fleet QUANDO RESOLVER QUESTAO DE QUANTIDADE OBSERVACOES

  df_merge <- dplyr::left_join(
    df_prep,
    df_pop
  ) %>%
    dplyr::left_join(
      df_fuel
    ) %>%
    dplyr::left_join(
      df_pib
    ) %>%
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


  # * reorder columns -------------------------------------------------------
  df_merge <- df_merge %>%
    dplyr::relocate(
      c(fuel_consum_capita_2001:fuel_consum_capita_2018, wghtd_mean_commute_time),
      .after = name_uca_case
      )

  # * add prefix (dependent & independent variable) -------------------------
  df_merge <- df_merge %>%
    # id
    dplyr::rename_with(
      .cols = code_urban_concentration:name_uca_case,
      function(x){paste0("i_", x)}
    ) %>%
    # dependent variables (y)
    dplyr::rename_with(
      .cols = fuel_consum_capita_2001:wghtd_mean_commute_time,
      function(x){paste0("y_", x)}
    ) %>%
    # independent variables (x)
    dplyr::rename_with(
      .cols = pop_1970:theil_h,
      function(x){paste0("x_", x)}
    )



# save data ---------------------------------------------------------------

  # complete.cases to avoid missing values
  df_merge <- df_merge[complete.cases(df_merge)]


  saveRDS(
    object = df_merge,
    file = '../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds',
    compress = 'xz'
  )

