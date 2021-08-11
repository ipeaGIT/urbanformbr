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
    dplyr::select(code_urban_concentration,year,fuel_consumption_per_capita)

  # filter only 184 from our df
  df_fuel <- subset(df_fuel, code_urban_concentration %in% df_prep$code_urban_concentration)

  df_fuel <- df_fuel %>%
    dplyr::arrange(year)

  df_fuel <- df_fuel %>%
    tidyr::pivot_wider(
    names_from = c("year"),
    values_from = c("fuel_consumption_per_capita"),
    names_prefix = "fuel_consumption_per_capita_"
  )

  df_fuel <- df_fuel %>%
    dplyr::select(code_urban_concentration, fuel_consumption_per_capita_2010)

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

  df_pib <- df_pib %>%
    dplyr::select(code_urban_concentration, pib_capita_2010)


  # * area coverage ---------------------------------------------------------
  df_area <- readr::read_rds("../../data/urbanformbr/pca_regression_df/area.rds") %>%
    dplyr::select(code_muni,urban_extent_size_2014,urban_extent_horizontal_geometric_growth) %>%
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
      -dplyr::matches("abs_diff")
      ) %>%
    dplyr::rename(code_urban_concentration = code_muni)

  # * landuse metrics -------------------------------------------------------
  df_landuse <- data.table::fread("../../data/urbanformbr/pca_regression_df/landuse_mix_metrics.csv") %>%
    dplyr::select(-name_uca_case) %>%
    dplyr::rename(code_urban_concentration = code_muni)


  # * street metrics --------------------------------------------------------
  df_street <- data.table::fread("../../data/urbanformbr/pca_regression_df/streets_metrics.csv")

  df_street <- subset(df_street, name_urban_concentration %in% df_prep$name_urban_concentration)

  # * fragmentation compacity -----------------------------------------------
  df_frag_comp <- data.table::fread("../../data/urbanformbr/pca_regression_df/fragmentation_compacity.csv") %>%
    select(-name_uca_case) %>%
    dplyr::rename(code_urban_concentration = code_muni)

  # * topography ------------------------------------------------------------
  df_topo <- readr::read_rds("../../data/urbanformbr/pca_regression_df/topography.rds") %>%
    dplyr::select(-c(name_uca_case,mean_ruggedness)) %>%
    dplyr::rename(code_urban_concentration = code_muni)

  # * Transport System -----------------------------------------------------
rede_tma <- readRDS("../../data/urbanformbr/rede_tma.rds/") 
tpocorredor <- as.data.frame(
  cbind(rede_tma$corredores$modo,rede_tma$estacoes$cidade,rede_tma$corredores$ano)) %>%
  dplyr::filter( V3>1 & V3<2011) %>% mutate(V1=NULL, V3=1) %>% unique()
colnames(tpocorredor) <- c("i_code_urban_name","D_SistBMT")
i_code_urban_conc <- data.frame(i_code_urban_conc = c(3304557,5300108,3106200,5208707,3170107,3550308,
                 3170206,4106902,2611606,0,4314902,4204608,2507507,1501402,2304400,4205407,2927408,2211001,
                 2307304,2111300,2408102,2704302,3548500,2312908)) 
tpocorredor <- cbind(tpocorredor,i_code_urban_conc)
pca_regression_df_ready_to_use <-  pca_regression_df_ready_to_use.rds %>% dplyr::left_join(
  tpocorredor, by = c("i_code_urban_concentration" = "i_code_urban_conc")) 
pca_regression_df_ready_to_use <- dplyr::mutate_all(pca_regression_df_ready_to_use, ~replace(.,is.na(.),0)) 

# merge data --------------------------------------------------------------


  df_merge <- dplyr::left_join(
    df_prep,
    df_pop_censo
  ) %>%
    dplyr::left_join(
      df_pop_growth
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


  # * reorder columns -------------------------------------------------------
  df_merge <- df_merge %>%
    dplyr::relocate(
      c(fuel_consumption_per_capita_2010, wghtd_mean_commute_time),
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
      .cols = fuel_consumption_per_capita_2010:wghtd_mean_commute_time,
      function(x){paste0("y_", x)}
    ) %>%
    # independent variables (x)
    dplyr::rename_with(
      .cols = pop_2015:length(.),
      function(x){paste0("x_", x)}
    )



# save data ---------------------------------------------------------------

  # check if there is any missing values
  any(is.na(df_merge))
  # complete.cases to avoid missing values
  #df_merge <- df_merge[complete.cases(df_merge)]


  saveRDS(
    object = df_merge,
    file = '../../data/urbanformbr/pca_regression_df/pca_regression_df_ready_to_use.rds',
    compress = 'xz'
  )

