
# description -------------------------------------------------------------
# this script estimates experienced built-up area (bua) & population density for
#..each uca's urban extent (urban concentraion area (at least 20% built-up: 184 ucas)

# the script estimates densities (bua & pop) for 1975 & 2014:
# 1. each 1km x 1km grid cell in each uca's urban extent (saved at R/GHSL/04_3_create_uca_grids)
# 2. the densities weighted mean for each uca

# experienced densities for 5 and 10 km are estimated for all the above

# setup -------------------------------------------------------------------

source('R/setup.R')

# define function ---------------------------------------------------------

# * function density matrix -----------------------------------------------
# function to extract density matrix for each urban extent (5 & 10 km)

f_get_density_matrix <- function(df_urban_areas){

  # df_urban_areas <- urban_area

  # f_density_uca recebe os centroids
  points_latlon <- suppressWarnings(
    sf::st_centroid(df_urban_areas) %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates()
  )

  # message("Calculating distance matrix")
  system.time(distance_matrix <- geodist::geodist(points_latlon, points_latlon, measure = "cheap"))
  # Santa Maria - 11674 points
  # user  system elapsed
  # 3.450   1.002   4.488
  gc(reset = T,full = T)
  #gc(reset = T,full = T)

  pop_vector <- df_urban_areas$pop

  # comando abaixo so funciona com celulas de 1kmx1km (se for de outro tamanho, deve obter a area
  #..da celula)
  built_vector <- df_urban_areas$built / 100

  ### 5 kilometers
  distance_matrix_05km <- distance_matrix
  distance_matrix_05km[distance_matrix <= 5000] <- 1
  distance_matrix_05km[distance_matrix >  5000] <- 0

  pop_05km <- distance_matrix_05km %*% pop_vector
  built_05km <- distance_matrix_05km %*% built_vector
  rm(distance_matrix_05km)

  ### 10 kilometers
  distance_matrix_10km <- distance_matrix
  distance_matrix_10km[distance_matrix <= 10000] <- 1
  distance_matrix_10km[distance_matrix >  10000] <- 0

  pop_10km <- distance_matrix_10km %*% pop_vector
  built_10km <- distance_matrix_10km %*% built_vector
  rm(distance_matrix_10km)

  temp_output <- data.table('code_muni' = df_urban_areas$code_muni,
                            'name_uca_case' = df_urban_areas$name_uca_case,
                            'cell' = df_urban_areas$cell,
                            'pop' = df_urban_areas$pop,
                            'built' = df_urban_areas$built / 100,
                            'pop_05km' = pop_05km,
                            'pop_10km' = pop_10km,
                            'built_05km' = built_05km,
                            'built_10km' = built_10km,
                            'consolidada' = df_urban_areas$consolidada
  )

  setnames(
    x = temp_output,
    old = c("pop_05km.V1","pop_10km.V1","built_05km.V1","built_10km.V1"),
    new = c("pop_05km","pop_10km","built_05km","built_10km")
  )

  return(temp_output)
}


# * function experienced density ------------------------------------------
# applies f_get_density_matrix to each urban extent & estimates experienced density
# densities for each grid cell (1km x 1km) are then saved
# map_df returns df with all weighted mean experienced densities

#ano <- c(1975,2014)

f_density_uca <- function(ano){
  areas <- read_rds(sprintf("../../data/urbanformbr/ghsl/results/total_area_grid_uca_consolidada_expansao_%s_cutoff20.rds", ano))

  codigos <- unique(areas$code_muni)
  #s <- "3550308" ## Sao paulo, SP

  df_density <- purrr::map_df(
    codigos,
    function(s){
      message(paste0("\n working on ", s,"\n"))

      # subset area
      df_urban_areas <- subset(areas, code_muni == s)

      output_df <- f_get_density_matrix(df_urban_areas)

      #output_df <- temp_output

      # calculate area of buffers
      output_df$area10km2 <- pi * 10^2
      output_df$area05km2 <- pi *  5^2

      # calculate pop density
      output_df$pop_density05km2 <- output_df$pop_05km / output_df$area05km2
      output_df$pop_density10km2 <- output_df$pop_10km / output_df$area10km2
      output_df$built_density05km2 <- output_df$built_05km / output_df$area05km2
      output_df$built_density10km2 <- output_df$built_10km / output_df$area10km2


      # save
      fwrite(output_df, paste0('../../data/urbanformbr/density-experienced/output_density_ghsl_',ano,"_",s,'.csv') )

      # total Pop vs avg Density
      # df total area
      df1 <- output_df[, .(ano = ano, pop_total = sum(pop), built_total = sum(built),
                           density_pop_05km2 = weighted.mean(x=pop_density05km2, w=pop),
                           density_pop_10km2 = weighted.mean(x=pop_density10km2, w=pop),
                           density_built_05km2 = weighted.mean(x=built_density05km2, w=built),
                           density_built_10km2 = weighted.mean(x=built_density10km2, w=built)),
                       by=.(code_muni,name_uca_case) ]

      df1 <- df1 %>%
        mutate(area_type = "total")

      # df group by consolidated (consolidada==1) or expansion area (consolidada==0)
      df2 <- output_df[, .(ano = ano, pop_total = sum(pop), built_total = sum(built),
                           density_pop_05km2 = weighted.mean(x=pop_density05km2, w=pop),
                           density_pop_10km2 = weighted.mean(x=pop_density10km2, w=pop),
                           density_built_05km2 = weighted.mean(x=built_density05km2, w=built),
                           density_built_10km2 = weighted.mean(x=built_density10km2, w=built)),
                       by=.(code_muni,name_uca_case, consolidada) ]

      df2 <- df2 %>%
        dplyr::mutate(area_type = case_when(
          consolidada == 0 ~ "expansao",
          consolidada == 1 ~ "consolidada"
        )) %>%
        dplyr::select(-consolidada)

      # join total area values with consolidated/expansion area
      df_final <- dplyr::bind_rows(df1,df2)

      return(df_final)

    }

  )

  return(df_density)

}

# run function for each year ----------------------------------------------

df_1975 <- f_density_uca(1975)
df_2014 <- f_density_uca(2014)

df_final <- data.table::rbindlist(list(df_1975,df_2014))

df_final[
  ,
  `:=`(
    pop_total_rel_diff = (pop_total - c(pop_total[1], head(pop_total, -1)) ) / c(pop_total[1], head(pop_total, -1)),
    built_total_rel_diff = (built_total - c(built_total[1], head(built_total, -1)) ) / c(built_total[1], head(built_total, -1)),
    density_pop_05km2_rel_diff = (density_pop_05km2 - c(density_pop_05km2[1], head(density_pop_05km2, -1)) ) / c(density_pop_05km2[1], head(density_pop_05km2, -1)),
    density_pop_10km2_rel_diff = (density_pop_10km2 - c(density_pop_10km2[1], head(density_pop_10km2, -1)) ) / c(density_pop_10km2[1], head(density_pop_10km2, -1)),
    density_built_05km2_rel_diff = (density_built_05km2 - c(density_built_05km2[1], head(density_built_05km2, -1)) ) / c(density_built_05km2[1], head(density_built_05km2, -1)),
    density_built_10km2_rel_diff = (density_built_10km2 - c(density_built_10km2[1], head(density_built_10km2, -1)) ) / c(density_built_10km2[1], head(density_built_10km2, -1))
  ),
  by = .(code_muni, name_uca_case, area_type)
]

# salvar base wide -> para diferenciar variaveis
df_final_wide <- tidyr::pivot_wider(
  df_final,
  names_from = c("area_type","ano"),
  values_from = c(
    "pop_total","built_total","density_pop_05km2", "density_pop_10km2",
    "density_built_05km2","density_built_10km2","pop_total_rel_diff","built_total_rel_diff",
    "density_pop_05km2_rel_diff", "density_pop_10km2_rel_diff","density_built_05km2_rel_diff",
    "density_built_10km2_rel_diff"
  )
) %>%
  # exclude vars relative to 1975 containing rel_difference/change
  dplyr::select(-c(dplyr::matches("rel_diff.*1975$"))) %>%
  # rename rel_diff variables
  dplyr::rename_with(
    .cols = c(pop_total_rel_diff_total_2014:density_built_10km2_rel_diff_expansao_2014),
    .fn = ~sub("_2014$","",.)
  )

data.table::setDT(df_final_wide)[
  ,
  `:=`(
    prop_pop_consolidated_area_2014 = pop_total_consolidada_2014 / pop_total_total_2014
  ),
  by = .(code_muni, name_uca_case)
]

# replace na values (itapipoca: expansion area equals to zero)
df_final_wide <- df_final_wide %>%
  mutate(across(everything(), .fns = ~replace_na(.,0)))


# remove pop_total variables:
# already created by pca_regression/00_3_pop_df.R with Censo data
df_final_wide <- df_final_wide %>%
  select(-c(pop_total_total_1975:pop_total_expansao_2014))

# save results ------------------------------------------------------------

saveRDS(
  df_final_wide,
  "../../data/urbanformbr/pca_regression_df/exp_density_ghsl.rds"
)


# plot data ---------------------------------------------------------------


ggplot(data=df_1975) +
  geom_point(aes(x=density_pop_10km2, y=density_built_10km2), alpha=.4) +
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')

ggplot(data=df_2014) +
  geom_point(aes(x=density_pop_10km2, y=density_built_10km2), alpha=.4) +
  scale_x_continuous(trans='log10')+
  scale_y_continuous(trans='log10')





