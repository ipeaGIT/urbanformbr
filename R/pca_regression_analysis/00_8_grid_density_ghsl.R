
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
  gc(reset = T,full = T)

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
                            'built_10km' = built_10km
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
  areas <- read_rds(sprintf("../../data/urbanformbr/ghsl/results/grid_uca_%s_cutoff20.rds", ano))

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
      df1 <- output_df[, .(ano = ano, pop_total = sum(pop), built_total = sum(built),
                           density_pop_05km2 = weighted.mean(x=pop_density05km2, w=pop),
                           density_pop_10km2 = weighted.mean(x=pop_density10km2, w=pop),
                           density_built_05km2 = weighted.mean(x=built_density05km2, w=built),
                           density_built_10km2 = weighted.mean(x=built_density10km2, w=built)),
                       by=.(code_muni,name_uca_case) ]
      return(df1)

    }

  )

  return(df_density)

}

# run function for each year ----------------------------------------------

df_1975 <- f_density_uca(1975)
df_2014 <- f_density_uca(2014)


# save results ------------------------------------------------------------

saveRDS(
  df_1975,
  "../../data/urbanformbr/pca_regression_df/exp_density_ghsl_1975.rds"
  )

saveRDS(
  df_2014,
  "../../data/urbanformbr/pca_regression_df/exp_density_ghsl_2014.rds"
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





