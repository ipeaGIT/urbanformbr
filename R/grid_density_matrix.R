
# Create function, matrix version -----------------------------------------
get_density_matrix <- function(df_urban_areas){

  points_latlon <- suppressWarnings(
    sf::st_transform(df_urban_areas, 4326) %>%
      sf::st_coordinates()
  )

  # message("Calculating distance matrix")
  system.time(distance_matrix <- geodist::geodist(points_latlon, points_latlon, measure = "cheap"))
  # Santa Maria - 11674 points
  # user  system elapsed
  # 3.450   1.002   4.488
  gc(reset = T,full = T)
  gc(reset = T,full = T)

  pop_vector <- df_urban_areas$POP

  ### 1 kilometer
  distance_matrix_01km <- distance_matrix
  distance_matrix_01km[distance_matrix <= 1000] <- 1
  distance_matrix_01km[distance_matrix >  1000] <- 0

  pop_01km <- distance_matrix_01km %*% pop_vector
  rm(distance_matrix_01km)

  ### 5 kilometers
  distance_matrix_05km <- distance_matrix
  distance_matrix_05km[distance_matrix <= 5000] <- 1
  distance_matrix_05km[distance_matrix >  5000] <- 0

  pop_05km <- distance_matrix_05km %*% pop_vector
  rm(distance_matrix_05km)

  ### 10 kilometers
  distance_matrix_10km <- distance_matrix
  distance_matrix_10km[distance_matrix <= 10000] <- 1
  distance_matrix_10km[distance_matrix >  10000] <- 0

  pop_10km <- distance_matrix_10km %*% pop_vector
  rm(distance_matrix_10km)

  temp_output <- data.table('ID_UNICO' = df_metro_temp$ID_UNICO,
                            'code_muni' = df_metro_temp$code_muni,
                            'abbrev_state' = df_metro_temp$abbrev_state,
                            'name_metro'= df_metro_temp$name_metro,
                            'pop_01km' = pop_01km,
                            'pop_05km' = pop_05km,
                            'pop_10km' = pop_10km)
  return(temp_output)
}

gc(reset = T, full = T)


# set number of cores
options(mc.cores=20)


# Apply function in parallel
options( future.globals.maxSize = 10000 * 1024 ^ 2 )
future::plan(strategy = 'multisession', workers=10)

areas <- unique(df_urb_concentration$code_urban_concentration)
s <- "4316907" ## Santa Maria, RS
s <- "3550308" ## Sao paulo, SP

for( s in areas[1:3]){ # states

  message(paste0("\n working on ", s,"\n"))

  # subset area
  df_urban_areas <- subset(df_urb_concentration, code_urban_concentration == s)

  # Apply function in parallel
  system.time(output_df <- get_density_matrix(df_urban_areas))
  # Tempo Santa Maria
  # user  system elapsed
  # 10.404   4.306  14.911

  # save
  fwrite(output_df, paste0('output_density_m_',s,'.csv') )

}






