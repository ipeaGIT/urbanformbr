
# description -------------------------------------------------------------
# this script estimates experienced built-up area (bua) & population density for
#..each uca's urban extent (urban concentraion area (at least 20% built-up: 184 ucas)

# the script estimates densities (bua & pop) for 1975 & 2014:
# 1. each 1km x 1km grid cell in each uca's urban extent (saved at R/GHSL/04_3_create_uca_grids)
# 2. the densities weighted mean for each uca

# experienced densities for 1, 2, 5 and 10 km are estimated for all the above

# setup -------------------------------------------------------------------

source('R/fun_support/setup.R')

# setup parallel ----------------------------------------------------------

future::plan(future::multicore, workers = future::availableCores() / 2)

# define function ---------------------------------------------------------

# * function density matrix -----------------------------------------------
# function to extract density matrix for each urban extent (5 & 10 km)

f_get_density_matrix <- function(df_urban_area_cutoff20, df_uca_pop_built){

  # df_urban_areas <- urban_area

  # f_density_uca recebe os centroids
  points_latlon_origem <- suppressWarnings(
    sf::st_centroid(df_urban_area_cutoff20) %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates()
  )

  points_latlon_destino <- df_uca_pop_built[, .(x,y)]

  # ler aquivo raster (area politico administrativo)

  # message("Calculating distance matrix")
  # geodist(latlon_origem, latlon_destino <- <- )
  system.time(distance_matrix <- geodist::geodist(
    points_latlon_origem,
    points_latlon_destino,
    measure = "cheap")
  )
  # Santa Maria - 11674 points
  # user  system elapsed
  # 3.450   1.002   4.488
  gc(reset = T, full = T)
  #gc(reset = T,full = T)

  pop_vector <- df_uca_pop_built$pop

  # comando abaixo so funciona com celulas de 1kmx1km (se for de outro tamanho, deve obter a area
  #..da celula)
  built_vector <- df_uca_pop_built$built / 100


  # * * matrices ------------------------------------------------------------

  ### 1 kilometer
  # utilizar 1100 para ter certeza que pegamos matriz pesos espaciais rookie (torre)
  distance_matrix_01km <- distance_matrix
  distance_matrix_01km[distance_matrix <= 1000] <- 1
  distance_matrix_01km[distance_matrix >  1000] <- 0

  pop_01km <- distance_matrix_01km %*% pop_vector
  built_01km <- distance_matrix_01km %*% built_vector
  rm(distance_matrix_01km)

  ### 2 kilometers
  # utilizar 2100 para ter certeza que pegamos pelo menos 2 km
  # (em funcao da incerteza sobre geodist(measure = "cheap"))
  distance_matrix_02km <- distance_matrix
  distance_matrix_02km[distance_matrix <= 2000] <- 1
  distance_matrix_02km[distance_matrix >  2000] <- 0

  pop_02km <- distance_matrix_02km %*% pop_vector
  built_02km <- distance_matrix_02km %*% built_vector
  rm(distance_matrix_02km)

  ### 3 kilometers
  distance_matrix_03km <- distance_matrix
  distance_matrix_03km[distance_matrix <= 3000] <- 1
  distance_matrix_03km[distance_matrix >  3000] <- 0

  pop_03km <- distance_matrix_03km %*% pop_vector
  built_03km <- distance_matrix_03km %*% built_vector
  rm(distance_matrix_03km)

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


  # * * temp output ---------------------------------------------------------

  temp_output <- data.table('code_urban_concentration' = df_urban_area_cutoff20$code_urban_concentration,
                            'name_uca_case' = df_urban_area_cutoff20$name_uca_case,
                            'cell' = df_urban_area_cutoff20$cell,
                            'pop' = df_urban_area_cutoff20$pop,
                            'built' = df_urban_area_cutoff20$built / 100,
                            'pop_01km' = pop_01km,
                            'pop_02km' = pop_02km,
                            'pop_03km' = pop_03km,
                            'pop_05km' = pop_05km,
                            'pop_10km' = pop_10km,
                            'built_01km' = built_01km,
                            'built_02km' = built_02km,
                            'built_03km' = built_03km,
                            'built_05km' = built_05km,
                            'built_10km' = built_10km
  )

  setnames(
    x = temp_output,
    old = c(
      "pop_01km.V1","pop_02km.V1","pop_03km.V1","pop_05km.V1","pop_10km.V1",
      "built_01km.V1","built_02km.V1","built_03km.V1","built_05km.V1","built_10km.V1"
    ),
    new = c(
      "pop_01km","pop_02km","pop_03km","pop_05km","pop_10km",
      "built_01km","built_02km","built_03km","built_05km","built_10km"
    )
  )

  return(temp_output)
}


# * function experienced density ------------------------------------------
# applies f_get_density_matrix to each urban extent & estimates experienced density
# densities for each grid cell (1km x 1km) are then saved
# map_df returns df with all weighted mean experienced densities

f_density_uca <- function(ano){

  areas <- read_rds(sprintf("../../data/urbanformbr/ghsl/results/grid_uca_%s_cutoff20.rds", ano))

  codigos <- unique(areas$code_urban_concentration)
  #code_uca <- "3550308" ## Sao paulo, SP

  df_density <- purrr::map_df(
    codigos,
    function(code_uca){
      message(paste0("\n working on ", code_uca,"\n"))

      # subset area
      df_urban_area <- subset(areas, code_urban_concentration == code_uca)

      nome_uca <- unique(df_urban_area$name_uca_case)

      # read raster -------------------------------------------------------------

      if (ano == 2014) {
        ano_pop = 2015
      } else {ano_pop = ano}

      # read raster pop
      uca_pop <- raster::raster(sprintf("../../data/urbanformbr/ghsl/POP/UCA/GHS_POP_E%s_%s_R2019A_54009_1K_V1_0_raster.tif", ano_pop, code_uca)) %>%
        raster::projectRaster(crs = 4326)

      # convert raster to data.frame
      df_uca_pop <- uca_pop %>%
        raster::as.data.frame(xy = T)

      # rename third column
      data.table::setnames(df_uca_pop, 3, "pop")

      # filter cells with pop > 0
      df_uca_pop <- data.table::setDT(df_uca_pop)[pop > 0]

      # read raster built
      uca_built <- raster::raster(
        sprintf("../../data/urbanformbr/ghsl/BUILT/UCA/GHS_BUILT_LDS%s_%s_R2018A_54009_1K_V2_0_raster.tif", ano, code_uca)) %>%
        raster::projectRaster(crs = 4326)

      # convert to data.frame
      df_uca_built <- uca_built %>%
        raster::as.data.frame(xy = T)

      data.table::setnames(df_uca_built, 3, "built")

      df_uca_built <- data.table::setDT(df_uca_built)[built > 0]

      # join uca pop & built
      df_uca_pop_built <- merge(df_uca_pop, df_uca_built, all = T)

      # fill NA values with zero
      data.table::setnafill(df_uca_pop_built, fill = 0)

      # run f_get_density_matrix ------------------------------------------------------------

      output_df <- f_get_density_matrix(df_urban_area, df_uca_pop_built)

      #output_df <- temp_output

      # calculate area of buffers
      output_df$area01km <- pi *  1^2
      output_df$area02km <- pi *  2^2
      output_df$area03km <- pi *  3^2
      output_df$area05km <- pi *  5^2
      output_df$area10km <- pi * 10^2


      # calculate density
      output_df$pop_density01km <- output_df$pop_01km / output_df$area01km
      output_df$pop_density02km <- output_df$pop_02km / output_df$area02km
      output_df$pop_density03km <- output_df$pop_03km / output_df$area03km
      output_df$pop_density05km <- output_df$pop_05km / output_df$area05km
      output_df$pop_density10km <- output_df$pop_10km / output_df$area10km
      output_df$built_density01km <- output_df$built_01km / output_df$area01km
      output_df$built_density02km <- output_df$built_02km / output_df$area02km
      output_df$built_density03km <- output_df$built_03km / output_df$area03km
      output_df$built_density05km <- output_df$built_05km / output_df$area05km
      output_df$built_density10km <- output_df$built_10km / output_df$area10km


      # save
      fwrite(
        output_df,
        sprintf("../../data/urbanformbr/density-experienced/output_density_ghsl_%s_%s.csv", ano, code_uca)
      )

      # total Pop vs avg Density
      # df total area
      df_final <- output_df[
        ,
        .(
          ano = ano,
          pop_total = sum(pop),
          built_total = sum(built),
          density_pop_01km = weighted.mean(x=pop_density01km, w=pop),
          density_pop_02km = weighted.mean(x=pop_density02km, w=pop),
          density_pop_03km = weighted.mean(x=pop_density03km, w=pop),
          density_pop_05km = weighted.mean(x=pop_density05km, w=pop),
          density_pop_10km = weighted.mean(x=pop_density10km, w=pop),
          density_built_01km = weighted.mean(x=built_density01km, w=built),
          density_built_02km = weighted.mean(x=built_density02km, w=built),
          density_built_03km = weighted.mean(x=built_density03km, w=built),
          density_built_05km = weighted.mean(x=built_density05km, w=built),
          density_built_10km = weighted.mean(x=built_density10km, w=built)
        ),
        by = .(code_urban_concentration, name_uca_case)
      ]

      return(df_final)

    }

  )

  return(df_density)

}

# run for mulitple years --------------------------------------------------

anos <- c("1990","2000","2014")

df_final <- furrr::future_map_dfr(anos, ~f_density_uca(.))


# pivot to wide -----------------------------------------------------------

df_final[, pop_total := NULL]
df_final[, built_total := NULL]

df_final_wide <- tidyr::pivot_wider(
  data = df_final
  , names_from = ano
  , values_from = density_pop_01km:density_built_10km
)

# save results ------------------------------------------------------------

data.table::fwrite(
  x = df_final_wide
  , file = '../../data/urbanformbr/consolidated_data/ghsl_experienced_density_metrics.csv'
  , sep = ";"
  , append = F
)

