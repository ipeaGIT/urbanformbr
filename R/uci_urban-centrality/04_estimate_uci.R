
# description -------------------------------------------------------------

# estimate UCI - Urban Centrality Index

# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")


# setup parallel ----------------------------------------------------------

future::plan(future::multicore, workers = future::availableCores() / 2)


# functions ---------------------------------------------------------------

# * support functions -----------------------------------------------------

# Venables, Spatial Separation index
venables <- function(b, d){

  v <- t(b) %*% d %*% b

  return(v[1])
}

# Location Coefficient (LC)
location_coef <- function(x){

  cl <-(sum(abs(x-(1/length(x)))))/2

  return(cl)
}

# uci function ------------------------------------------------------------


f_uci <- function(){

  # read data ---------------------------------------------------------------
  df_uca <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")

  df_rais <- readRDS('../../data/urbanformbr/rais/2010/rais_2010_ghsl_cells_intersect_convex_hull.rds')


  # change projection to UTM
  df_rais <- suppressWarnings(sf::st_transform(df_rais, 3857))

  # estimate uci ------------------------------------------------------------
  codigos <- df_uca$code_urban_concentration

  # code_uca <- 1100122
  # ji-parana

  df_uci <- furrr::future_map_dfr(
    codigos,
    function(code_uca){

      message(paste0("\n working on ", code_uca,"\n"))

      df_rais_subset <- subset(df_rais, code_urban_concentration == code_uca)

      data.table::setDT(df_rais_subset)
      set(df_rais_subset, which(is.na(df_rais_subset[["workers_per_cell"]])), "workers_per_cell", 0)
      df_rais_subset <- sf::st_as_sf(df_rais_subset)

      # pre calculations --------------------------------------------------------

      # * determine border polygons ---------------------------------------------

      # get perimeter of whole area
      boundry <- st_union(df_rais_subset) %>%
        sf::st_boundary()

      # Determine border polygons
      int <- st_intersects(df_rais_subset, boundry)
      border <- sapply(1:length(int),function(i){length(int[[i]])})
      df_rais_subset$border <- border

      ## plot
      # plot(df_rais_subset, col=border)

      # ggplot() +
      #   geom_sf(data=df_rais_subset, aes(fill=factor(border))) +
      #   geom_sf(data=boundry, color='red', size=2) +
      #   theme_void()

      # ggplot() +
      # geom_sf(data=df_rais_subset, aes(fill=factor(workers_per_cell))) +
      # geom_sf(data=boundry, color='red', size=2) +
      # theme_void()


      ### normalize distribution of variable
      # var_x <- df_rais_subset[var_name][[1]]
      # var_x <- matrix(var_x, length(var_x),1)
      # var_x_norm <- var_x/sum(var_x) # normalization

      # workers per cell
      workers <- df_rais_subset['workers_per_cell'][[1]]
      workers <- matrix(workers, length(workers),1)
      workers_norm <- workers/sum(workers) # normalization
      # sum(workers_norm)


      ### calculate distance matrix
      coords <- suppressWarnings(st_coordinates( st_centroid(df_rais_subset) ))
      distance <- parallelDist::parDist(coords)
      distance <- as.matrix(distance)
      #plot(coords)

      # # which is faster? R: parallelDist::parDist

      # system.time( distance <- geodist::geodist(coords, measure = "cheap") )
      # system.time( b <- rdist::rdist(coords) )

      # microbenchmarking
      # microbenchmark::microbenchmark(
      #   parallelDist::parDist(coords)
      #   , geodist::geodist(coords, measure = "cheap")
      #   , rdist::rdist(coords)
      #   , times = 3
      # )


      # self distance >> REF Crafts & Mulatu (2006)
      # IMPORTANT: the area must be in the same unit as the distance matrix####
      n_reg <- nrow(distance)
      poly_areas <- st_area(df_rais_subset)
      self <- diag((poly_areas/pi)^(1/2), nrow = n_reg, ncol = n_reg)
      distance_sum <- distance + self ## Sum distance matrix and self-distance


      # UCI and its components --------------------------------------------------

      ## location coefficient
      lc <- location_coef(workers_norm)

      # Spatial separation index (venables)
      v <- venables(b = workers_norm, d = distance_sum)

      ## MAX spatial separation
      # with all activities equally distributed along the border
      b <- border
      b[is.na(b)] <- 0
      b[b == 1] <- 1/length(b[b == 1])
      v_max <- venables(b = b, d = distance_sum)

      # Proximity Index PI
      proximity_idex <- 1-(v/(v_max))

      # UCI
      UCI <- lc * proximity_idex

      #return(UCI[1])
      output_df <- data.frame(
        code_urban_concentration = code_uca,
        uci = UCI,
        location_coef = lc,
        spatial_separation = v,
        spatial_separation_max = v_max
      )

      return(output_df)

    }
  )



# save data ---------------------------------------------------------------

  data.table::fwrite(
    x = df_uci
    , file = '../../data/urbanformbr/consolidated_data/uci_urban_centrality_index.csv'
    , append = F
  )


}


# run function ------------------------------------------------------------

f_uci()
