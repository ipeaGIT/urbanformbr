
# description -------------------------------------------------------------


# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")


# setup parallel ----------------------------------------------------------

future::plan(future::multicore, workers = future::availableCores() / 2)


# function ----------------------------------------------------------------

f_uci <- function(){


# read data ---------------------------------------------------------------


# * uca -------------------------------------------------------------------
  df_uca <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")

  df_uca_muni <- geobr::read_urban_concentrations(simplified = F)


# * RAIS ------------------------------------------------------------------
  df_rais <- readRDS('../../data/urbanformbr/rais/2010/rais_2010_geocoded_sum_of_workers.rds')

# * grid ------------------------------------------------------------------
  df_grid <- readRDS("../../data/urbanformbr/ghsl/results/grid_uca_2014_political_administrative_area.rds")


# prepare -----------------------------------------------------------------
  glimpse(head(df_rais))
  glimpse(head(df_grid))

# * subset columns --------------------------------------------------------
c_rais_rm <- c("code_muni","name_muni","codemun","cep","logradouro")
df_rais[, c(c_rais_rm) := NULL]

df_grid <- df_grid %>%
  select(-c(built, pop, name_uca_case))


# * subset estabs with problem at lon-lon ---------------------------------

df_rais <- subset(df_rais, !nchar(lon) == 0)

# * transform into spatial data -------------------------------------------

# a <- df_rais %>%
#     sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  # why not working?
  # any(is.na(df_rais))

# * make sure same crs ----------------------------------------------------
# sf::crs(df_rais) == sf::crs(df_grid)


# estimate ------------------------------------------------------------------

  codigos <- df_uca$code_urban_concentration

  # code_uca <- 1100122
  # ji-parana

  df_jobs_per_cell <- furrr::future_map_dfr(# furrr::future_map_dfr ? purrr::map_df
    codigos,
    function(code_uca){

      message(paste0("\n working on ", code_uca,"\n"))

      df_rais_subset <- subset(df_rais, code_urban_concentration == code_uca)

      df_grid_subset <- subset(df_grid, code_urban_concentration == code_uca)


      # * transform rais to sf --------------------------------------------------
      df_rais_subset <- df_rais_subset %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

      # * make sure same crs ----------------------------------------------------
      sf::st_crs(df_rais_subset) == sf::st_crs(df_grid_subset)

      # intersects -----------------------------------------------------------------

      # * * full spatial join ---------------------------------------------------
      #spatial join
      # df_join_total <- sf::st_join(
      #   df_grid_subset,
      #   df_rais_subset %>% select(-code_urban_concentration)
      # )
      #
      # df_sum_workers <- data.table::setDT(df_join_total)[
      #   ,
      #   .(workers_per_cell = sum(total_corrigido, na.rm = T)),
      #   by = .(code_urban_concentration, cell)
      # ]
      #
      # df_grid_merge <- merge(
      #   df_grid_subset, df_sum_workers, by = c("code_urban_concentration", "cell")
      #   )

      # df_grid_subset <- df_grid_subset %>%
      #   dplyr::relocate(workers_per_cell, .before = geometry)

      # # replace NA with zero
      # data.table::setDT(df_join_total)
      # set(df_join_total, which(is.na(df_join_total[["total_corrigido"]])), "total_corrigido", 0)


      # * * subset way ----------------------------------------------------------

      ## cells with firms

      # subset df_grid_subset cells that contain firms
      df_grid_firms <- df_grid_subset[
        sf::st_intersects(df_grid_subset, df_rais_subset) %>% lengths() > 0,
      ]

      # spatial join
      df_grid_firms <- sf::st_join(
        df_grid_firms,
        df_rais_subset %>% select(-code_urban_concentration)
      )

      df_grid_firms <- setDT(df_grid_firms)[
        ,
        .(workers_per_cell = sum(total_corrigido, na.rm = T)),
        by = .(code_urban_concentration, cell)
      ]

      df_grid_merge <- merge(df_grid_subset, df_grid_firms, all.x = T)

      # df_grid_firms <- df_grid_firms %>%
      #   dplyr::group_by(code_urban_concentration, cell) %>%
      #   dplyr::summarise(workers_per_cell = sum(total_corrigido, na.rm = T)) %>%
      #   ungroup()


      # firms that don't contain firms (hence, n_workers = 0)
      # df_grid_no_firms <- df_grid_subset[
      #   !sf::st_intersects(df_grid_subset, df_rais_subset) %>% lengths() > 0,
      # ]
      #
      # df_grid_no_firms <- df_grid_no_firms %>%
      #   mutate(workers_per_cell = 0) %>%
      #   relocate(workers_per_cell, .before = geometry)
      #
      # df_grid_no_firms <- sf::st_join(
      #   df_grid_no_firms,
      #   df_rais_subset %>% select(-code_urban_concentration)
      # )
      #
      # data.table::setDT(df_grid_no_firms)
      # set(df_grid_no_firms, which(is.na(df_grid_no_firms[["total_corrigido"]])), "total_corrigido", 0)
      #
      # df_grid_no_firms <- df_grid_no_firms %>%
      #   st_as_sf()
      #
      # df_grid_no_firms <- df_grid_no_firms %>%
      #   dplyr::group_by(code_urban_concentration, cell) %>%
      #   dplyr::summarise(workers_per_cell = sum(total_corrigido, na.rm = T)) %>%
      #   ungroup()
      #
      # df_join_total <- data.table::rbindlist(list(df_grid_firms, df_grid_no_firms)) %>%
      #   arrange(cell)

      # * * geos way ------------------------------------------------------------
      # dt_rais_s <- data.table::as.data.table(df_rais_subset)
      # dt_grid_s <- data.table::as.data.table(df_grid_subset)
      #
      # dt_rais_s[, geo := geos::as_geos_geometry(geometry)]
      # dt_grid_s[, geo := geos::as_geos_geometry(geometry)]
      #
      # #### LATER: TRY TO PERFORM ST_JOIN WITH GEOS PACKAGE
      # # see https://dewey.dunnington.ca/post/2022/profiling-point-in-polygon-joins-in-r/
      # dt_join_total <- sf::st_join(
      #   x = df_grid_subset,
      #   y = df_rais_subset %>% select(-code_urban_concentration),
      #   join = sf::st_within
      # )
      #
      # # replace NA with zero
      # data.table::setDT(dt_join_total)
      # set(dt_join_total, which(is.na(dt_join_total[["total_corrigido"]])), "total_corrigido", 0)
      #
      # dt_join_total[, geo := geos::as_geos_geometry(geometry) %>% geos::geos_make_collection()]
      #
      #
      #
      # dt_join_total[
      #   ,
      #   .(workers_per_cell = sum(total_corrigido, na.rm = T)),
      #   by = .(geo)
      # ]
      #
      # dt_join_total %>%
      #   dplyr::group_by(geo, cell) %>%
      #   dplyr::summarise(workers_per_cell = sum(total_corrigido, na.rm = T)) %>%
      #   dplyr::ungroup()
      #
      # # backup
      # a <- dt_join_total[
      #   ,
      #   .(geo = geos::as_geos_geometry(geometry) %>% geos_make_collection()),
      #   by = .(cell)
      # ]


    }
  )

# save data ---------------------------------------------------------------

saveRDS(
  object = df_jobs_per_cell,
  file = '../../data/urbanformbr/rais/2010/rais_2010_jobs_per_cell_ghsl_grid.rds',
  compress = 'xz'
)

}


# run function ------------------------------------------------------------

f_uci()

