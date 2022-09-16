
# description -------------------------------------------------------------


# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")


# setup parallel ----------------------------------------------------------

future::plan(future::multicore, workers = future::availableCores() / 2)


# function ----------------------------------------------------------------

f_convex <- function(){


# read data ---------------------------------------------------------------


# * uca -------------------------------------------------------------------
  df_uca <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")

  df_uca_muni <- geobr::read_urban_concentrations(simplified = F)


# * RAIS ------------------------------------------------------------------
  df_rais <- readRDS('../../data/urbanformbr/rais/2010/rais_2010_jobs_per_cell_ghsl_grid.rds')


# find intersected polygons ------------------------------------------------------------------

  codigos <- df_uca$code_urban_concentration

  # code_uca <- 1100122
  # ji-parana

  df_intersects_convex <- furrr::future_map_dfr(# furrr::future_map_dfr ? purrr::map_df
    codigos,
    function(code_uca){

      message(paste0("\n working on ", code_uca,"\n"))

      df_rais_subset <- subset(df_rais, code_urban_concentration == code_uca)

      # subset non-empty cells --------------------------------------------------
      df_non_empty <- subset(df_rais_subset, !is.na(workers_per_cell))

      # create unioned convex polygon -------------------------------------------
      df_convex_hull <- st_convex_hull(st_union(df_non_empty))

      # st intersects: grid cells & convex polygon ------------------------------
      # select grid cells that intersect with convex hull polygon
      df_intersects <- df_rais_subset[
        sf::st_intersects(df_rais_subset, df_convex_hull) %>% lengths() > 0,
      ]

      # check polygons
      # pol_adm <- df_uca %>% filter(code_urban_concentration==code_uca) %>% select(geometry)
      #
      # ggplot() +
      #   geom_sf(data = pol_adm, aes(geometry = geometry)) +
      #   geom_sf(data = df_rais_subset) +
      #   geom_sf(data = df_non_empty, fill = "blue") +
      #   geom_sf(data = df_convex_hull, colour = "red", fill = NA)
      #

    }
  )

# save data ---------------------------------------------------------------

saveRDS(
  object = df_intersects_convex,
  file = '../../data/urbanformbr/rais/2010/rais_2010_ghsl_cells_intersect_convex_hull.rds',
  compress = 'xz'
)

}


# run function ------------------------------------------------------------

f_uci()

