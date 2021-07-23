# description -------------------------------------------------------------

# UPDATE DESCRIPTION

# setup -------------------------------------------------------------------

source('R/setup.R')

# define function ---------------------------------------------------------

# year <- 2014

f_create_expansion_polygon <- function(var) {


  # * read data -------------------------------------------------------------

  polygon_built_area_1975 <- readr::read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_1975_cutoff20.rds")

  polygon_built_area_2014 <- readr::read_rds("../../data/urbanformbr/ghsl/results/urban_extent_uca_2014_cutoff20.rds")


# * clean data ------------------------------------------------------------

  polygon_built_area_1975 <- split(polygon_built_area_1975, polygon_built_area_1975$name_uca_case)

  polygon_built_area_2014 <- split(polygon_built_area_2014, polygon_built_area_2014$name_uca_case)

  polygon_built_area_2014 <- polygon_built_area_2014 %>%
    purrr::map(
      ~dplyr::select(., geometry)
    )


  # * define expansion area polygon -----------------------------------------

  polygon_expansion_area <- purrr::map2(
    .x = polygon_built_area_1975, .y = polygon_built_area_2014,
    function(x,y)
      sf::st_sym_difference(x = x, y = y)
  )

  # atencao: expansion area em itapipoca (2306405) equals to zero: no rows at polygon_expansion_area

  df_polygon_expansion_area <- bind_rows(polygon_expansion_area)

  # add itapipoca row
  df_polygon_expansion_area[nrow(df_polygon_expansion_area) + 1, ] <- list(2306405,"itapipoca",NA)

  df_polygon_expansion_area <- df_polygon_expansion_area %>%
    dplyr::arrange(name_uca_case)


  # * save data -------------------------------------------------------------

  saveRDS(
    object = df_polygon_expansion_area,
    file = '../../data/urbanformbr/ghsl/results/expansion_area_cutoff20.rds',
    compress = 'xz'
  )

}


# run function ------------------------------------------------------------


