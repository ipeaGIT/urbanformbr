
# description -------------------------------------------------------------

# this script plots urban extent (cutoff 20% of built up area) shape and urban shapes
# (political administrative) to show the evolution of the urban extent from 1990 to 2014


# setup -------------------------------------------------------------------
source("R/fun_support/setup.R")
source("R/fun_support/style.R")
source("R/fun_support/colours.R")

# select ucas -------------------------------------------------------------

#built_growth <- data.table::fread("../../data/urbanformbr/consolidated_data/urban_growth_builtarea.csv")

#pop_growth <- data.table::fread("../../data/urbanformbr/consolidated_data/urban_growth_population.csv")

#censo <- data.table::fread("../../data/urbanformbr/consolidated_data/censo_metrics.csv")

#belem: 1501402
#belo horizonte: 3106200
#campo grande: 5002704
#caxias do sul: 4305108
#salvador: 2927408
#teresina: 2211001

codigos <- c(
  1501402
  , 3106200
  , 5002704
  , 4305108
  , 2927408
  , 2211001
)

anos <- c(1990,2000,2014)
  # function ----------------------------------------------------------------
f_prepare <- function(ano){

  # ano <- 1990
  # df <- "urban extent"

  df_full <- readr::read_rds(sprintf("../../data/urbanformbr/ghsl/results/urban_extent_uca_%s_cutoff20.rds",ano))

  f_subset_bind <- function(code_uca){
    df_subset <- subset(df_full, code_urban_concentration == code_uca)

    df_subset <- df_subset %>%
      dplyr::mutate(year = ano) %>%
      dplyr::relocate("year", .before = geometry)

    return(df_subset)
  }

  df_bind <- purrr::map_df(
    .x = codigos,
    ~f_subset_bind(code_uca = .x)
  )

  return(df_bind)

}


# run function ------------------------------------------------------------
df_extents <- purrr::map_df(
  .x = anos,
  ~f_prepare(ano = .x)
)

df_extents <- df_extents %>%
  dplyr::arrange(code_urban_concentration, year)

# read urban shapes (political administrative shape for all uca)
df_shapes <- readr::read_rds("../../data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds")
df_shapes <- subset(df_shapes, code_urban_concentration %in% codigos)


# plot --------------------------------------------------------------------
a <- df_shapes %>%
  dplyr::group_by(name_uca_case) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    plot = purrr::map2(
      .x = data, .y = name_uca_case,
      ~ggplot(data = .x) +
        geom_sf() +
        aop_style() +
        theme(
          axis.line.x = element_blank(),
          axis.text = element_blank()
        ) +
        labs(
          title = .y
        )
    )
  )


f_teste <- function(name){

  # name <- "belo_horizonte_mg"
  extents_s <- subset(df_extents, name_uca_case == name)

  shapes_s <- subset(df_shapes, name_uca_case == name)

  ggplot() +
    geom_sf(
      data = shapes_s
      ,aes()
    ) +
    geom_sf(
      data = extents_s
      ,aes(fill = forcats::fct_rev(factor(year)))
    ) +
    scale_fill_viridis_d() +
    aop_style() +
    theme(
      axis.line.x = element_blank(),
      axis.text = element_blank(),
      legend.position = "bottom"
    ) +
    labs(
      subtitle = shapes_s$name_urban_concentration
      , fill = "Ano"
    ) +
    guides(fill = guide_legend(reverse = T))

}

gg_out <- purrr::map(
  .x = df_shapes$name_uca_case,
  ~f_teste(name = .x)
)

6666666
a <- purrr::reduce(gg_out, .f = `+`)
a + patchwork::plot_layout(guides = "collect") & theme(legend.position = "bottom")

# save plot ---------------------------------------------------------------

