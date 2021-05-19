# description -------------------------------------------------------------

# this script tests criteria for defining built-up area polygon (urban extent)
#..to be compared between Urban Concentration Areas (uca)
# built-up area raster (GSHL) used is 1km x 1km pixel
# testing is done in two steps

# 1. preliminary test for selected group of ucas:
## define different cutoffs (>0%, 10%, 25%, 50%) that constitute built-up
##..area polygon for a group of ucas
## generate maps (1975 & 2014) to be analysed by AOP team
# ucas: for,bsb,bhz,ctb,poa,rio,sp,nat,man,vit

# 2. comparison between cutoffs and "ground truth" (IBGE urban footprint)
## based on preview analysis, compare 10% & 25% cutoffs with IBGE urban..
##..footprint.
## check which cutoff provides the least amount of difference when compared to
##.. IBGE's area

# setup -------------------------------------------------------------------

source("R/setup.R")
source('R/style.R')
source('R/colours.R')

# directory ---------------------------------------------------------------

ghsl_built_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/"


# 1 preliminary function: inspect 9 cities  -------------------------------

### PROJECTION: O QUE FAZER? fazer projecao antes ou depois de extrair o valor?

### CLASSIFICACAO: QUAL CRITERIO? qual criterio (em termos quant.) de area
# construida para classifica-la como "centro urbano" (e com isso obter..
# ..o poligono de area construida de cada ano)


# * 1.1 define files ------------------------------------------------------

files_preliminary <- dir(
  "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/",
  pattern = "(1975|2014).*(sao_paulo|fortaleza|brasilia|curitiba|belo_horizonte|rio_de_janeiro|bage|porto_alegre|vitoria_es).*\\.tif$"
)

#input <- files_preliminary

# * 1.2 define function ---------------------------------------------------

f_preliminary <- function(input) {

  # read all raster files from one year in a list
  bua_uca <- purrr::map(input, ~ raster::raster(paste0(ghsl_built_dir, .)))

  # extract the year
  anos <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS)[0-9]{4}"))

  # extract uca name
  uca_name <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2)"))

  # add years to name
  uca_name <- paste0(uca_name,'_',anos)

  # rename each raster in the list
  names(bua_uca) <- uca_name

  #str(bua_uca,max.level = 1)


  # * function raster polygons and classify -----------------------------------

  f_raster_pol_class <- function(bua_raster){

  bua_pol <- bua_raster %>%
    # convert raster to polygon (sp)
    raster::rasterToPolygons() %>%
    # transform to sf
    sf::st_as_sf() %>%
    # rename first column
    dplyr::rename(bua_value = 1) %>%
    # create columns classifying area based on cutoff values (0,25,50%)
    dplyr::mutate(
      cutoff_0 = dplyr::case_when(
        bua_value > 0 ~ "Construída",
        T ~ "Não construida"
      ),
      cutoff_10 = dplyr::case_when(
        bua_value >= 10 ~ "Construída",
        T ~ "Não construida"
      ),
      cutoff_25 = dplyr::case_when(
        bua_value >= 25 ~ "Construída",
        T ~ "Não construida"
      ),
      cutoff_50 = dplyr::case_when(
        bua_value >= 50 ~ "Construída",
        T ~ "Não construida"
        )
    )

  }

  # run for every uca
  bua_pol <- purrr::map(bua_uca, f_raster_pol_class)


  # * convert crs polygon and summarise -------------------------------------
  f_group_summarise <- function(base, variavel){

    variavel <- rlang::ensym(variavel)

    base %>%
      dplyr::group_by(!!variavel) %>%
      dplyr::summarise()

  }

  # create column name vector
  vetor <- paste0('cutoff_',c(0,10,25,50))
  # generate converted list of sf df
  #a <- pmap(list(variavel = vetor), f_group_summarise, base = bua_pol$bage_rs_1975)

  bua_convert <- map(bua_pol,
           ~pmap(list(variavel = vetor), f_group_summarise, base = .)
           )

  f_names <- function(base){

    rlang::set_names(base, paste0('cutoff_',c(0,10,25,50)))

  }

  # rename list elements
  bua_convert <- purrr::map(bua_convert, ~f_names(.))

  # reproject crs
  bua_convert <- purrr::modify_depth(
    .x = bua_convert, .depth = 2, ~ sf::st_transform(., crs = 4326)
      )


  # * plot data -------------------------------------------------------------
  f_plot <- function(base, column){

    column <- rlang::ensym(column)

    ggplot(data = base, aes(fill = !!column), colour = NA) +
      geom_sf() +
      viridis::scale_fill_viridis(discrete = T) +
      theme_void()

  }


  bua_plots <- purrr::map(
    bua_convert,
    ~purrr::modify_in(., 1, ~f_plot(., 'cutoff_0'))
    )
  bua_plots <- purrr::map(
    bua_plots,
    ~purrr::modify_in(., 2, ~f_plot(., 'cutoff_10'))
  )
  bua_plots <- purrr::map(
    bua_plots,
    ~purrr::modify_in(., 3, ~f_plot(., 'cutoff_25'))
  )
  bua_plots <- purrr::map(
    bua_plots,
    ~purrr::modify_in(., 4, ~f_plot(., 'cutoff_50'))
  )

  bua_reduce <- purrr::map(bua_plots, ~purrr::reduce(., `/`))


  f_titulo <- function(plot_mapa, nomes){

    plot_mapa <- plot_mapa + patchwork::plot_annotation(title = nomes)

  }

  bua_reduce <- purrr::map2(
    .x = bua_reduce, .y = names(bua_reduce), function(x, y)
      f_titulo(plot_mapa = x, nomes = y)
      )

  nomes <- unique(str_extract(names(bua_reduce), ".+(?=_\\d{4})"))
  #nomes <- substr(names(bua_reduce), 1, 8)
  #nomes <- unique(nomes)

  f_select <- function(lista, nomes){

    lista <- names(lista) %>%
      stringr::str_detect(nomes) %>%
      purrr::keep(lista, .)

  }

  bua_select <- purrr::map(nomes, ~f_select(bua_reduce, nomes = .))

  names(bua_select) <- nomes

  bua_compare <- purrr::map(bua_select, function(x)
    plot_grid(pluck(x, 1), pluck(x, 2))
    )


  # * save data -------------------------------------------------------------

  purrr::walk2(bua_compare, names(bua_compare), function(x,y)
    ggplot2::ggsave(
      filename = paste0('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/figures/', y, '.png'),
      plot = x, dpi = 300, device = 'png'
      )
    )



}


# * 1.3 run preliminary function ------------------------------------------

f_preliminary(input = files_preliminary)

# 2 compare cutoffs vs. IBGE urban footprint ------------------------------


# * 2.1 define files ------------------------------------------------------

files_compare <- dir(
  "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/",
  pattern = "(2014).*\\.tif$"
)

# APAGAR DEPOIS !!!!!!!!
#input <- files_preliminary[c(10:17)]
#files_compare <- files_compare[c(1:8)]
#input <- files_compare
#input <- files_compare[[1]]

# * 2.2 define function ---------------------------------------------------

f_compare <- function(){

  # read all raster files from one year in a list
  bua_uca <- purrr::map(input, ~ raster::raster(paste0(ghsl_built_dir, .)))

  # extract the year
  anos <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS)[0-9]{4}"))

  # extract uca name
  uca_name <- purrr::map_chr(input, ~ stringr::str_extract(., "(?<=LDS[\\d]{4}_).+(?=_R2)"))

  # add years to name
  uca_name <- paste0(uca_name,'_',anos)

  # rename each raster in the list
  names(bua_uca) <- uca_name

  # * function raster polygons and classify -----------------------------------

  f_raster_pol_class <- function(bua_raster){

    bua_pol <- bua_raster %>%
      # convert raster to polygon (sp)
      raster::rasterToPolygons() %>%
      # transform to sf
      sf::st_as_sf() %>%
      # rename first column
      dplyr::rename(bua_value = 1) %>%
      # create columns classifying area based on cutoff values (0,25,50%)
      dplyr::mutate(
        cutoff_10 = data.table::fcase(
          bua_value >= 10, 'Construída',
          bua_value < 10, 'Não construída'
        ),
        cutoff_25 = data.table::fcase(
          bua_value >= 25, 'Construída',
          bua_value < 25, 'Não construída'
        )
      )

  }

  # run for every uca
  bua_pol <- purrr::map(bua_uca, f_raster_pol_class)

  # * convert crs polygon and summarise -------------------------------------
  f_group_summarise <- function(base, variavel){

    variavel <- rlang::ensym(variavel)

    base %>%
      dplyr::group_by(!!variavel) %>%
      dplyr::summarise()

  }

  # create column name vector
  vetor <- paste0('cutoff_',c(10,25))
  # generate converted list of sf df

  bua_convert <- map(bua_pol,
                     ~pmap(list(variavel = vetor), f_group_summarise, base = .)
  )

  f_names <- function(base){

    rlang::set_names(base, paste0('cutoff_',c(10,25)))

  }

  # rename list elements
  bua_convert <- purrr::map(bua_convert, ~f_names(.))

  # * reproject crs ---------------------------------------------------------
  # reproject crs
  bua_convert <- purrr::modify_depth(
    .x = bua_convert, .depth = 2, ~ sf::st_transform(., crs = 4326)
  )


  # filter built-up area
  bua_convert <- purrr::modify_depth(.x = bua_convert, .depth = 2,
                           ~dplyr::filter(., .[[1]] == 'Construída')
                           )

  nomes <- stringr::str_extract(
    string = names(bua_convert),
    pattern = ".*(?=_\\d{4})"
    )
  # add uca name to df
  bua_convert <- purrr::map2(
    .x = bua_convert, .y = nomes, function(x,y)
      purrr::map(.x = x, ~dplyr::mutate(., name_muni = y))
  )

  # estimate built-up area
  bua_convert <- purrr::map(
    .x = bua_convert,
    ~purrr::modify_in(., 1, ~dplyr::mutate(., bua_area_10 = units::set_units(sf::st_area(.), value = km^2)))
  )

  bua_convert <- purrr::map(
    .x = bua_convert,
    ~purrr::modify_in(., 2, ~dplyr::mutate(., bua_area_25 = units::set_units(sf::st_area(.), value = km^2)))
  )



  # * create df with footprint areas ----------------------------------------

    # * * create df bua areas ghsl --------------------------------------------

  # footprint areas: bua_area_10, bua_area_25, ibge_area
  # drop geometry
  df_bua_areas <- purrr::modify_depth(
    .x = bua_convert, .depth = 2, ~sf::st_drop_geometry(.)
  )
  # select columns (name_muni and bua_area)
  df_bua_areas <- purrr::modify_depth(
    .x = df_bua_areas, .depth = 2,
    ~dplyr::select(., !dplyr::starts_with('cutoff'))
  )
  # reduce lists by left join dfs
  df_bua_areas <- purrr::map(
    .x = df_bua_areas, ~purrr::reduce(., dplyr::left_join, by = 'name_muni')
  )
  # bind dataframes
  df_bua_areas <- data.table::rbindlist(df_bua_areas)

  # read urban shapes saved at `urban_shapes.R` to get code_muni
  urban_shapes <- readr::read_rds('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/urban_area_shapes/urban_area_pop_100000_dissolved.rds') %>%
    sf::st_drop_geometry() %>%
    data.table::setDT()
  # add code_muni columns to df
  df_bua_areas[
    urban_shapes,
    `:=`(
      code_urban_concentration = i.code_urban_concentration
    ),
    on = c(name_muni = 'name_uca_case')
  ]
  # reorder columns
  df_bua_areas <- df_bua_areas %>%
    dplyr::relocate(code_urban_concentration, .after = name_muni)


    # * * read ibge footprint -------------------------------------------------

  # read ibge area
  ibge <- read_urban_area(year=2015, simplified = F)
  # create column built-up (urban footprint) area ibge
  ibge <- ibge %>%
    dplyr::mutate(bua_area_ibge = units::set_units(sf::st_area(.), value = km^2))
  # aggregate bua ibge by uca (code_muni)
  area_urb_ibge <- data.table::setDT(ibge)[
    ,
    .(bua_area_ibge = sum(bua_area_ibge)),
    by = .(code_muni)
  ]
  # left join ibge areas to df_bua_areas
  df_bua_areas[
    area_urb_ibge,
    `:=`(
      bua_area_ibge = i.bua_area_ibge
    ),
    on = c(code_urban_concentration = 'code_muni')
  ]


    # * * estimate difference bua_areas -----------------------------------------
  # create columns with difference
  df_bua_areas[
    ,
    `:=`(
      diff_area_10 = as.double((abs(bua_area_ibge - bua_area_10)) / (bua_area_ibge)),
      diff_area_25 = as.double((abs(bua_area_ibge - bua_area_25)) / (bua_area_ibge))
    )
  ]

  # column checking which cutoff minimizes the difference
  df_bua_areas[
    ,
    `:=`(
      min_diff = dplyr::case_when(
        diff_area_10 < diff_area_25 ~ "cutoff_10",
        T ~ 'cutoff_25'
        )
      )
  ]

  # check frequency (absolute and proportion)
  table(df_bua_areas$min_diff)
  prop.table(table(df_bua_areas$min_diff))


    # * * uca characteristics and cutoff ----------------------------------------
  # include additional information about uca to explore data

  # classify by region
  df_bua_areas[
    ,
    name_region := data.table::fcase(
      grepl("^1", code_urban_concentration), 'Norte',
      grepl("^2", code_urban_concentration), 'Nordeste',
      grepl("^3", code_urban_concentration), 'Sudeste',
      grepl("^4", code_urban_concentration), 'Sul',
      grepl("^5", code_urban_concentration), 'Centro Oeste'
    )
  ]

  df_bua_areas <- df_bua_areas %>%
    dplyr::relocate(name_region, .after = code_urban_concentration)

  # read uca pop data saved at 03_2
  uca_pop <- readr::read_rds("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/results/uca_pop_100000_built_up_area_population_results.rds") %>%
    data.table::setDT()


  df_bua_areas[
    uca_pop,
    `:=`(
      pop2015 = i.pop2015
    ),
    on = .(code_urban_concentration)
  ]


  # plot data

  # load Helvetica font
  windowsFonts('Helvetica' = windowsFont('Helvetica'))
  #
  df_bua_areas %>%
    dplyr::mutate(
      pop2015 = pop2015 / 100000,
    ) %>%
    ggplot() +
    geom_point(
      aes(
        x = diff_area_25, y = diff_area_10,
        fill = as.factor(name_region),
        size = pop2015,
        #shape = min_diff
        ),
      alpha = 0.75,
      shape = 21
      ) +
    geom_abline(intercept = 0, size = 0.75, colour = 'black', alpha = 0.5) +
    scale_size(range = c(1, 20)) +
    #viridis::scale_fill_viridis(discrete = T, option = 'magma') +
        labs(
      x = 'Difference area 25%', y = 'Difference area 10%', fill = 'Region',
      size = 'Population (100.000)'
    ) +
    aop_style() +
    scale_fill_aop(palette = 'blue_red') +
    theme(legend.position = 'right') +
    guides(
      fill = guide_legend(override.aes = list(size = 5))#,
      #size = guide_legend(,override.aes = list(size = 10))
      )

  # save plot
  ggplot2::ggsave(
    filename = paste0('//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/figures/', 'difference_cutoff_10_25_ibge', '.png'),
    dpi = 300, device = 'png'
  )

  # save df footprints areas
  saveRDS(
    df_bua_areas,
    "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/results/cutoff_criteria_built_area.rds"
    )


  # * check spatial interception different cutoffs-ibge ---------------------

  # drop unnecessary column
  bua_areas <- purrr::modify_depth(
    .x = bua_convert, .depth = 2,
    ~dplyr::select(., !dplyr::starts_with('cutoff'))
  )

  # transpose list
  bua_areas <- purrr::transpose(bua_areas)
  # bind dfs
  bua_areas <- purrr::map(bua_areas, dplyr::bind_rows)
  # add code_muni to dfs
  bua_areas <- purrr::map(
    bua_areas,
    ~dplyr::left_join(
      .,
      urban_shapes %>% dplyr::select(code_urban_concentration, name_uca_case),
      by = c("name_muni" = "name_uca_case")
      )
  )



  ibge_pol <- ibge %>%
    dplyr::filter(code_muni %in% bua_areas$cutoff_10$code_urban_concentration)
  ibge_pol <- sf::st_as_sf(ibge_pol)
  ##### COMBINE DISSOLVE OU OUTRA OPERACAO ESPACIAL PARA JUNTAR OS POLYGONS
  # DOS MESMOS UCAS


  teste <- ibge_pol %>%
    rmapshaper::ms_dissolve(sum_fields = 'bua_area_ibge')

  ibge_pol <- sf::st_as_sf(ibge)
  ibge_pol <-

}




# ibge footprint ----------------------------------------------------------

# read urban footprint ibge
ibge <- read_urban_area(year=2015, simplified = F)
ibge$area <- as.numeric(st_area(ibge))/1000000

area_urb_ibge <- setDT(ibge)[, .(area_km2 = sum(area)), by=code_muni]

# * 2.3 run function ------------------------------------------------------


