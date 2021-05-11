# description -------------------------------------------------------------

# this script
# 1. reads data from built up area cropped for selected urban areas (1975-2014)
# areas: for,bsb,bhz,ctb,poa,rio,sp,nat,man
#
# 2. tests different criteria for defining built-up area polygon (urban extent)
# ..to be compared between cities.
# 2.1 test 1km pixel. different cutoffs (>0%, 25%, 50%)

# setup -------------------------------------------------------------------

source("R/setup.R")

# directory ---------------------------------------------------------------

ghsl_built_dir <- "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/"


# read data ---------------------------------------------------------------

# vector with ucas and time period
# ucas: for,bsb,bhz,ctb,poa,rio,sp,nat,man
files_cities <- dir(
  "//storage6/usuarios/Proj_acess_oport/data/urbanformbr/ghsl/BUILT/UCA/",
  pattern = "(1975|2014).*(sao_paulo|fortaleza|brasilia|curitiba|belo_horizonte|rio_de_janeiro|bage|porto_alegre|vitoria_es)"
)

input <- files_cities



# NAO RODAR -> CRIAR FUNCAO CORRETA

funcao <- function(input) {

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

  str(bua_uca,max.level = 1)


  # * funcao raster polygons and classify -----------------------------------
  # ver funcoes p/ obter bh1975 abaixo
  # incluir group_by e summarise depois
  # deixar o st_transform para depois? o que fazer? ESCLARECER
  # fazer projecao antes ou depois de extrair o valor?
  #bua_raster <- bua_uca$bage_rs_1975

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

  ### PROJECTION: O QUE FAZER? fazer projecao antes ou depois de extrair o valor?

  ### CLASSIFICACAO: QUAL CRITERIO? qual criterio (em termos quant.) de area
  # construida para classifica-la como "centro urbano" (e com isso obter..
  # ..o poligono de area construida de cada ano)



}


# run function ------------------------------------------------------------

# set up parallel
future::plan(future::multicore)

# files vector
years <- c("1975", "1990", "2000", "2014")
files <- purrr::map(years, ~ dir(ghsl_built_dir, pattern = .))

input <- files[[1]]

# run for multiple years
extrair <- furrr::future_map(files, ~ funcao(.))
