# description -------------------------------------------------------------

# this script extracts variables and estimates metrics from the demographic census
#..2010 to be used at the pca and regression analysis.

# all work related variables contain individuals which:
## age >= 16 (V6036>=16)
## live in urban areas (V1006 == 1)
## ocupation situation == ocuppied (V6920==1), except commute time


# setup -------------------------------------------------------------------

source('R/setup.R')

# define function ----------------------------------------------------------------
# create function case with factor
fct_case_when <- function(...){
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]]) # extract RHS formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels = levels)
}


f_censo <- function(){

  # read data ---------------------------------------------------------------

  # * censo domicilios ------------------------------------------------------
  df_censo_dom <- readr::read_rds('../../data/urbanformbr/censo/censo_dom.rds')

  # * censo individuos ------------------------------------------------------
  df_censo_pes <- readr::read_rds('../../data/urbanformbr/censo/censo_pes.rds')

  # * pca regression df -----------------------------------------------------

  # read df with code and names of urban concentration, and codes of each muni
  #..belonging to them
  df_codes <- readr::read_rds("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/pca_regression_df/pca_regression_df.rds")

  # count the number of munis in each urban concentration
  df_codes <- df_codes %>%
    dplyr::mutate(
      isolated_muni = data.table::fcase(
        purrr::map_int(code_muni_uca, length) == 1L, 1L,
        default = 0L
      )
    )

  # format df_codes so that it can be used at a left join (two columns):
  # i. urban concentration code (whole uca)
  # ii. municipality code (each muni within uca in a row with their own code)
  df_codes <- df_codes %>%
    tidyr::unnest_longer(code_muni_uca) %>%
    data.table::setDT()

  df_codes <- df_codes %>%
    dplyr::mutate(
      nucleo = data.table::fcase(
        code_muni_uca == code_urban_concentration, 1L,
        code_muni_uca != code_urban_concentration, 0L,
        default = 100L
      )
    )

  # * * read classify uca pop df ------------------------------------------------
  df_classify_uca <- readr::read_rds("../../data/urbanformbr/pca_regression_df/classify_uca_large_pop.rds") %>%
    dplyr::select(-pop_uca_2010)


  # * * merge codes urban_shapes --------------------------------------------
  df_codes <- dplyr::left_join(
    df_codes,
    df_classify_uca,
    by = c("code_urban_concentration" = "code_urban_concentration")
    )



  # clean data --------------------------------------------------------------


  # * add urban concentration area code -------------------------------------

  # fix muni code
  df_censo_dom[, code_muni := (V0001*100000) + V0002]

  df_censo_pes[, code_muni := (V0001*100000) + V0002]

  # add urban concentration area code to each household and individual based on..
  #.. code municipality
  df_censo_dom[
    df_codes,
    `:=`(
      code_urban_concentration = i.code_urban_concentration,
      name_uca_case = i.name_uca_case
    ),
    on = c("code_muni" = "code_muni_uca")
  ]

  df_censo_pes[
    df_codes,
    `:=`(
      code_urban_concentration = i.code_urban_concentration,
      name_uca_case = i.name_uca_case,
      isolated_muni = i.isolated_muni,
      nucleo = i.nucleo,
      large_uca_pop = i.large_uca_pop
    ),
    on = c("code_muni" = "code_muni_uca")
  ]

  # * filter data -----------------------------------------------------------
  # filter households/individuals from uca belonging to df_codes

  # filter only dom/pes from cities that belong to uca present at df_codes
  data.table::setkey(df_censo_dom, code_urban_concentration)
  df_censo_dom <- df_censo_dom[.(unique(df_codes$code_urban_concentration))]
  # or filter by na at code_urban_concentration
  #df_censo_dom <- df_censo_dom[!is.na(code_urban_concentration)]

  data.table::setkey(df_censo_pes, code_urban_concentration)
  df_censo_pes <- df_censo_pes[.(unique(df_codes$code_urban_concentration))]


  # filter only dom/pes from urban areas (V1006 == 1)
  #df_censo_dom <- subset(df_censo_dom, V1006 == 1) OU
  #data.table::setkey(df_censo_dom, V1006)
  #df_censo_dom <- df_censo_dom[.(1)]

  #df_censo_pes <- subset(df_censo_pes, V1006 == 1)


  # * create vars. dom ------------------------------------------------------

  # create var automobile &/or motorcycle in the household
  df_censo_dom[
    ,
    `:=`(
      car_motorcycle_sep = data.table::fcase(
        V0221 == 1L & V0222 == 1L, "Carro e motocicleta",

        V0222 == 1L & ( V0221 == 2L | is.na(V0221) ), "Apenas carro",
        V0221 == 1L & ( V0222 == 2L | is.na(V0222) ), "Apenas motocicleta",

        (V0221 == 2L & ( V0222 == 2L | is.na(V0222)) ) |
          (V0222 == 2L & ( V0221 == 2L | is.na(V0221)) ),
        "Nem carro nem motocicleta",

        is.na(V0221) & is.na(V0222), NA_character_,

        default = 'Erro'
      ),
      car_motorcycle = data.table::fcase(
        V0221 == 1 | V0222 == 1, "Carro ou motocicleta",
        (V0221 == 2 & V0222 == 2) | (V0221 == 2) | (V0222 == 2)
        , "Nem carro nem motocicleta",

        default = NA_character_
      )
    )
  ]


  # * create vars. pes ------------------------------------------------------


  #classify uca
  # from IBGE (2016) - Arranjos Populacionais e Concentracoes Urbanas:
  # uca (with more than 100000 pop) can be classified as
  # medium uca: ucas or isolated (only one muni) muni with 100000 <= pop <= 750000
  # large: ucas or isolated (only one muni) muni with pop > 750000

  # pop size by microdata
  #df_size <- df_censo_pes[
  #  ,
  #  .(pop_sum = sum(V0010,na.rm = T)),
  #  by = .(code_urban_concentration)
  #  ]


  df_censo_pes[
    ,
    `:=`(
      commute_time = data.table::fcase(
        V0662 == 1L, 5,
        V0662 == 2L, 15,
        V0662 == 3L, 45,
        V0662 == 4L, 90,
        V0662 == 5L, 120,
        default = NA_real_
      ),
      #sexo_raca = data.table::fcase(
      #  V0601 == 1L & V0606 == 1L, "Homem branco",
      #  V0601 == 1L & (V0606 == 2L | V0606 == 4L), "Homem preto ou pardo",
      #  V0601 == 1L & V0606 == 3L, "Homem amarelo",
      #  V0601 == 1L & V0606 == 5L, "Homem indígena",
      #  V0601 == 1L & V0606 == 9L, "Homem não-especificado",
      #  V0601 == 2L & V0606 == 1L, "Mulher branca",
      #  V0601 == 2L & (V0606 == 2L | V0606 == 4L), "Mulher preta ou parda",
      #  V0601 == 2L & V0606 == 3L, "Mulher amarela",
      #  V0601 == 2L & V0606 == 5L, "Mulher indígena",
      #  V0601 == 2L & V0606 == 9L, "Mulher não-especificada",
      #  default = NA_character_
      #),
      raca = data.table::fcase(
        V0606 == 1L, "Branca",
        V0606 == 2L | V0606 == 4L, "Preta ou Parda",
        V0606 == 3L, "Amarela",
        V0606 == 5L, "Indígena",
        default = NA_character_
      ),
      # V6400 education
      education = fct_case_when(
        # individuals with low education levels (from no instruction to ensino medio)
        V6400 <= 2L ~ "Baixa escolaridade",
        V6400 == 3L ~ "Média escolaridade",
        # individuals with high education level (superior completo)
        V6400 == 4L ~ "Alta escolaridade",
        T ~ NA_character_
      ),
      # V6471 categories for activities (CNAE) : industry, services/comerce, agro
      sector = data.table::fcase(
        V6471 > 0L & V6471 <= 03999L          # AGRICULTURA, PECUÁRIA, PRODUÇÃO FLORESTAL, PESCA E AQUICULTURA
        , "Agricultura",
        V6471 >= 05000L & V6471 <= 09999L |   # INDÚSTRIAS EXTRATIVAS
          V6471 >= 10000L & V6471 <= 33999L | # INDÚSTRIAS DE TRANSFORMAÇÃO
          V6471 >= 41000L & V6471 <= 43999L   # CONSTRUÇÃO
        , "Indústria",
        V6471 >= 35000L & V6471 <= 39999L |   # ELETRICIDADE E GÁS
          V6471 >= 45000L & V6471 <= 48999L | # COMÉRCIO;REPARAÇÃO DE VEÍCULOS AUTOMOTORES E MOTOCICLETAS
                                            # COMÉRCIO, EXCETO DE VEICULOS AUTOMOTORES E MOTOCICLETAS
          V6471 >= 49000L & V6471 <= 53999L | # TRANSPORTE, ARMAZENAGEM E CORREIO
          V6471 >= 55000L & V6471 <= 56999L | # ALOJAMENTO E ALIMENTAÇÃO
          V6471 >= 58000L & V6471 <= 75999L | # INFORMAÇÃO E COMUNICAÇÃO|
                                            # ATIVIDADES IMOBILIÁRIAS |
                                            # ATIVIDADES PROFISSIONAIS, CIENTÍFICAS E TÉCNICAS
          V6471 >= 77000L & V6471 <= 88999L | # ATIVIDADES ADMINISTRATIVAS E SERVIÇOS COMPLEMENTARES
                                            # ADMINISTRAÇÃO PÚBLICA, DEFESA E SEGURIDADE SOCIAL
                                            # SAÚDE HUMANA E SERVIÇOS SOCIAIS
          V6471 >= 90000L & V6471 <= 94999L | # ARTES, CULTURA, ESPORTE E RECREAÇÃO
          V6471 >= 95000L & V6471 <= 99999L   # OUTRAS ATIVIDADES DE SERVIÇOS
                                            # SERVIÇOS DOMÉSTICOS
                                            # ORGANISMOS INTERNACIONAIS E OUTRAS INSTITUIÇÕES EXTRATERRITORIAIS
        , "Serviços",
        default = NA_character_
      ),
      # V6036 idade trabalhadores -> criar categorias
      age = data.table::fcase(
        V6036 <= 15L, "Até 15 anos",
        V6036 >= 16L & V6036 <= 39L, "16-39 anos",
        V6036 >= 40L & V6036 <= 64L, "40-64 anos",
        V6036 >= 65L, "65+ anos",
        default = NA_character_
      ),
      razao_dep = data.table::fcase(
        V6036 <= 14L, "Até 14 anos",
        V6036 >= 15L & V6036 <= 64L, "15-64 anos",
        V6036 >= 65L, "65+ anos",
        default = NA_character_
      ),
      # V0660 which municipality the worker works
      work_muni = data.table::fcase(
        V0660 == 1L, "Próprio domicílio",
        V0660 == 2L, "Mesmo município, mas não no domicílio",
        V0660 >= 3L & V0660 <= 5L, "Outro ou mais municípios/país",
        default = NA_character_
      )
    )
  ]

  ## check proportion economic activity -> fazer com code_urban_concentration sem filtro
  ## check OK
  ## https://www.ibge.gov.br/apps/snig/v1/index.html?loc=0,0U,0R&cat=-1,-2,-27,112,113,114,128&ind=4741
  #df_censo_pes[
  #  !is.na(sector),
  #  .(
  #    prop_industria = sum(V0010[which(sector == "Indústria" & V6920==1 & V6036>=16)], na.rm = T) / sum(V0010[which(V6920==1 & V6036>=16)], na.rm = T),
  #    prop_serv = sum(V0010[which(sector == "Serviços" & V6920==1 & V6036>=16)], na.rm = T) / sum(V0010[which(V6920==1 & V6036>=16)], na.rm = T),
  #    prop_agri = sum(V0010[which(sector == "Agricultura" & V6920==1 & V6036>=16)], na.rm = T) / sum(V0010[which(V6920==1 & V6036>=16)], na.rm = T)
  #  )#,
  #by = .(code_urban_concentration)
  #]

  ## create necessary variables for generating informal/formal job
  df_censo_pes[
    ,
    `:=`(
      # grupos ocupacionais CBO 2000
      grupocup = data.table::fcase(
        (V6462>=1111L & V6462<=1140L) |
          (V6462>=1210L & V6462<=1230L) |
          V6462==1310L |
          V6462==1320L,
        1L, # Membros superiores do Poder Publico, dirigentes de organizacoes de
        # interesse publico e de empresas, gerentes

        (V6462>=2011L & V6462<=2021L) |
          (V6462>=2111L & V6462<=2153L) |
          (V6462>=2211L & V6462<=2237L) |
          (V6462>=2311L & V6462<=2394L) |
          (V6462>=2410L & V6462<=2423L) |
          (V6462>=2511L & V6462<=2531L) |
          (V6462>=2611L & V6462<=2631L),
        2L, #Profissionais das Ciencias e das Artes

        (V6462>=3001L & V6462<=3012L) |
          (V6462>=3111L & V6462<=3192L) |
          (V6462>=3201L & V6462<=3281L) |
          (V6462>=3311L & V6462<=3341L) |
          (V6462>=3411L & V6462<=3426L) |
          (V6462>=3511L & V6462<=3548L) |
          (V6462>=3711L & V6462<=3773L) |
          V6462==3911L |
          V6462==3912L,
        3L, #Tecnicos de nivel medio

        (V6462>=4101L & V6462<=4152L) |
          (V6462>=4201L & V6462<=4241L),
        4L, #Trabalhadores de servicos administrativos

        (V6462>=5101L & V6462<=5199L) |
          (V6462>=5201L & V6462<=5243L),
        5L, #Trabalhadores dos servicos, vendedores do comercio em lojas e mercados

        (V6462>=6110L & V6462<=6139L) |
          (V6462>=6201L & V6462<=6239L) |
          (V6462>=6301L & V6462<=6329L) |
          (V6462>=6410L & V6462<=6430L),
        6L, #Trabalhadores agropecuarios, florestais, da caca e da pesca

        (V6462>=7101L & V6462<=7170L) |
          (V6462>=7201L & V6462<=7257L) |
          (V6462>=7301L & V6462<=7321L) |
          (V6462>=7401L & V6462<=7421L) |
          (V6462>=7501L & V6462<=7524L) |
          (V6462>=7601L & V6462<=7687L) |
          (V6462>=7701L & V6462<=7772L) |
          (V6462>=7801L & V6462<=7842L) |
          (V6462>=8101L & V6462<=8181L) |
          (V6462>=8201L & V6462<=8281L) |
          (V6462>=8301L & V6462<=8339L) |
          (V6462>=8401L & V6462<=8493L) |
          (V6462>=8601L & V6462<=8625L) |
          V6462==8711L,
        7L, #Trabalhadores da producao de bens e servicos industriais

        (V6462>=9101L & V6462<=9193L) |
          (V6462>=9501L & V6462<=9543L) |
          (V6462>=9911L & V6462<=9922L),
        8L, #Trabalhadores de reparacao e manutencao

        V6462==0L |
          V6462==0100L |
          V6462==0200L |
          V6462==0300L |
          (V6462>=0401L & V6462<=0413L) |
          (V6462>=0501L & V6462<=0513L),
        9L, #Membros das forcas armadas, policiais e bombeiros militares

        default = NA_integer_
      )
    )
  ]

  # gerar conta-propria excluindo trab liberais (grupocup = 1 ou 2)
  df_censo_pes[
    ,
    `:=`(
      V6930_conta_propria = data.table::fcase(
        V6930 == 1L, "Empregados com carteira de trabalho assinada",
        V6930 == 2L, "Militares e funcionários públicos estatutários",
        V6930 == 3L, "Empregados sem carteira de trabalho assinada",
        (V6930 == 4L & grupocup > 2L), "Conta própria sem profissão liberal - informal",
        (V6930 == 4L & grupocup <= 2L), "Conta própria com profissão liberal - formal",
        V6930 == 5L, "Empregadores",
        V6930 == 6L, "Não remunerados",
        V6930 == 7L, "Trabalhadores na produção para o próprio consumo",
        default = NA_character_
      )
    )
  ]


  # informalidade:
  # informal = 1 (sem-carteira (domestico e nao-domestico); conta-propria (exceto os liberais); nao-remunerados; consumo próprio)
  # informal = 0 (*formal* - com carteira (domestico e nao-domestico), func. publico, conta-propria libeiras e empregador)
  df_censo_pes[
    ,
    `:=`(
      informal = data.table::fcase(
        V6930_conta_propria == "Empregados sem carteira de trabalho assinada" |
          V6930_conta_propria == "Não remunerados" |
          V6930_conta_propria == "Trabalhadores na produção para o próprio consumo" |
          V6930_conta_propria == "Conta própria sem profissão liberal - informal",
        "Informal",

        V6930_conta_propria == "Empregados com carteira de trabalho assinada" |
          V6930_conta_propria == "Militares e funcionários públicos estatutários" |
          V6930_conta_propria == "Empregadores" |
          V6930_conta_propria == "Conta própria com profissão liberal - formal",
        "Formal",

        default = NA_character_
      )
    )
  ]


  # household size: number of individuals per household -> UTILIZAR V0401 !!
  #df_household_size <- df_censo_pes[
  #  ,
  #  .(household_size = .N),
  #  by = .(V0300)
  #]
  #df_censo_pes[
  #  df_household_size,
  #  `:=`(household_size = i.household_size),
  #  on = c("V0300" = "V0300")
  #]
  #df_household_size <- df_censo_pes[
  #  V1006==1,
  #  .(wghtd_mean_density_resident_household = weighted.mean(household_size, w = V0010, na.rm = T)),
  #  by = .(code_urban_concentration)
  #]

  # merge dom + pes data ----------------------------------------------------

  # merge household and individual data
  df_censo_pes[
    df_censo_dom,
    `:=`(
      car_motorcycle_sep = i.car_motorcycle_sep,
      car_motorcycle = i.car_motorcycle,
      V0221 = i.V0221,
      V0222 = i.V0222
    ),
    on = c("V0300" = "V0300")
  ]


  # estimate variables ------------------------------------------------------
  # variaveis estimadas (por uca)

  # * vars domicilios (households) -----------------------------------------------
  # V1006: % domicilios em situacao urbana
  # estimar proporcao 1 at V1006

  #### obs: a partir da var de cima, todas as estimacoes devem ter filtro V1006==1
  # car_motorcycle: % domicilios com car/bike
  # var criada; estimar proporcao
  # V0203: numero medio de comodos por domicilio
  # estimar media via V0203
  # V6203: media de densidade morador/comodo
  # estimar media via V6203
  # V6204: media de densidade morador/dormitorio
  # estimar media via V6204

  df_wghtd_mean_dom <- df_censo_dom[
    V1006 == 1L, # filter only individuals from urban areas
    lapply(.SD, weighted.mean, w = V0010, na.rm = T),
    by = .(code_urban_concentration, name_uca_case),
    .SDcols = c("V0203","V6203","V6204","V0401")
  ]

  data.table::setnames(
    x = df_wghtd_mean_dom,
    old = c("V0203","V6203","V6204","V0401"),
    new = c("wghtd_mean_density_rooms_household","wghtd_mean_density_resident_rooms",
            "wghtd_mean_density_resident_bedroom","wghtd_mean_density_resident_household")
  )

  df_prop_dom <- df_censo_dom[
    ,
    .(
      prop_dom_urban = sum(V0010[which(V1006 == 1L)], na.rm = T) / sum(V0010, na.rm = T)#,

      #prop_motos_dom = sum(V0010[which(V0221 == 1L & V1006 == 1L)], na.rm = T) / sum(V0010[which(V1006 == 1L)], na.rm = T),
      #prop_autos_dom = sum(V0010[which(V0222 == 1L & V1006 == 1L)], na.rm = T) / sum(V0010[which(V1006 == 1L)], na.rm = T),

      #prop_car_or_motorcycle_dom = sum(V0010[which(car_motorcycle == "Carro ou motocicleta" & V1006 == 1L)], na.rm = T) / sum(V0010[which(V1006 == 1L)], na.rm = T),
      #prop_car_dom = sum(V0010[which(car_motorcycle_sep == "Apenas carro" & V1006 == 1L)], na.rm = T) / sum(V0010[which(V1006 == 1L)], na.rm = T),
      #prop_motorcycle_dom = sum(V0010[which(car_motorcycle_sep == "Apenas motocicleta" & V1006 == 1L)], na.rm = T) / sum(V0010[which(V1006 == 1L)], na.rm = T),
      #prop_car_and_motorcycle_dom = sum(V0010[which(car_motorcycle_sep == "Carro e motocicleta" & V1006 == 1L)], na.rm = T) / sum(V0010[which(V1006 == 1L)], na.rm = T)
    ),
    by = .(code_urban_concentration)
  ]

  #df_prop_car_bike <- df_censo_dom[
  #  !is.na(car_motorcycle),
  #  .(n_car_motorcycle_join = .N),
  #  by = .(code_urban_concentration, car_motorcycle)
  #  ][
  #  ,
  #  prop_car_motorcycle_dom := prop.table(n_car_motorcycle_join),
  #  by = .(code_urban_concentration)
  #  ]

  df_vars_dom <- data.table::merge.data.table(
    x = df_wghtd_mean_dom %>%
      select(-c(wghtd_mean_density_resident_bedroom,wghtd_mean_density_resident_rooms)),
    y = df_prop_dom,
    by = "code_urban_concentration"
  )


  # * vars pessoas (individuals) --------------------------------------------
  ## prorportion
  # V1006: % pessoas em domicilios em situacao urbana
  # estimar proporcao 1 at V1006
  # V0601: % sexo masculino
  # estimar proporcao 1 at V0601
  # V0601: % sexo feminino
  # estimar proporcao 2 at V0601
  # raca: % pessoas brancas (ou Preta ou Parda)
  # var criada; estimar prop
  # education: % baixa escolaridade
  # var criada; esitmar prop education->baixa
  # education: % alta escolaridade
  # var criada; esitmar prop education->alta
  # age: % 15<
  # var criada; esitmar prop age
  # age: % 18-39
  # var criada; esitmar prop age
  # age: % 40-64
  # var criada; esitmar prop age
  # age: % 65+
  # var criada; esitmar prop age
  # informal: % trabalhadores (in)formais
  # var criada; esitmar prop formal
  # work_muni: % trab. que trabalham em outro municipio
  # var criada; esitmar prop "outro muni"
  # V6920: trabalhadores empregados
  # estimar proporcao 1 at V6920
  # sector: % trab. industria
  # var criada; estimar prop industria (secundario)
  # sector: % trab. servicos
  # var criada: estimar prop servicos (terciario)
  # car_motorcycle: % prop individuos moram dom. com car/bike
  # var criada: estimar prop "Carro ou motocicleta"

  ## weighted mean
  # commute_time (apenas V0661 == 1): tempo deslocamento casa-trabalho
  # var criada: estimar tempo medio viagem (ver R/commute_time_censo2010)

  df_wghtd_mean_pes <- dplyr::left_join(
    df_censo_pes[
      # V0661==1 (commute daily), more than 16 years of age, V1006==1 (urban)
      V0661 == 1L & age != "Até 15 anos" & V1006 == 1L,
      .(wghtd_mean_commute_time = weighted.mean(commute_time, w = V0010, na.rm = T)),
      by = .(code_urban_concentration)
    ],
    df_censo_pes[
      V1006 == 1L, # filter only individuals from urban areas
      .(wghtd_mean_household_income_per_capita = weighted.mean(V6531, w = V0010, na.rm = T)),
      by = .(code_urban_concentration)
    ]
  )

  #df_prop_pes_urban <- df_censo_pes[
  #  ,
  #  .(prop_pes_urban = sum(V0010[which(V1006 == 1)], na.rm = T) / sum(V0010, na.rm = T)),
  #  by = .(code_urban_concentration)
  #]

  df_prop_pes <- df_censo_pes[
    V1006 == 1L, # filter only individuals from urban areas
    .(
      #prop_men = sum(V0010[which(V0601 == 1L)], na.rm = T) / sum(V0010, na.rm = T),
      #prop_women = sum(V0010[which(V0601 == 2L)], na.rm = T) / sum(V0010, na.rm = T),
      #prop_white = sum(V0010[which(raca == "Branca")], na.rm = T) / sum(V0010, na.rm = T),
      prop_black = sum(V0010[which(raca == "Preta ou Parda")], na.rm = T) / sum(V0010, na.rm = T),
      #prop_white_men = sum(V0010[which(sexo_raca == "Homem branco")], na.rm = T) / sum(V0010, na.rm = T),
      #prop_black_men = sum(V0010[which(sexo_raca == "Homem preto ou pardo")], na.rm = T) / sum(V0010, na.rm = T),
      #prop_yellow_men = sum(V0010[which(sexo_raca == "Homem amarelo")], na.rm = T) / sum(V0010, na.rm = T),
      #prop_indigenous_men = sum(V0010[which(sexo_raca == "Homem indígena")], na.rm = T) / sum(V0010, na.rm = T),
      #prop_white_women = sum(V0010[which(sexo_raca == "Mulher branca")], na.rm = T) / sum(V0010, na.rm = T),
      #prop_black_women = sum(V0010[which(sexo_raca == "Mulher preta ou parda")], na.rm = T) / sum(V0010, na.rm = T),
      #prop_yellow_women = sum(V0010[which(sexo_raca == "Mulher amarela")], na.rm = T) / sum(V0010, na.rm = T),
      #prop_indigenous_women = sum(V0010[which(sexo_raca == "Mulher indígena")], na.rm = T) / sum(V0010, na.rm = T),

      #prop_low_educ = sum(V0010[which(education =="Baixa escolaridade")],na.rm = T) / sum(V0010, na.rm=T),
      prop_high_educ = sum(V0010[which(education == "Alta escolaridade")],na.rm = T) / sum(V0010, na.rm=T),
      #prop_age_15_less = sum(V0010[which(age == "Até 15 anos")],na.rm = T) / sum(V0010, na.rm=T),
      #prop_age_16_39 = sum(V0010[which(age == "16-39 anos")],na.rm = T) / sum(V0010, na.rm=T),
      #prop_age_40_64 = sum(V0010[which(age == "40-64 anos")],na.rm = T) / sum(V0010, na.rm=T),
      #prop_age_65_more = sum(V0010[which(age == "65+ anos")],na.rm = T) / sum(V0010, na.rm=T),
      prop_razao_dep = sum(V0010[which(razao_dep == "Até 14 anos" | razao_dep == "65+ anos")],na.rm = T) / sum(V0010[which(razao_dep == "15-64 anos")], na.rm=T),

      # motos e autos
      prop_motos_pes = sum(V0010[which(V0221 == 1L)],na.rm = T) / sum(V0010, na.rm=T),
      prop_autos_pes = sum(V0010[which(V0222 == 1L)],na.rm = T) / sum(V0010, na.rm=T),

      #prop_car_or_motorcycle_pes = sum(V0010[which(car_motorcycle == "Carro ou motocicleta")],na.rm = T) / sum(V0010, na.rm=T),
      #prop_car_pes = sum(V0010[which(car_motorcycle_sep == "Apenas carro")], na.rm = T) / sum(V0010, na.rm = T),
      #prop_motorcycle_pes = sum(V0010[which(car_motorcycle_sep == "Apenas motocicleta")], na.rm = T) / sum(V0010, na.rm = T),
      #prop_car_and_motorcycle_pes = sum(V0010[which(car_motorcycle_sep == "Carro e motocicleta")], na.rm = T) / sum(V0010, na.rm = T),

      # all work related variables filter age != "Até 15 anos" & V6920 == 1 (situacao ocupacao == ocupada)
      prop_employed = sum(V0010[which(age != "Até 15 anos" & V6920 == 1L)],na.rm = T) / sum(V0010[which(age != "Até 15 anos")], na.rm=T),
      prop_formal = sum(V0010[which(informal == "Formal" & age != "Até 15 anos" & V6920 == 1L)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L)], na.rm=T),
      #prop_informal = sum(V0010[which(informal == "Informal" & age != "Até 15 anos" & V6920 == 1)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1)], na.rm=T),
      #prop_work_other_muni = sum(V0010[which(work_muni == "Outro ou mais municípios/país" & age != "Até 15 anos" & V6920 == 1L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L)], na.rm = T),
      #prop_work_home_office = sum(V0010[which(work_muni == "Próprio domicílio" & age != "Até 15 anos" & V6920 == 1L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L)], na.rm = T),
      #prop_work_same_muni_not_home_office = sum(V0010[which(work_muni == "Mesmo município, mas não no domicílio" & age != "Até 15 anos" & V6920 == 1L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1)], na.rm = T),

      prop_work_other_muni_res_nucleo = sum(V0010[which(work_muni == "Outro ou mais municípios/país" & age != "Até 15 anos" & V6920 == 1L & nucleo == 1L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & nucleo == 1L)], na.rm = T),
      prop_work_other_muni_res_not_nucleo = sum(V0010[which(work_muni == "Outro ou mais municípios/país" & age != "Até 15 anos" & V6920 == 1L & nucleo == 0L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & nucleo == 0L)], na.rm = T),

      prop_work_home_office_res_nucleo = sum(V0010[which(work_muni == "Próprio domicílio" & age != "Até 15 anos" & V6920 == 1L & nucleo == 1L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & nucleo == 1L)], na.rm = T),
      prop_work_home_office_res_not_nucleo = sum(V0010[which(work_muni == "Próprio domicílio" & age != "Até 15 anos" & V6920 == 1L & nucleo == 0L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & nucleo == 0L)], na.rm = T),

      #prop_work_same_muni_not_home_office_nucleo = sum(V0010[which(work_muni == "Mesmo município, mas não no domicílio" & age != "Até 15 anos" & V6920 == 1L & nucleo == 1L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1 & nucleo == 1L)], na.rm = T),
      #prop_work_same_muni_not_home_office_not_nucleo = sum(V0010[which(work_muni == "Mesmo município, mas não no domicílio" & age != "Até 15 anos" & V6920 == 1L & nucleo == 0L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1 & nucleo == 0L)], na.rm = T),


      prop_industry = sum(V0010[which(sector == "Indústria" & age != "Até 15 anos" & V6920 == 1L)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L)], na.rm=T),
      prop_services = sum(V0010[which(sector == "Serviços" & age != "Até 15 anos" & V6920 == 1L)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L)], na.rm=T)
    ),
    by = .(code_urban_concentration)
  ]

  # replace nan from not_nucleo
  df_prop_pes <- df_prop_pes %>%
    mutate_all(~replace(.,is.nan(.),0))

  #REMOVER -> GERAR INTERACAO SOMENTE AO RODAR O MODELO
  #df_vars_pes_interaction <- df_censo_pes[
  #  V1006 == 1L, # filter only individuals from urban areas,
  #  .(
      # interaction large uca pop & sector
  #    prop_industry_large = sum(V0010[which(sector == "Indústria" & age != "Até 15 anos" & V6920 == 1L & large_uca_pop == 1L)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & large_uca_pop == 1L)], na.rm=T),
  #    prop_industry_medium = sum(V0010[which(sector == "Indústria" & age != "Até 15 anos" & V6920 == 1L & large_uca_pop == 0L)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & large_uca_pop == 0L)], na.rm=T),

  #    prop_services_large = sum(V0010[which(sector == "Serviços" & age != "Até 15 anos" & V6920 == 1L & large_uca_pop == 1L)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & large_uca_pop == 1L)], na.rm=T),
  #    prop_services_medium = sum(V0010[which(sector == "Serviços" & age != "Até 15 anos" & V6920 == 1L & large_uca_pop == 0L)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & large_uca_pop == 0L)], na.rm=T),

      # interaction nucleo/isolated & work place
  #    prop_work_other_muni_isolated = sum(V0010[which(work_muni == "Outro ou mais municípios/país" & age != "Até 15 anos" & V6920 == 1L & isolated_muni == 1L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & isolated_muni == 1L)], na.rm = T),
  #    prop_work_home_office_isolated = sum(V0010[which(work_muni == "Próprio domicílio" & age != "Até 15 anos" & V6920 == 1L & isolated_muni == 1L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & isolated_muni == 1L)], na.rm = T),
  #    prop_work_same_muni_not_home_office_isolated = sum(V0010[which(work_muni == "Mesmo município, mas não no domicílio" & age != "Até 15 anos" & V6920 == 1L & isolated_muni == 1L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & isolated_muni == 1L)], na.rm = T),

  #    prop_work_other_muni_not_isolated = sum(V0010[which(work_muni == "Outro ou mais municípios/país" & age != "Até 15 anos" & V6920 == 1L & isolated_muni == 0L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & isolated_muni == 0L)], na.rm = T),
  #    prop_work_home_office_not_isolated = sum(V0010[which(work_muni == "Próprio domicílio" & age != "Até 15 anos" & V6920 == 1L & isolated_muni == 0L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & isolated_muni == 0L)], na.rm = T),
  #    prop_work_same_muni_not_home_office_not_isolated = sum(V0010[which(work_muni == "Mesmo município, mas não no domicílio" & age != "Até 15 anos" & V6920 == 1L & isolated_muni == 0L)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1L & isolated_muni == 0L)], na.rm = T)

  #  ),
  #  by = .(code_urban_concentration)
  #]

  # REMOVE NAN FROM INTERACTION VARIABLES -> DEVO REMOVER?
  #df_vars_pes_interaction <- df_vars_pes_interaction %>%
  #  mutate_all(~replace(.,is.nan(.),0))


  #df_vars_pes <- data.table::merge.data.table(x = df_wghtd_mean_pes
  #                                            ,y = df_prop_pes_urban
  #                                            ,by = "code_urban_concentration")
  df_vars_pes <- data.table::merge.data.table(
    x = df_wghtd_mean_pes
    ,y = df_prop_pes
    ,by = "code_urban_concentration"
    )

  #df_vars_pes <- data.table::merge.data.table(
  #  x = df_vars_pes
  #  ,y = df_vars_pes_interaction
  #  ,by = "code_urban_concentration"
   # )


  # * merge dom pes vars ----------------------------------------------------
  df_vars_total <- data.table::merge.data.table(
    x = df_vars_dom,
    y = df_vars_pes,
    by = "code_urban_concentration"
  )


  # save data ---------------------------------------------------------------

  saveRDS(
    object = df_vars_total,
    file = '../../data/urbanformbr/pca_regression_df/censo.rds',
    compress = 'xz'
  )


}

# run function ------------------------------------------------------------
f_censo()

# plot data ---------------------------------------------------------------
#ggplot(df_vars_total) +
#  geom_point(aes(wghtd_mean_commute_time,prop_work_other_muni)) +
#  scale_x_log10() +
#  scale_y_log10()

# vars description --------------------------------------------------------

# * censo domicilios ------------------------------------------------------

cols_to_read_dom <- c(
  'V0001', # UF
  'V0002', # codigo municipio
  #'V0011', # area de ponderacao
  'V0010', # peso amostral
  "V0300", # controle
  "V1006", # situacao do domicilio (1 urbana 2 rural)
  'V0221', # existencia de motocicleta para uso particular (1 sim, 2 nao)
  'V0222', # existencia de automovel para uso particular (1 sim, 2 nao)
  "V0203", # numero de comodos -> criar numero medio de comodos por domicilio
  "V6203", # densidade morador/cômodo
  "V6204", # densidade morador/dormitorio
  "V0401"  # quantas pessoas moravam no domicilio 31/07/10
)


# * censo pessoas ---------------------------------------------------------

cols_to_read_pes <-c(
  'V0001', # UF
  'V0002', # codigo municipio
  'V0011', # area ponderacao
  'V0010', # peso amostral
  "V0300", # controle
  "V1006", # situacao do domicilio (1 urbana ou 2 rural)
  "V0601", # sexo
  #"V0633", # curso mais elevado que frequentou
  "V6400", # nivel de instrucao
  "V6036", # idade calculada em anos
  "V6930", # posicao na ocupacao
  "V0648", # nesse trabalho era
  "V6471", # atividade CNAE
  "V6462", # ocupacao CBO
  #"V6910", # condicao na ocupacao (2- desocupadas)
  "V6920", # situacao na ocupacao (2- nao ocupadas) -> USAR ESSE
  "V0661", # retorna do trabalho para casa diariamente (1 sim, 2 nao)
  "V0662", # tempo deslocamento casa-trabalho
  "V0606", # raca
  "V0660", # em que municipio e UF trabalha
  "V6531"  # rendimento domiciliar (domicilio particular) per capita julho 2010
)



