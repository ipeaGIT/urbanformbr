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

  cols_to_read_dom <- c(
    'V0001', # UF
    'V0002', # codigo municipio
    #'V0011', # area de ponderacao
    'V0010', # peso amostral
    "V0300", # controle
    "V1006", # situacao do domicilio (1 urbana 2 rural)
    'V0221', # existencia de motocicleta para uso particular
    'V0222', # existencia de automovel para uso particular
    "V0203", # numero de comodos -> criar numero medio de comodos por domicilio
    "V6203", # densidade morador/cômodo
    "V6204", # densidade morador/dormitorio
    "V0401"  # quantas pessoas moravam no domicilio 31/07/10
    )

  df_censo_dom <- data.table::fread(
    file = '//storage6/bases2/NINSOC/Bases/Censo_Demografico/2010/CSV/censo_2010.domicilios.csv.bz2',
    #nrows = 5,
    select = cols_to_read_dom#,
    #colClasses = 'character'
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
    "V0661", # retorna do trabalho para casa diariamente
    "V0662", # tempo deslocamento casa-trabalho
    "V0606", # raca
    "V0660", # em que municipio e UF trabalha
    "V6531"  # rendimento domiciliar (domicilio particular) per capita julho 2010
    )

  df_censo_pes <- data.table::fread(
    file = '//storage6/bases2/NINSOC/Bases/Censo_Demografico/2010/CSV/censo_2010.pessoas.csv.bz2',
    #nrows = 5,
    select = cols_to_read_pes#,
    #colClasses = 'character'
  )

  # check total population
  #sum(df_censo_pes$V0010, na.rm=T)

  # * pca regression df -----------------------------------------------------
  # read df with code and names of urban concentration, and codes of each muni
  #..belonging to them
  df_codes <- readr::read_rds("//storage6/usuarios/Proj_acess_oport/data/urbanformbr/pca_regression_df/pca_regression_df.rds")

  # format df_codes so that it can be used at a left join (two columns):
  # i. urban concentration code (whole uca)
  # ii. municipality code (each muni within uca in a row with their own code)
  df_codes <- df_codes %>%
    tidyr::unnest_longer(code_muni_uca) %>%
    data.table::setDT()

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
      name_uca_case = i.name_uca_case
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
      #car_motorcycle_sep = data.table::fcase(
      #  is.na(V0221) & is.na(V0222), NA_character_,
      #  V0221 == 2 & V0222 == 2, "Nem carro nem motocicleta",
      #  V0221 == 1 & V0222 == 1, "Carro e motocicleta",
      #  V0222 == 1 & V0221 == 2 | is.na(V0221), "Apenas carro",
      #  V0221 == 1 & V0222 == 2 | is.na(V0222), "Apenas motocicleta",
      #  default = 'Erro'
      #),
      car_motorcycle = data.table::fcase(
        V0221 == 2 & V0222 == 2, "Nem carro nem motocicleta",
        V0221 == 1 | V0222 == 1, "Carro ou motocicleta",
        default = NA_character_
      )
    )
  ]


  # * create vars. pes ------------------------------------------------------

  df_censo_pes[
    ,
    `:=`(
      commute_time = data.table::fcase(
        V0662 == 1, 5,
        V0662 == 2, 15,
        V0662 == 3, 45,
        V0662 == 4, 90,
        V0662 == 5, 120,
        default = NA_real_
        ),
      sexo_raca = data.table::fcase(
        V0601 == 1 & V0606 == 1, "Homem branco",
        V0601 == 1 & V0606 == 2 | V0606 == 4, "Homem preto ou pardo",
        V0601 == 1 & V0606 == 3, "Homem amarelo",
        V0601 == 1 & V0606 == 5, "Homem indígena",
        V0601 == 2 & V0606 == 1, "Mulher branca",
        V0601 == 2 & V0606 == 2 | V0606 == 4, "Mulher preta ou parda",
        V0601 == 2 & V0606 == 3, "Mulher amarela",
        V0601 == 2 & V0606 == 5, "Mulher indígena",
        default = NA_character_
      ),
      raca = data.table::fcase(
        V0606 == 1, "Branca",
        V0606 == 2 | V0606 == 4, "Preta ou Parda",
        V0606 == 3, "Amarela",
        V0606 == 5, "Indígena",
        default = NA_character_
      ),
      # V6400 education
      education = fct_case_when(
        # individuals with low education levels (from no instruction to ensino medio)
        V6400 <= 2 ~ "Baixa escolaridade",
        V6400 == 3 ~ "Média escolaridade",
        # individuals with high education level (superior completo)
        V6400 == 4 ~ "Alta escolaridade",
        T ~ NA_character_
      ),
      # V6471 categories for activities (CNAE) : industry, services/comerce, agro
      sector = data.table::fcase(
        V6471 > 0 & V6471 <= 03999 #| V6471 >= 10000 & v6471 <= 12999
        , "Agricultura",

        V6471 >= 05000 & V6471 <= 09999 |
          V6471 >= 10000 & V6471 <= 33999 | # conferir industria da transformacao
          V6471 >= 41000 & V6471 <= 43999
        , "Indústria",

        V6471 >= 35000 & V6471 <= 39999 |
          V6471 >= 45000 & V6471 <= 48999 |
          V6471 >= 49000 & V6471 <= 53999 |
          V6471 >= 55000 & V6471 <= 56999 |
          V6471 >= 58000 & V6471 <= 75999 |
          V6471 >= 77000 & V6471 <= 88999 |
          V6471 >= 90000 & V6471 <= 94999 |
          V6471 >= 95000 & V6471 <= 99999
        , "Serviços",
        default = NA_character_
      ),
      # V6036 idade trabalhadores -> criar categorias
      age = fct_case_when(
        V6036 <= 15 ~ "Até 15 anos",
        V6036 >= 16 & V6036 <= 39 ~ "16-39 anos",
        V6036 >= 40 & V6036 <= 64 ~ "40-64 anos",
        V6036 >= 65 ~ "65+ anos",
        T ~ NA_character_
      ),
      # V0660 which municipality the worker works
      work_muni = data.table::fcase(
        V0660 == 1, "Próprio domicílio",
        V0660 == 2, "Mesmo município, mas não no domicílio",
        V0660 >= 3 & V0660 <= 5, "Outro ou mais municípios/país",
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

  ## GERAR TRABALHO INFORMAL (CRIAR VARIAVEIS NECESSARIAS)
  df_censo_pes[
    ,
    `:=`(
      # grupos ocupacionais CBO 2000
      grupocup = data.table::fcase(
        (V6462>=1111 & V6462<=1140) |
          (V6462>=1210 & V6462<=1230) |
          V6462==1310 |
          V6462==1320,
        1L, #Membros superiores do Poder Publico, dirigentes de organizacoes de interesse publico e de empresas, gerentes

        (V6462>=2011 & V6462<=2021) |
          (V6462>=2111 & V6462<=2153) |
          (V6462>=2211 & V6462<=2237) |
          (V6462>=2311 & V6462<=2394) |
          (V6462>=2410 & V6462<=2423) |
          (V6462>=2511 & V6462<=2531) |
          (V6462>=2611 & V6462<=2631),
        2L, #Profissionais das Ciencias e das Artes

        (V6462>=3001 & V6462<=3012) |
          (V6462>=3111 & V6462<=3192) |
          (V6462>=3201 & V6462<=3281) |
          (V6462>=3311 & V6462<=3341) |
          (V6462>=3411 & V6462<=3426) |
          (V6462>=3511 & V6462<=3548) |
          (V6462>=3711 & V6462<=3773) |
          V6462==3911 |
          V6462==3912,
        3L, #Tecnicos de nivel medio

        (V6462>=4101 & V6462<=4152) |
          (V6462>=4201 & V6462<=4241),
        4L, #Trabalhadores de servicos administrativos

        (V6462>=5101 & V6462<=5199) |
          (V6462>=5201 & V6462<=5243),
        5L, #Trabalhadores dos servicos, vendedores do comercio em lojas e mercados

        (V6462>=6110 & V6462<=6139) |
          (V6462>=6201 & V6462<=6239) |
          (V6462>=6301 & V6462<=6329) |
          (V6462>=6410 & V6462<=6430),
        6L, #Trabalhadores agropecuarios, florestais, da caca e da pesca

        (V6462>=7101 & V6462<=7170) |
          (V6462>=7201 & V6462<=7257) |
          (V6462>=7301 & V6462<=7321) |
          (V6462>=7401 & V6462<=7421) |
          (V6462>=7501 & V6462<=7524) |
          (V6462>=7601 & V6462<=7687) |
          (V6462>=7701 & V6462<=7772) |
          (V6462>=7801 & V6462<=7842) |
          (V6462>=8101 & V6462<=8181) |
          (V6462>=8201 & V6462<=8281) |
          (V6462>=8301 & V6462<=8339) |
          (V6462>=8401 & V6462<=8493) |
          (V6462>=8601 & V6462<=8625) |
          V6462==8711,
        7L, #Trabalhadores da producao de bens e servicos industriais

        (V6462>=9101 & V6462<=9193) |
          (V6462>=9501 & V6462<=9543) |
          (V6462>=9911 & V6462<=9922),
        8L, #Trabalhadores de reparacao e manutencao

        V6462==0100 | V6462==0200 | V6462==0300 |
          (V6462>=0401 & V6462<=0413) | (V6462>=0501 & V6462<=0513),
        9L, #Membros das forcas armadas, policiais e bombeiros militares

        default = NA_integer_
      )
    )
  ]

  # gerar conta-propria excluindo trab liberais (grupocup = 1 ou 2)
  df_censo_pes[
    ,
    `:=`(
      conta_propria = data.table::fcase(
        V6930 == 4L & grupocup > 2L
        , "Conta-própria",

        V6930 < 4L |
          V6930 > 4L |
          (V6930 == 4L & grupocup <=2L)
        , "Não conta-própria",

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
        conta_propria == "Conta-própria" |
          V6930 == 3L | # empregados sem carteira
          V6930 == 6L | # nao remunerados
          V6930 == 7L,  # producao para consumo proprio
        "Informal",

        conta_propria == "Não conta-própria" |
          V6930 == 1L | # carteira de trabalho assinada
          V6930 == 2L | # militares e funcionarios publicos
          V6930 == 5L,  # empregadores
        "Formal",

        #V6930 >= 6
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
      #car_motorcycle_sep = i.car_motorcycle_sep,
      car_motorcycle = i.car_motorcycle
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
    V1006 == 1, # filter only individuals from urban areas
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

  df_prop_dom <- df_censo_pes[
    ,
    .(
      prop_dom_urban = sum(V0010[which(V1006 == 1)], na.rm = T) / sum(V0010, na.rm = T),
      prop_car_motorcycle_dom = sum(V0010[which(car_motorcycle == "Carro ou motocicleta" & V1006 == 1)], na.rm = T) / sum(V0010[which(V1006 == 1)], na.rm = T)
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


  df_vars_dom <- df_wghtd_mean_dom[
    df_prop_dom,
    `:=`(
      prop_dom_urban = i.prop_dom_urban,
      prop_car_motorcycle_dom = i.prop_car_motorcycle_dom
    ),
    on = c("code_urban_concentration" = "code_urban_concentration")
  ]

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
      V0661 == 1 & age != "Até 15 anos" & V1006 == 1,
      .(wghtd_mean_commute_time = weighted.mean(commute_time, w = V0010, na.rm = T)),
      by = .(code_urban_concentration)
    ],
    df_censo_pes[
      V1006 == 1, # filter only individuals from urban areas
      .(wghtd_mean_household_income_per_capita = weighted.mean(V6531, w = V0010, na.rm = T)),
      by = .(code_urban_concentration)
    ]
  )

  df_prop_pes_urban <- df_censo_pes[
    ,
    .(prop_pes_urban = sum(V0010[which(V1006 == 1)], na.rm = T) / sum(V0010, na.rm = T)),
    by = .(code_urban_concentration)
  ]

  df_prop_pes <- df_censo_pes[
    V1006 == 1, # filter only individuals from urban areas
    .(
      prop_men = sum(V0010[which(V0601 == 1)], na.rm = T) / sum(V0010, na.rm = T),
      prop_women = sum(V0010[which(V0601 == 2)], na.rm = T) / sum(V0010, na.rm = T),
      prop_pretas_pardas = sum(V0010[which(raca == "Preta ou Parda")], na.rm = T) / sum(V0010,na.rm = T),
      prop_brancas = sum(V0010[which(raca == "Branca")], na.rm = T) / sum(V0010,na.rm = T),
      prop_low_educ = sum(V0010[which(education =="Baixa escolaridade")],na.rm = T) / sum(V0010, na.rm=T),
      prop_high_educ = sum(V0010[which(education == "Alta escolaridade")],na.rm = T) / sum(V0010, na.rm=T),
      prop_age_15_less = sum(V0010[which(age == "Até 15 anos")],na.rm = T) / sum(V0010, na.rm=T),
      prop_age_16_39 = sum(V0010[which(age == "16-39 anos")],na.rm = T) / sum(V0010, na.rm=T),
      prop_age_40_64 = sum(V0010[which(age == "40-64 anos")],na.rm = T) / sum(V0010, na.rm=T),
      prop_age_65_more = sum(V0010[which(age == "65+ anos")],na.rm = T) / sum(V0010, na.rm=T),
      # all work related variables filter age != "Até 15 anos" & V6920 == 1 (situacao ocupacao == ocupada)
      prop_employed = sum(V0010[which(age != "Até 15 anos" & V6920 == 1)],na.rm = T) / sum(V0010[which(age != "Até 15 anos")], na.rm=T),
      prop_formal = sum(V0010[which(informal == "Formal" & age != "Até 15 anos" & V6920 == 1)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1)], na.rm=T),
      prop_informal = sum(V0010[which(informal == "Informal" & age != "Até 15 anos" & V6920 == 1)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1)], na.rm=T),
      prop_work_other_muni = sum(V0010[which(work_muni == "Outro ou mais municípios/país" & age != "Até 15 anos" & V6920 == 1)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1)], na.rm = T),
      prop_work_home_office = sum(V0010[which(work_muni == "Próprio domicílio" & age != "Até 15 anos" & V6920 == 1)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1)], na.rm = T),
      prop_work_same_muni_not_home_office = sum(V0010[which(work_muni == "Mesmo município, mas não no domicílio" & age != "Até 15 anos" & V6920 == 1)]) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1)], na.rm = T),
      prop_industry = sum(V0010[which(sector == "Indústria" & age != "Até 15 anos" & V6920 == 1)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1)], na.rm=T),
      prop_services = sum(V0010[which(sector == "Serviços" & age != "Até 15 anos" & V6920 == 1)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1)], na.rm=T),
      prop_car_motorcycle_pes = sum(V0010[which(car_motorcycle == "Carro ou motocicleta" & age != "Até 15 anos" & V6920 == 1)],na.rm = T) / sum(V0010[which(age != "Até 15 anos" & V6920 == 1)], na.rm=T)

    ),
    by = .(code_urban_concentration)
  ]


  df_vars_pes <- dplyr::left_join(
    df_wghtd_mean_pes, df_prop_pes_urban,
    by = c("code_urban_concentration" = "code_urban_concentration")
  ) %>%
    dplyr::left_join(
      df_prop_pes,
      by = c("code_urban_concentration" = "code_urban_concentration")
      )


  # * merge dom pes vars ----------------------------------------------------
  df_vars_total <- dplyr::left_join(
    df_vars_dom, df_vars_pes,
    by = c("code_urban_concentration" = "code_urban_concentration")
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


# variables description -------------------------------------------------

#' DOM:
#' V0001	UNIDADE DA FEDERAÇÃO
#' V0002	CÓDIGO DO MUNICÍPIO
#' V0011	ÁREA DE PONDERAÇÃO
#' V0300  CONTROLE
#' V0010	PESO AMOSTRAL
#' V0221 "MOTOCICLETA PARA USO PARTICULAR, EXISTÊNCIA:
#'        1- Sim
#'        2- Não
#'        Branco"
#' V0222 "AUTOMÓVEL PARA USO PARTICULAR, EXISTÊNCIA:
#         1- Sim
#         2- Não
#         Branco"



# PES:
#' V0001	UNIDADE DA FEDERAÇÃO
#' V0002	CÓDIGO DO MUNICÍPIO
#' V0011	ÁREA DE PONDERAÇÃO
#' V0010	PESO AMOSTRAL
#' V0300  CONTROLE
#' V1006  "SITUAÇÃO DO DOMICÍLIO:
#'        1- Urbana
#'        2- Rural"
#' V0601 "SEXO:
#         1- Masculino
#         2- Feminino"


# ENSINO SUPERIOR: V0633 OU V6400

#' V0633 "CURSO MAIS ELEVADO QUE FREQUENTOU:
#         01- Creche, pré-escolar (maternal e jardim de infância), classe de alfabetização - CA
#         02- Alfabetização de jovens e adultos
#         03- Antigo primário (elementar)
#         04- Antigo ginásio (médio 1º ciclo)
#         05- Ensino fundamental ou 1º grau (da 1ª a 3ª série/ do 1º ao 4º ano)
#         06- Ensino fundamental ou 1º grau (4ª série/ 5º ano)
#         07- Ensino fundamental ou 1º grau (da 5ª a 8ª série/ 6º ao 9º ano)
#         08- Supletivo do ensino fundamental ou do 1º grau
#         09- Antigo científico, clássico, etc.....(médio 2º ciclo)
#         10- Regular ou supletivo do ensino médio ou do 2º grau
#         11- Superior de graduação
#         12- Especialização de nível superior ( mínimo de 360 horas )
#         13- Mestrado
#         14- Doutorado
#         ' Branco"

#' V6400 "NÍVEL DE INSTRUÇÃO:
#         1- Sem instrução e fundamental incompleto
#         2- Fundamental completo e médio incompleto
#         3- Médio completo e superior incompleto
#         4- Superior completo
#         5- Não determinado "
#' V6036 "VARIÁVEL AUXILIAR DA IDADE CALCULADA EM ANOS:
#         - 0 a 140"
#' V6930 "POSIÇÃO NA OCUPAÇÃO E CATEGORIA DO EMPREGO NO TRABALHO PRINCIPAL
#         1- Empregados com carteira de trabalho assinada
#         2- Militares e funcionários públicos estatutários
#         3- Empregados sem carteira de trabalho assinada
#         4- Conta própria
#         5- Empregadores
#         6- Não remunerados
#         7- Trabalhadores na produção para o próprio consumo
#         Branco"
#' V0648 "NESSE TRABALHO ERA:
#         1- Empregado com carteira de trabalho assinada
#         2- Militar do exército, marinha, aeronáutica, policia militar ou corpo de bombeiros
#         3- Empregado pelo regime jurídico dos funcionários públicos
#         4- Empregado sem carteira de trabalho assinada
#         5- Conta própria
#         6- Empregador
#         7- Não remunerado
#         Branco"


# SETOR DE ATIVIDADE DO TRABALHO: V6471 (CNAE) OU V6462 (CBO)

#' V6471 "ATIVIDADE – código
#(pode ter valor branco)
#- A relação de códigos encontra-se no arquivo: “CNAEDOM2.0_Estrutura 2010.xls”"

#' v6462 Qual era a ocupação que exercia no trabalho que tinha? - código 2000  (branco; banco de códigos de 2000)

# TRABALHADORES EMPREGADOS: V6910 ou V6920
#' V6910 "CONDIÇÃO DE OCUPAÇÃO NA SEMANA DE REFERÊNCIA
#1- Ocupadas
#2- Desocupadas
#Branco"
#' V6920 "SITUAÇÃO DE OCUPAÇÃO NA SEMANA DE REFERÊNCIA
#1- Ocupadas
#2- Desocupadas
#Branco"


#' V0661	"RETORNA DO TRABALHO PARA CASA DIARIAMENTE:
#'       1- Sim
#'       2- Não
#'       Branco"
#' V0662	"QUAL É O TEMPO HABITUAL GASTO DE DESLOCAMENTO DE SUA CASA ATÉ O TRABALHO:
#'       1- Até 05 minutos
#'       2- De 06 minutos até meia hora
#'       3- Mais de meia hora até uma hora
#'       4- Mais de uma hora até duas horas
#'       5- Mais de duas horas
#'       Branco"


# VARIAVEIS POTENCIAIS

# DOM
# V6203 DENSIDADE DE MORADOR/CÔMODO

# PES:
# RACA: V0606
# EM QUE MUNICIPIO TRABALHA: V0660
# V0660 "EM QUE MUNICÍPIO E UNIDADE DA FEDERAÇÃO OU PAÍS ESTRANGEIRO TRABALHA:
#1- No próprio domicílio
#2- Apenas neste município, mas não no próprio domicílio
#3- Em outro município
#4- Em país estrangeiro
#5- Em mais de um município ou país
#Branco"



