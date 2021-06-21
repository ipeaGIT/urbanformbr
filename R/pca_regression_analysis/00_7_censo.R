# description -------------------------------------------------------------

# this script extracts variables and estimates metrics from the demographic census
#..2010 to be used at the pca and regression analysis.

# variables description:

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



# setup -------------------------------------------------------------------

source('R/setup.R')


# define function ----------------------------------------------------------------
f_censo <- function(){

  # read data ---------------------------------------------------------------

  # * censo domicilios ------------------------------------------------------

  cols_to_read_dom <- c(
    'V0001', # UF
    'V0002', # codigo municipio
    #'V0011', # area de ponderacao
    #'V0010', # peso amostral
    "V0300", # controle
    'V0221', # existencia de motocicleta para uso particular
    'V0222'  # existencia de automovel para uso particular
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
    "V0601", # sexo
    "V0633", # curso mais elevado que frequentou
    "V6400", # nivel de instrucao
    "V6036", # idade calculada em anos
    "V6930", # posicao na ocupacao
    "V0648", # nesse trabalho era
    "V6471", # atividade CNAE
    "V6462", # ocupacao CBO
    "V6910", # condicao na ocupacao
    "V6920", # situacao na ocupacao
    "V0661", # retorna do trabalho para casa diariamente
    "V0662", # tempo deslocamento casa-trabalho
    # VARIAVEIS "POTENCIAIS"
    "V0606", # raca
    "V0660"  # em que municipio e UF trabalha
    )

  df_censo_pes <- data.table::fread(
    file = '//storage6/bases2/NINSOC/Bases/Censo_Demografico/2010/CSV/censo_2010.pessoas.csv.bz2',
    #nrows = 5,
    select = cols_to_read_pes#,
    #colClasses = 'character'
  )

  # check total population
  sum(df_censo_pes$V0010, na.rm=T)

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

  # fix muni code
  df_censo_dom[, code_muni := (V0001*100000) + V0002]

  df_censo_pes[, code_muni := (V0001*100000) + V0002]

  # add urban concentration area code to each household and individual based on..
  #.. code municipality
  df_censo_dom[
    df_codes,
    `:=`(
      code_urban_concentration = i.code_urban_concentration
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

  # filter only dom/pes from cities that belong to uca present at df_codes
  data.table::setkey(df_censo_dom, code_urban_concentration)
  df_censo_dom <- df_censo_dom[.(unique(df_codes$code_urban_concentration))]
  # or filter by na at code_urban_concentration
  #df_censo_dom <- df_censo_dom[!is.na(code_urban_concentration)]

  data.table::setkey(df_censo_pes, code_urban_concentration)
  df_censo_pes <- df_censo_pes[.(unique(df_codes$code_urban_concentration))]
  #df_censo_pes <- df_censo_pes[!is.na(code_urban_concentration)]


  # filter workers with daily commute (V0661 == 1)
  data.table::setkey(df_censo_pes, V0661)
  df_censo_pes <- df_censo_pes[.(1)]


  # merge dom + pes data ----------------------------------------------------

  # ATENCAO: PESO AMOSTRAL E O MESMO PARA AS DUAS BASES?
  # ALGUM PROBLEMA FAZER ESTE MERGE?

  # merge household and individual data
  ######### CHECK MERGE
  df_censo_pes[
    df_censo_dom,
    `:=`(
      V0221 = i.V0221,
      V0222 = i.V0222
    ),
    on = c("V0300" = "V0300")
  ]


  # recode commute time
  df_censo_pes[
    ,
    commute_time := data.table::fcase(
      V0662 == 1, 5,
      V0662 == 2, 15,
      V0662 == 3, 45,
      V0662 == 4, 90,
      V0662 == 5, 120)
  ]

  summary(df_censo_pes$commute_time)

  # estimate average travel time --------------------------------------------


  # save data ---------------------------------------------------------------






}

# run function ------------------------------------------------------------


