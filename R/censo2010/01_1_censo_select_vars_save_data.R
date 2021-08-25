# description -------------------------------------------------------------

# this script reads the demographic census (individual and household surveys)
# and selects variables to be used at pca and regression analysis.
# both data frames are saved to be used at the analysis.


# setup -------------------------------------------------------------------

source('R/setup.R')


# read data ---------------------------------------------------------------

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
  "V0661", # retorna do trabalho para casa diariamente (1 sim, 2 nao)
  "V0662", # tempo deslocamento casa-trabalho
  "V0606", # raca
  "V0660", # em que municipio e UF trabalha
  "V6531", # rendimento domiciliar (domicilio particular) per capita julho 2010

  "V0641", # trabalho remunerado
  "V0642", # tinha trabalho remunerado do qual estava temporariamente afastado
  "V6940", # subgrupo e categoria no emprego principal (grupo c/ trab. domestico)
  "V0651", # qual o rendimento bruto trab principal (n tem, dinheiro, somente beneficios)
  "V6513"  # rendimento trabalho principal

)

df_censo_pes <- data.table::fread(
  file = '//storage6/bases2/NINSOC/Bases/Censo_Demografico/2010/CSV/censo_2010.pessoas.csv.bz2',
  #nrows = 5,
  select = cols_to_read_pes#,
  #colClasses = 'character'
)

# check total population
#sum(df_censo_pes$V0010, na.rm=T)
# save data ---------------------------------------------------------------

saveRDS(
  object = df_censo_dom,
  file = '../../data/urbanformbr/censo/censo_dom.rds',
  compress = 'xz'
)

saveRDS(
  object = df_censo_pes,
  file = '../../data/urbanformbr/censo/censo_pes.rds',
  compress = 'xz'
)


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
#' #         1- Sim
#' #         2- Não
#' #         Branco"
#' V6203	DENSIDADE DE MORADOR/CÔMODO



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
#'
#'V0641	"NA SEMANA DE 25 A 31/07/10, DURANTE PELO MENOS 1 HORA, TRABALHOU GANHANDO EM DINHEIRO, PRODUTOS, MERCADORIAS OU BENEFÍCIOS:
#'1- Sim
#'2- Não
#'Branco"
#'V0642	"NA SEMANA DE 25 A 31/07/10, TINHA TRABALHO REMUNERADO DO QUAL ESTAVA TEMPORARIAMENTE AFASTADO(A):
#'  1- Sim
#'  2- Não
#'  Branco"
#'  V0606	"COR OU RAÇA:
#'  1- Branca
#'  2- Preta
#'  3- Amarela
#'  4- Parda
#'  5- Indígena
#'  9- Ignorado"
#'
#'  V0660	"EM QUE MUNICÍPIO E UNIDADE DA FEDERAÇÃO OU PAÍS ESTRANGEIRO TRABALHA:
#'  1- No próprio domicílio
#'  2- Apenas neste município, mas não no próprio domicílio
#'  3- Em outro município
#'  4- Em país estrangeiro
#'  5- Em mais de um município ou país
#'  Branco"
#'
#'  V6940	"SUBGRUPO E CATEGORIA DO EMPREGO NO TRABALHO PRINCIPAL
#'  1- Trabalhadores domésticos com carteira de trabalho assinada
#'  2- Trabalhadores domésticos sem carteira de trabalho assinada
#'  3- Demais empregados com carteira de trabalho assinada
#'  4- Militares e funcionários públicos estatutários
#'  5- Demais empregados sem carteira de trabalho assinada
#'  Branco"
#'
#'  V0651	"NO TRABALHO PRINCIPAL, QUAL ERA O RENDIMENTO BRUTO (OU A RETIRADA) MENSAL QUE GANHAVA HABITUALMENTE EM JULHO DE 2010:
#'  0- Não tem
#'  1- Em dinheiro, produtos ou mercadorias
#'  2- Somente em benefícios
#'  Branco"
#'
#'  V6513	RENDIMENTO NO TRABALHO PRINCIPAL: (pode ter valor branco)





