# description -------------------------------------------------------------

# this script extracts and estimates variates from the demographic census 2010
#..to be used at the pca and regression analysis.

# variables description:

#' DOM:
#' V0001	UNIDADE DA FEDERAÇÃO
#' V0002	CÓDIGO DO MUNICÍPIO
#' V0011	ÁREA DE PONDERAÇÃO
#' V0010	PESO AMOSTRAL
#' V0221 "MOTOCICLETA PARA USO PARTICULAR, EXISTÊNCIA:
#'        1- Sim
#'        2- Não
#'        Branco"
#' v0222 "AUTOMÓVEL PARA USO PARTICULAR, EXISTÊNCIA:
#         1- Sim
#         2- Não
#         Branco"



# PES:
#' V0001	UNIDADE DA FEDERAÇÃO
#' V0002	CÓDIGO DO MUNICÍPIO
#' V0011	ÁREA DE PONDERAÇÃO
#' V0010	PESO AMOSTRAL
#' v0601 "SEXO:
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


# function ----------------------------------------------------------------


