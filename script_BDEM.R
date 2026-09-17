# script roteiro do BDEM - no repositório Projeto_BDEM_2016
# Antes de começar a fazer qualquer coisa:
# a) Coloque todos os arquivos postados no Classroom (já descompactados) dentro do repositório local Projeto_BDEM_2016
# b) commit este roteiro com a mensagem "dados, arquivos de texto e script roteiro BDEM" e envie para o repositório Projeto_BDEM_2016
# c) salve o script com outro nome (script_BDEM.R) e commit com a mensagem "script BDEM" e envie para o repositório Projeto_BDEM_2016

# Ao inserir os comandos em cada Tarefa de cada Etapa, mantenha as linhas de comentários e orientações colocadas pela professora


##################################
# ETAPA 1: BANCO DE DADOS DO SIM
##################################
# Você deve criar e estar na branch SIM antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1. Leitura do banco de dados SIM_2016 com 1309774 linhas e 87 colunas com o nome de dados_sim
# Verificar se a leitura foi feita corretamente e a estrutura dos dados

dados_sim = read.csv("SIM_2016.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
dim(dados_sim)
str(dados_sim)

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SIM - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# As colunas serão: 1, 3, 9, 10, 11, 14, 17, 35, 47
# Nomes das respectivas variáveis: CONTADOR, TIPOBITO, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, CAUSABAS

dados_sim_1 = dados_sim[, c(1, 3, 9, 10, 11, 14, 17, 35, 47)]

names(dados_sim_1) = c("CONTADOR", "TIPOBITO", "IDADE", "SEXO", 
                       "RACACOR", "ESC2010", "CODMUNRES", 
                       "TPMORTEOCO", "CAUSABAS")

str(dados_sim_1)

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SIM - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11:8344      12:3763     13:16799    14:2157      15:38557     16:2995     17:7490
# 21:34362     22:19187    23:54276    24:21922     25:28041     26:66928    27:20769    28:13516     29:88094
# 31:135257    32:22868    33:141089   35:296359
# 41:74740     42:40270    43:87583
# 50:16749     51:17535    52:38074    53:12050 

dados_sim_2 = dados_sim_1[substr(dados_sim_1$CODMUNRES, 1, 2) == "25", ]
nrow(dados_sim_2)
str(dados_sim_2)

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SIM - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis:
# TIPOBITO, SEXO, RACACOR, ESC2010, TPMORTEOCO, CAUSABAS
# Avalie também os valores das variável IDADE (não estranhe mas idade é composta de um dígito inicial que indica a unidade de medida)
# Unidades de medida a serem consideradas em IDADE: 0: minutos, 1: horas, 2: dias, 3: meses, 4: anos, 5: idade maior que 100 anos
# Atenção: a unidade de medida de IDADE no DICIONÀRIO do SIM está errada
# O propósito das avaliações acima é verificar se as categorias estão de acordo com o dicionário do SIM ou se aparecem categorias estranhas

cat("TIPOBITO\n")
table(dados_sim_2$TIPOBITO, useNA = "ifany")
cat("\nSEXO\n")
table(dados_sim_2$SEXO, useNA = "ifany")
cat("\nRACACOR\n")
table(dados_sim_2$RACACOR, useNA = "ifany")
cat("\nESC2010\n")
table(dados_sim_2$ESC2010, useNA = "ifany")
cat("\nTPMORTEOCO\n")
table(dados_sim_2$TPMORTEOCO, useNA = "ifany")
cat("\nCAUSABAS\n")
table(dados_sim_2$CAUSABAS, useNA = "ifany")
cat("\nUNIDADE DA IDADE\n")
table(substr(dados_sim_2$IDADE, 1, 1), useNA = "ifany")
cat("\nIDADE COMPLETA\n")
table(dados_sim_2$IDADE, useNA = "ifany")

# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SIM - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# Verifique o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 9999 para NA

dados_sim_2$TIPOBITO[dados_sim_2$TIPOBITO == "9"] <- NA
dados_sim_2$SEXO[dados_sim_2$SEXO %in% c("0", "9")] <- NA
dados_sim_2$RACACOR[dados_sim_2$RACACOR == "9"] <- NA
dados_sim_2$ESC2010[dados_sim_2$ESC2010 == "9"] <- NA
dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == "9"] <- NA
dados_sim_2$IDADE[dados_sim_2$IDADE == "9999"] <- NA
dados_sim_2$IDADE[dados_sim_2$IDADE == "9"] <- NA
table(dados_sim_2$TIPOBITO, useNA = "ifany")
table(dados_sim_2$SEXO, useNA = "ifany")
table(dados_sim_2$RACACOR, useNA = "ifany")
table(dados_sim_2$ESC2010, useNA = "ifany")
table(dados_sim_2$TPMORTEOCO, useNA = "ifany")
table(dados_sim_2$IDADE, useNA = "ifany")

# Ao terminar a Tarefa 5 commit com a mensagem "script BDEM - SIM - tarefas 1 a 5" e envie para o repositório Projeto_BDEM_2016


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a PRIMEIRA LETRA da legenda é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis dentro do banco de dados

dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO,
                              levels = c("1", "2"),
                              labels = c("Fetal", "Não fetal"))

dados_sim_2$SEXO = factor(dados_sim_2$SEXO,
                          levels = c("1", "2"),
                          labels = c("Masculino", "Feminino"))

dados_sim_2$RACACOR = factor(dados_sim_2$RACACOR,
                             levels = c("1", "2", "3", "4", "5"),
                             labels = c("Branca", "Preta", "Amarela", 
                                        "Parda", "Indígena"))

dados_sim_2$ESC2010 = factor(dados_sim_2$ESC2010,
                             levels = c("0", "1", "2", "3", "4", "5"),
                             labels = c("Sem escolaridade",
                                        "Fundamental I",
                                        "Fundamental II",
                                        "Médio",
                                        "Superior incompleto",
                                        "Superior completo"))

dados_sim_2$TPMORTEOCO = factor(dados_sim_2$TPMORTEOCO,
                                levels = c("1", "2", "3", "4", "5", "8"),
                                labels = c("Na gravidez",
                                           "No parto",
                                           "No abortamento",
                                           "Até 42 dias após o término do parto",
                                           "De 43 dias a 1 ano após o término da gestação",
                                           "Não ocorreu nestes períodos"))

str(dados_sim_2)

# Ao terminar a Tarefa 6 commit com a mensagem "script BDEM - SIM - tarefas 1 a 6" e envie para o repositório Projeto_BDEM_2016


LETRA = substr(dados_sim_2$CAUSABAS, 1, 1)
NUM = as.numeric(substr(dados_sim_2$CAUSABAS, 2, 3))


# Base inicial (municípios)
base = data.frame(CODMUNRES = sort(unique(dados_sim_2$CODMUNRES)))


# TO - Total de óbitos
TO = as.data.frame(table(factor(dados_sim_2$CODMUNRES)))

names(TO) = c("CODMUNRES","TO")

base = merge(base, TO, by = "CODMUNRES", all.x = TRUE)


# TORC - Registros completos nas 87 variáveis
dados_UF = dados_sim[substr(as.character(dados_sim$CODMUNRES),1,2) == "25",]

dados_UF_comp = dados_UF[complete.cases(dados_UF),]

TORC = as.data.frame(table(factor(dados_UF_comp$CODMUNRES,
                                  levels = base$CODMUNRES)))

names(TORC) = c("CODMUNRES","TORC")

base = merge(base, TORC, by = "CODMUNRES", all.x = TRUE)


# TORCR - Total de óbitos com registros completos nas 9 variáveis selecionadas
dados_UF_1 = dados_sim_1[substr(as.character(dados_sim_1$CODMUNRES),1,2) == "25",]

dados_UF_1_comp = dados_UF_1[complete.cases(dados_UF_1),]

TORCR = as.data.frame(table(factor(dados_UF_1_comp$CODMUNRES,
                                   levels = base$CODMUNRES)))

names(TORCR) = c("CODMUNRES","TORCR")

base = merge(base, TORCR, by = "CODMUNRES", all.x = TRUE)


# TO_NN - Total de óbitos por causas externas (CID-10: V01-Y98)
tab = table(factor(dados_sim_2$CODMUNRES[
  LETRA %in% c("V","W","X","Y")
], levels = base$CODMUNRES))

TO_NN = as.data.frame(tab)

names(TO_NN) = c("CODMUNRES","TO_NN")

base = merge(base, TO_NN, by = "CODMUNRES", all.x = TRUE)


# TO_N - Total de óbitos por causas naturais
tab = table(factor(dados_sim_2$CODMUNRES[
  !(LETRA %in% c("V","W","X","Y"))
], levels = base$CODMUNRES))

TO_N = as.data.frame(tab)

names(TO_N) = c("CODMUNRES","TO_N")

base = merge(base, TO_N, by = "CODMUNRES", all.x = TRUE)


# TO_CB_I - Doenças infecciosas e parasitárias (A00-B99)
tab = table(factor(dados_sim_2$CODMUNRES[
  LETRA %in% c("A","B")
], levels = base$CODMUNRES))

TO_CB_I = as.data.frame(tab)

names(TO_CB_I) = c("CODMUNRES","TO_CB_I")

base = merge(base, TO_CB_I, by = "CODMUNRES", all.x = TRUE)


# TO_CB_N - Neoplasias e doenças do sangue (C00-D48 e D50-D89)
tab = table(factor(dados_sim_2$CODMUNRES[
  LETRA == "C" |
    (LETRA == "D" & NUM <= 48) |
    (LETRA == "D" & NUM >= 50)
], levels = base$CODMUNRES))

TO_CB_N = as.data.frame(tab)

names(TO_CB_N) = c("CODMUNRES","TO_CB_N")

base = merge(base, TO_CB_N, by = "CODMUNRES", all.x = TRUE)


# TO_CB_C - Doenças do aparelho circulatório (I00-I99)
tab = table(factor(dados_sim_2$CODMUNRES[
  LETRA == "I"
], levels = base$CODMUNRES))

TO_CB_C = as.data.frame(tab)

names(TO_CB_C) = c("CODMUNRES","TO_CB_C")

base = merge(base, TO_CB_C, by = "CODMUNRES", all.x = TRUE)


# TO_CB_R - Doenças do aparelho respiratório (J00-J99)
tab = table(factor(dados_sim_2$CODMUNRES[
  LETRA == "J"
], levels = base$CODMUNRES))

TO_CB_R = as.data.frame(tab)

names(TO_CB_R) = c("CODMUNRES","TO_CB_R")

base = merge(base, TO_CB_R, by = "CODMUNRES", all.x = TRUE)


# TO_CB_O - Outras causas básicas naturais
tab = table(factor(dados_sim_2$CODMUNRES[
  !(LETRA %in% c("V","W","X","Y")) &
    !(LETRA %in% c("A","B","C","I","J")) &
    !(LETRA == "D" & NUM <= 48) &
    !(LETRA == "D" & NUM >= 50)
], levels = base$CODMUNRES))

TO_CB_O = as.data.frame(tab)

names(TO_CB_O) = c("CODMUNRES","TO_CB_O")

base = merge(base, TO_CB_O, by = "CODMUNRES", all.x = TRUE)


# TO_M - Total de óbitos masculinos
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$SEXO == "Masculino"
], levels = base$CODMUNRES))

TO_M = as.data.frame(tab)

names(TO_M) = c("CODMUNRES","TO_M")

base = merge(base, TO_M, by = "CODMUNRES", all.x = TRUE)


# TO_F - Total de óbitos femininos
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$SEXO == "Feminino"
], levels = base$CODMUNRES))

TO_F = as.data.frame(tab)

names(TO_F) = c("CODMUNRES","TO_F")

base = merge(base, TO_F, by = "CODMUNRES", all.x = TRUE)


# TO_F_IF - Total de óbitos femininos em idade fértil (15 a 49 anos)
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$SEXO == "Feminino" &
    dados_sim_2$IDADE >= 415 &
    dados_sim_2$IDADE <= 449
], levels = base$CODMUNRES))

TO_F_IF = as.data.frame(tab)

names(TO_F_IF) = c("CODMUNRES","TO_F_IF")

base = merge(base, TO_F_IF, by = "CODMUNRES", all.x = TRUE)


# TO_FT - Total de óbitos fetais
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TIPOBITO == "Fetal"
], levels = base$CODMUNRES))

TO_FT = as.data.frame(tab)

names(TO_FT) = c("CODMUNRES","TO_FT")

base = merge(base, TO_FT, by = "CODMUNRES", all.x = TRUE)


# TO_NT - Total de óbitos neonatais (0 a 27 dias)
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TIPOBITO == "Não fetal" &
    dados_sim_2$IDADE >= 200 &
    dados_sim_2$IDADE <= 227
], levels = base$CODMUNRES))

TO_NT = as.data.frame(tab)

names(TO_NT) = c("CODMUNRES","TO_NT")

base = merge(base, TO_NT, by = "CODMUNRES", all.x = TRUE)


# TO_NT_P - Total de óbitos neonatais precoces (0 a 6 dias)
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TIPOBITO == "Não fetal" &
    dados_sim_2$IDADE >= 200 &
    dados_sim_2$IDADE <= 206
], levels = base$CODMUNRES))

TO_NT_P = as.data.frame(tab)

names(TO_NT_P) = c("CODMUNRES","TO_NT_P")

base = merge(base, TO_NT_P, by = "CODMUNRES", all.x = TRUE)


# TO_NT_T - Total de óbitos neonatais tardios (7 a 27 dias)
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TIPOBITO == "Não fetal" &
    dados_sim_2$IDADE >= 207 &
    dados_sim_2$IDADE <= 227
], levels = base$CODMUNRES))

TO_NT_T = as.data.frame(tab)

names(TO_NT_T) = c("CODMUNRES","TO_NT_T")

base = merge(base, TO_NT_T, by = "CODMUNRES", all.x = TRUE)


# TO_PNT - Total de óbitos pós-neonatais (28 dias a 364 dias)
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TIPOBITO == "Não fetal" &
    dados_sim_2$IDADE >= 228 &
    dados_sim_2$IDADE <= 311
], levels = base$CODMUNRES))

TO_PNT = as.data.frame(tab)

names(TO_PNT) = c("CODMUNRES","TO_PNT")

base = merge(base, TO_PNT, by = "CODMUNRES", all.x = TRUE)


# TONT_B - Óbitos neonatais de raça/cor branca
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TIPOBITO == "Não fetal" &
    dados_sim_2$IDADE >= 200 &
    dados_sim_2$IDADE <= 227 &
    dados_sim_2$RACACOR == "Branca"
], levels = base$CODMUNRES))

TONT_B = as.data.frame(tab)

names(TONT_B) = c("CODMUNRES","TONT_B")

base = merge(base, TONT_B, by = "CODMUNRES", all.x = TRUE)


# TONT_PT - Óbitos neonatais de raça/cor preta
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TIPOBITO == "Não fetal" &
    dados_sim_2$IDADE >= 200 &
    dados_sim_2$IDADE <= 227 &
    dados_sim_2$RACACOR == "Preta"
], levels = base$CODMUNRES))

TONT_PT = as.data.frame(tab)

names(TONT_PT) = c("CODMUNRES","TONT_PT")

base = merge(base, TONT_PT, by = "CODMUNRES", all.x = TRUE)


# TONT_A - Óbitos neonatais de raça/cor amarela
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TIPOBITO == "Não fetal" &
    dados_sim_2$IDADE >= 200 &
    dados_sim_2$IDADE <= 227 &
    dados_sim_2$RACACOR == "Amarela"
], levels = base$CODMUNRES))

TONT_A = as.data.frame(tab)

names(TONT_A) = c("CODMUNRES","TONT_A")

base = merge(base, TONT_A, by = "CODMUNRES", all.x = TRUE)


# TONT_PD - Óbitos neonatais de raça/cor parda
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TIPOBITO == "Não fetal" &
    dados_sim_2$IDADE >= 200 &
    dados_sim_2$IDADE <= 227 &
    dados_sim_2$RACACOR == "Parda"
], levels = base$CODMUNRES))

TONT_PD = as.data.frame(tab)

names(TONT_PD) = c("CODMUNRES","TONT_PD")

base = merge(base, TONT_PD, by = "CODMUNRES", all.x = TRUE)


# TONT_I - Óbitos neonatais de raça/cor indígena
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TIPOBITO == "Não fetal" &
    dados_sim_2$IDADE >= 200 &
    dados_sim_2$IDADE <= 227 &
    dados_sim_2$RACACOR == "Indígena"
], levels = base$CODMUNRES))

TONT_I = as.data.frame(tab)

names(TONT_I) = c("CODMUNRES","TONT_I")

base = merge(base, TONT_I, by = "CODMUNRES", all.x = TRUE)


# TO_MT - Total de óbitos maternos - durante a gestação, parto,
# abortamento, até 42 dias ou tardio
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO %in% c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Até 42 dias após o término do parto",
    "De 43 dias a 1 ano após o término da gestação"
  )
], levels = base$CODMUNRES))

TO_MT = as.data.frame(tab)

names(TO_MT) = c("CODMUNRES","TO_MT")

base = merge(base, TO_MT, by = "CODMUNRES", all.x = TRUE)


# TO_MT_DG - Óbitos maternos durante a gestação
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO == "Na gravidez"
], levels = base$CODMUNRES))

TO_MT_DG = as.data.frame(tab)

names(TO_MT_DG) = c("CODMUNRES","TO_MT_DG")

base = merge(base, TO_MT_DG, by = "CODMUNRES", all.x = TRUE)


# TO_MT_PT - Óbitos maternos no parto
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO == "No parto"
], levels = base$CODMUNRES))

TO_MT_PT = as.data.frame(tab)

names(TO_MT_PT) = c("CODMUNRES","TO_MT_PT")

base = merge(base, TO_MT_PT, by = "CODMUNRES", all.x = TRUE)


# TO_MT_AB - Óbitos maternos no abortamento
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO == "No abortamento"
], levels = base$CODMUNRES))

TO_MT_AB = as.data.frame(tab)

names(TO_MT_AB) = c("CODMUNRES","TO_MT_AB")

base = merge(base, TO_MT_AB, by = "CODMUNRES", all.x = TRUE)


# TO_MT_42 - Óbitos maternos até 42 dias após o parto
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO == "Até 42 dias após o término do parto"
], levels = base$CODMUNRES))

TO_MT_42 = as.data.frame(tab)

names(TO_MT_42) = c("CODMUNRES","TO_MT_42")

base = merge(base, TO_MT_42, by = "CODMUNRES", all.x = TRUE)


# TO_MT_43 - Óbitos maternos tardios (43 dias a 1 ano)
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO == "De 43 dias a 1 ano após o término da gestação"
], levels = base$CODMUNRES))

TO_MT_43 = as.data.frame(tab)

names(TO_MT_43) = c("CODMUNRES","TO_MT_43")

base = merge(base, TO_MT_43, by = "CODMUNRES", all.x = TRUE)


# TO_MT_P - Total de óbitos maternos precoces
# Soma dos óbitos durante a gestação, parto,
# abortamento e até 42 dias após o parto
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO %in% c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Até 42 dias após o término do parto"
  )
], levels = base$CODMUNRES))

TO_MT_P = as.data.frame(tab)

names(TO_MT_P) = c("CODMUNRES","TO_MT_P")

base = merge(base, TO_MT_P, by = "CODMUNRES", all.x = TRUE)


# TO_MT_P_I - Total de óbitos maternos precoces
# de mulheres em idade fértil - Idade fértil: 15 a 49 anos
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO %in% c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Até 42 dias após o término do parto"
  ) &
    dados_sim_2$IDADE >= 415 &
    dados_sim_2$IDADE <= 449
], levels = base$CODMUNRES))

TO_MT_P_I = as.data.frame(tab)

names(TO_MT_P_I) = c("CODMUNRES","TO_MT_P_I")

base = merge(base, TO_MT_P_I, by = "CODMUNRES", all.x = TRUE)


# TO_MT_P_ES - Óbitos maternos precoces de mulheres sem escolaridade
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO %in% c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Até 42 dias após o término do parto"
  ) &
    dados_sim_2$ESC2010 == "Sem escolaridade"
], levels = base$CODMUNRES))

TO_MT_P_ES = as.data.frame(tab)

names(TO_MT_P_ES) = c("CODMUNRES","TO_MT_P_ES")

base = merge(base, TO_MT_P_ES, by = "CODMUNRES", all.x = TRUE)


# TO_MT_P_EFI - Óbitos maternos precoces de mulheres com Fundamental I
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO %in% c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Até 42 dias após o término do parto"
  ) &
    dados_sim_2$ESC2010 == "Fundamental I"
], levels = base$CODMUNRES))

TO_MT_P_EFI = as.data.frame(tab)

names(TO_MT_P_EFI) = c("CODMUNRES","TO_MT_P_EFI")

base = merge(base, TO_MT_P_EFI, by = "CODMUNRES", all.x = TRUE)


# TO_MT_P_EFII - Óbitos maternos precoces de mulheres com Fundamental II
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO %in% c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Até 42 dias após o término do parto"
  ) &
    dados_sim_2$ESC2010 == "Fundamental II"
], levels = base$CODMUNRES))

TO_MT_P_EFII = as.data.frame(tab)

names(TO_MT_P_EFII) = c("CODMUNRES","TO_MT_P_EFII")

base = merge(base, TO_MT_P_EFII, by = "CODMUNRES", all.x = TRUE)


# TO_MT_P_EM - Óbitos maternos precoces de mulheres com escolaridade média
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO %in% c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Até 42 dias após o término do parto"
  ) &
    dados_sim_2$ESC2010 == "Médio"
], levels = base$CODMUNRES))

TO_MT_P_EM = as.data.frame(tab)

names(TO_MT_P_EM) = c("CODMUNRES","TO_MT_P_EM")

base = merge(base, TO_MT_P_EM, by = "CODMUNRES", all.x = TRUE)


# TO_MT_P_ESI - Óbitos maternos precoces de mulheres com superior incompleto
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO %in% c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Até 42 dias após o término do parto"
  ) &
    dados_sim_2$ESC2010 == "Superior incompleto"
], levels = base$CODMUNRES))

TO_MT_P_ESI = as.data.frame(tab)

names(TO_MT_P_ESI) = c("CODMUNRES","TO_MT_P_ESI")

base = merge(base, TO_MT_P_ESI, by = "CODMUNRES", all.x = TRUE)


# TO_MT_P_ESC - Óbitos maternos precoces de mulheres com escolaridade superior completa
tab = table(factor(dados_sim_2$CODMUNRES[
  dados_sim_2$TPMORTEOCO %in% c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Até 42 dias após o término do parto"
  ) &
    dados_sim_2$ESC2010 == "Superior completo"
], levels = base$CODMUNRES))

TO_MT_P_ESC = as.data.frame(tab)

names(TO_MT_P_ESC) = c("CODMUNRES","TO_MT_P_ESC")

base = merge(base, TO_MT_P_ESC, by = "CODMUNRES", all.x = TRUE)


# código da UF
linha_estado = data.frame(matrix(ncol = ncol(base), nrow = 1))

names(linha_estado) = names(base)

linha_estado[, -1] = colSums(base[, -1])

linha_estado$CODMUNRES = 25


# Banco de dados final para a Paraíba
SIM_UF = rbind(linha_estado, base)

SIM_UF$NIVEL = c("UF", rep("MUNICIPIO", nrow(SIM_UF)-1))

SIM_UF$ANO = 2016

SIM_UF = SIM_UF[, c(
  "ANO",
  "NIVEL",
  "CODMUNRES",
  names(SIM_UF)[!names(SIM_UF) %in%
                  c("ANO","NIVEL","CODMUNRES")]
)]

SIM_UF$CODMUNRES = as.character(SIM_UF$CODMUNRES)


# Verificando o banco final
str(SIM_UF)
head(SIM_UF)
dim(SIM_UF)


# Tarefa 8. Exportar o banco de dados com o nome SIM_UF.csv
# (Exemplo: SIM_RJ.csv)

write.csv(SIM_UF, "SIM_PB.csv", row.names = FALSE)


# Ao terminar a Tarefa 8 fazer um commit com o comentário
# "dados SIM_UF 2016 e script - SIM - tarefas 1 a 8"
# e envie para o repositório Projeto_BDEM_2016

####################################
# ETAPA 2: BANCO DE DADOS DO SINASC
####################################
# Você deve criar e estar na branch SINASC antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1. Leitura do banco de dados SINASC_2016 com 2857800 linhas e 61 colunas com o nome de dados_sinasc
# Verificar se a leitura foi feita corretamente e a estrutura dos dados
# Por uma questão de padronização coloque todos os nomes das variáveis em letra maiúscula,
# usando o comando names(dados_sinasc) = toupper(names(dados_sinasc))

dados_sinasc = read.csv("SINASC_2016.csv",
                        sep = ";",
                        header = TRUE,
                        stringsAsFactors = FALSE)

dim(dados_sinasc)
str(dados_sinasc)

names(dados_sinasc) = toupper(names(dados_sinasc))
names(dados_sinasc)

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SINASC - tarefa 1" e envie para o repositório Projeto_BDEM_2016

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# As colunas serão 3, 4, 5, 6, 11, 12, 13, 14, 18, 20, 21, 22, 23, 34, 37, 43, 47, 58, 59, 60, 61
# Nomes das respectivas variáveis: CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO, 
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK, CONTADOR

dados_sinasc_1 = dados_sinasc[, c(
  3, 4, 5, 6, 11, 12, 13, 14, 18, 20, 21, 22, 23,
  34, 37, 43, 47, 58, 59, 60, 61
)]

names(dados_sinasc_1) = c(
  "CODMUNNASC",
  "LOCNASC",
  "IDADEMAE",
  "ESTCIVMAE",
  "CODMUNRES",
  "GESTACAO",
  "GRAVIDEZ",
  "PARTO",
  "SEXO",
  "APGAR5",
  "RACACOR",
  "PESO",
  "IDANOMAL",
  "ESCMAE2010",
  "RACACORMAE",
  "SEMAGESTAC",
  "TPAPRESENT",
  "TPROBSON",
  "PARIDADE",
  "KOTELCHUCK",
  "CONTADOR"
)

dim(dados_sinasc_1)
str(dados_sinasc_1)

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 26602     12: 15773     13: 76703     14: 11376     15: 137681    16: 15521      17: 23870
# 21: 110493    22: 46986     23: 126246    24: 45366     25: 56083     26: 130733     27: 48164     28: 32218     29: 199830
# 31: 253520    32: 53413     33: 219129    35: 601437     
# 41: 155066    42: 95313     43: 141411
# 50: 42432     51: 53531     52: 95563     53: 43340 

dados_sinasc_2 = dados_sinasc_1[
  substr(dados_sinasc_1$CODMUNRES, 1, 2) == "25",
]

dim(dados_sinasc_2)
nrow(dados_sinasc_2)

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK
# Avalie também os valores das variáveis quantitativas de IDADEMAE, SEMAGESTAC, APGAR5 e PESO

table(dados_sinasc_2$LOCNASC, useNA = "ifany")
table(dados_sinasc_2$ESTCIVMAE, useNA = "ifany")
table(dados_sinasc_2$GESTACAO, useNA = "ifany")
table(dados_sinasc_2$GRAVIDEZ, useNA = "ifany")
table(dados_sinasc_2$PARTO, useNA = "ifany")
table(dados_sinasc_2$SEXO, useNA = "ifany")
table(dados_sinasc_2$RACACOR, useNA = "ifany")
table(dados_sinasc_2$IDANOMAL, useNA = "ifany")
table(dados_sinasc_2$ESCMAE2010, useNA = "ifany")
table(dados_sinasc_2$RACACORMAE, useNA = "ifany")
table(dados_sinasc_2$TPAPRESENT, useNA = "ifany")
table(dados_sinasc_2$TPROBSON, useNA = "ifany")
table(dados_sinasc_2$PARIDADE, useNA = "ifany")
table(dados_sinasc_2$KOTELCHUCK, useNA = "ifany")

summary(dados_sinasc_2$IDADEMAE)
summary(dados_sinasc_2$SEMAGESTAC)
summary(dados_sinasc_2$APGAR5)
summary(dados_sinasc_2$PESO)

# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# Verifique o dicionário do SINASC para identificar qual o código das categorias de cada variável
# KOTELCHUCK = 9 significa "Não informado"   TPROBSON = 11 significa "Não classificado por falta de informação"
# Em variáveis quantitativas como IDADEMAE verificar se existem valores como 9999 para NA

dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == "9"] <- NA

dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == "9"] <- NA

dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == "9"] <- NA

dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == "9"] <- NA

dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == "9"] <- NA

dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == "0" | dados_sinasc_2$SEXO == "9"] <- NA

dados_sinasc_2$RACACOR[dados_sinasc_2$RACACOR == "9"] <- NA

dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == "9"] <- NA

dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == "9"] <- NA

dados_sinasc_2$RACACORMAE[dados_sinasc_2$RACACORMAE == "9"] <- NA

dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == "9"] <- NA

dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == "11"] <- NA

dados_sinasc_2$PARIDADE[dados_sinasc_2$PARIDADE == "9"] <- NA

dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == "9"] <- NA

dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == "9999"] <- NA

# Ao terminar a Tarefa 5 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 5" e envie para o repositório Projeto_BDEM_2016


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da legenda é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis dentro do banco de dados

dados_sinasc_2$LOCNASC = factor(
  dados_sinasc_2$LOCNASC,
  levels = c("1", "2", "3", "4", "5"),
  labels = c(
    "Hospital",
    "Outros estabelecimentos de saúde",
    "Domicílio",
    "Outros",
    "Aldeia indígena"
  )
)

dados_sinasc_2$ESTCIVMAE = factor(
  dados_sinasc_2$ESTCIVMAE,
  levels = c("1", "2", "3", "4", "5"),
  labels = c(
    "Solteira",
    "Casada",
    "Viúva",
    "Separada judicialmente/divorciada",
    "União estável"
  )
)

dados_sinasc_2$GESTACAO = factor(
  dados_sinasc_2$GESTACAO,
  levels = c("1", "2", "3", "4", "5", "6"),
  labels = c(
    "Menos de 22 semanas",
    "22 a 27 semanas",
    "28 a 31 semanas",
    "32 a 36 semanas",
    "37 a 41 semanas",
    "42 semanas e mais"
  )
)

dados_sinasc_2$GRAVIDEZ = factor(
  dados_sinasc_2$GRAVIDEZ,
  levels = c("1", "2", "3"),
  labels = c(
    "Única",
    "Dupla",
    "Tripla ou mais"
  )
)

dados_sinasc_2$PARTO = factor(
  dados_sinasc_2$PARTO,
  levels = c("1", "2"),
  labels = c(
    "Vaginal",
    "Cesáreo"
  )
)

dados_sinasc_2$SEXO = factor(
  dados_sinasc_2$SEXO,
  levels = c("1", "2"),
  labels = c(
    "Masculino",
    "Feminino"
  )
)

dados_sinasc_2$RACACOR = factor(
  dados_sinasc_2$RACACOR,
  levels = c("1", "2", "3", "4", "5"),
  labels = c(
    "Branca",
    "Preta",
    "Amarela",
    "Parda",
    "Indígena"
  )
)

dados_sinasc_2$IDANOMAL = factor(
  dados_sinasc_2$IDANOMAL,
  levels = c("1", "2"),
  labels = c(
    "Sim",
    "Não"
  )
)

dados_sinasc_2$ESCMAE2010 = factor(
  dados_sinasc_2$ESCMAE2010,
  levels = c("0", "1", "2", "3", "4", "5"),
  labels = c(
    "Sem escolaridade",
    "Fundamental I",
    "Fundamental II",
    "Médio",
    "Superior incompleto",
    "Superior completo"
  )
)

dados_sinasc_2$RACACORMAE = factor(
  dados_sinasc_2$RACACORMAE,
  levels = c("1", "2", "3", "4", "5"),
  labels = c(
    "Branca",
    "Preta",
    "Amarela",
    "Parda",
    "Indígena"
  )
)

dados_sinasc_2$TPAPRESENT = factor(
  dados_sinasc_2$TPAPRESENT,
  levels = c("1", "2", "3"),
  labels = c(
    "Cefálico",
    "Pélvica ou podálica",
    "Transversa"
  )
)

dados_sinasc_2$TPROBSON = factor(
  dados_sinasc_2$TPROBSON,
  levels = as.character(1:10),
  labels = paste("Grupo de Robson", 1:10)
)

dados_sinasc_2$PARIDADE = factor(
  dados_sinasc_2$PARIDADE,
  levels = c("0", "1"),
  labels = c(
    "Nulípara",
    "Multípara"
  )
)

dados_sinasc_2$KOTELCHUCK = factor(
  dados_sinasc_2$KOTELCHUCK,
  levels = c("1", "2", "3", "4", "5"),
  labels = c(
    "Não realizou pré-natal",
    "Inadequado",
    "Intermediário",
    "Adequado",
    "Mais que adequado"
  )
)

str(dados_sinasc_2)

# Ao terminar a Tarefa 6 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 6" e envie para o repositório Projeto_BDEM_2016


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$PEREG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator

# Tarefa 7. Categorizar PESO
dados_sinasc_2$F_PESO = cut(
  as.numeric(dados_sinasc_2$PESO),
  breaks = c(-Inf, 2499, 3999, Inf),
  labels = c(
    "Baixo peso",
    "Peso normal",
    "Macrossomia"
  )
)

# Categorizar IDADEMAE
dados_sinasc_2$F_IDADE = cut(
  as.numeric(dados_sinasc_2$IDADEMAE),
  breaks = c(-Inf, 14, 19, 24, 29, 34, 39, 44, 49, Inf),
  labels = c(
    "<15",
    "15-19",
    "20-24",
    "25-29",
    "30-34",
    "35-39",
    "40-44",
    "45-49",
    "50+"
  )
)

# Categorizar APGAR5
dados_sinasc_2$F_APGAR5 = cut(
  as.numeric(dados_sinasc_2$APGAR5),
  breaks = c(-Inf, 6, Inf),
  labels = c(
    "Baixo",
    "Normal"
  )
)

# Criar variável de peregrinação
dados_sinasc_2$PEREG = ifelse(
  is.na(dados_sinasc_2$CODMUNNASC) |
    is.na(dados_sinasc_2$CODMUNRES),
  NA,
  ifelse(
    dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES,
    "Não",
    "Sim"
  )
)
dados_sinasc_2$PEREG = factor(
  dados_sinasc_2$PEREG,
  levels = c("Não", "Sim")
)

# Criar variável de estado civil

dados_sinasc_2$ESTCIV = ifelse(
  is.na(dados_sinasc_2$ESTCIVMAE),
  NA,
  ifelse(
    dados_sinasc_2$ESTCIVMAE %in% c(
      "Solteira",
      "Viúva",
      "Separada judicialmente/divorciada"
    ),
    "Sem companheiro",
    ifelse(
      dados_sinasc_2$ESTCIVMAE %in% c(
        "Casada",
        "União estável"
      ),
      "Com companheiro",
      NA
    )
  )
)
dados_sinasc_2$ESTCIV = factor(
  dados_sinasc_2$ESTCIV,
  levels = c(
    "Sem companheiro",
    "Com companheiro"
  )
)

# Ao terminar a Tarefa 7 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 7" e envie para o repositório Projeto_BDEM_2016


# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# Criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.


# Ao terminar a Tarefa 8 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 8" e envie para o repositório Projeto_BDEM_2016


# Tarefa 9. Criar um banco de dados, de nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 9 - SINASC.pdf”
# Atenção: a ordem das variáveis do arquivo deve ser respeitada


# Ao terminar a Tarefa 9 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 9" e envie para o repositório Projeto_BDEM_2016


# Tarefa 10. Exportar o banco de dados com o nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv)
# Ao terminar a Tarefa 10 commit com o comentário "dados SINASC_UF 2016 e script - SIM - tarefas 1 a 10"  e envie para o repositório Projeto_BDEM_2016



####################################
# ETAPA 3: BANCOS DE DADOS DO SIDRA
####################################
# Você deve criar e estar na branch SIDRA antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler os bancos de dados abaixo listados com os respectivos nomes
# dados_sidra_1 para população residente estimada - UF e municípios - 2016 - SIDRA - tabela_6579.csv
# dados_sidra_2 para população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv
# dados_sidra_3 para população residente censo 2010 - por faixa etária - UF - SIDRA - tabela_1552.csv
# dados_sidra_4 para população residente censo 2010 - por faixa etária e sexo - municípios - SIDRA - tabela_1552.csv
# Atenção que agora os arquivos têm nomes e códigos (com 7 dígitos) dos municípios (e alguns UF)

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados


# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SIDRA - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Criar uma nova variável de nome CODUF com os códigos da UF nos bancos dados_sidra_1, dados_sidra_2, dados_sidra_4


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Selecionar em dados_sidra_ 1 a dados_sidra_4 a UF de responsabilidade do aluno 
# e chamar os bancos de dados, respectivamente por sidra_1, sidra_2, sidra_3 e sidra_4


# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4: Criar um banco de dados, de nome SIDRA_UF.csv (Exemplo: SIDRA_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 4 - SIDRA.pdf”

# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5:Exportar o banco de dados com o nome SIDRA_UF.csv (Exemplo: SIDRA_RJ.csv)
# Ao terminar a Tarefa 5 commit com o comentário "dados SIDRA_UF 2016 e script - SIDRA - tarefas 1 a 5"  e envie para o repositório Projeto_BDEM_2016


####################################
# ETAPA 4: BANCOS DE DADOS DO ATLAS
####################################
# Você deve criar e estar na branch ATLAS antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler os bancos de dados abaixo listados com os respectivos nomes
# codigos_IBGE_2010 para códigos dos municípios - 2010.csv
# dados_atlas_1 para IDHM - 2010 (CENSO) e 2016 (PNAD) - total e por sexo - UF - Atlas Brasil.csv
# dados_atlas_2 para IDHM - 2010 - municípios - Atlas Brasil.csv
# Atenção que agora alguns arquivos só têm os nomes dos municípios e das UFs, mas não têm os códigos

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - ATLAS - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2: Manipular o banco de dados e criar o banco de dados ATLAS_UF

# Criar o banco UF_codigo tipo tabela de correspondência
UF_codigo = data.frame(
  UF = c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins",
         "Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
         "Pernambuco","Alagoas","Sergipe","Bahia","Minas Gerais",
         "Espírito Santo","Rio de Janeiro","São Paulo","Paraná",
         "Santa Catarina","Rio Grande do Sul","Mato Grosso do Sul",
         "Mato Grosso","Goiás","Distrito Federal"),
  
  SIGLA = c("RO","AC","AM","RR","PA","AP","TO",
            "MA","PI","CE","RN","PB","PE","AL",
            "SE","BA","MG","ES","RJ","SP",
            "PR","SC","RS","MS","MT","GO","DF"),
  
  CODUF = c(11,12,13,14,15,16,17,
            21,22,23,24,25,26,27,
            28,29,31,32,33,35,
            41,42,43,50,51,52,53)
)

# Retirar de dados_atlas_1 a linha do Brasil e adicionar (com merge by UF) as colunas de UF_codigo

# Criar o banco linha_estado somente com as linhas da UF e com as seguintes colunas:
# ANO=2016, NIVEL=UF, CODMUNRES, IDHM_A, IDHM_CA, IDHM_CA_M e IDHM_CA_F 

# Selecionar de linha_estado a UF da responsabilidade do aluno por CODMUNRES

# Criar em dados_atlas_2 a coluna com UF

# Retirar (UF) da variável município

# Acrescentar em codigos_IBGE_2010 a variável CODUF baseado nos dois primeiros dígitos de CODMUNRES

# Acrescentar a codigos_IBGE_2010 as variáveis de UF_codigo (merge by CODUF)

# Associar dados_atlas_2 a codigos_IBGE_2010 e nomear o novo arquivo por atlas_municipio
# Neste caso o merge será by.x = c("município","UF") e by.y = c("município","SIGLA")

# Remover de atlas_municipio a coluna UF.y criada no merge

# Selecionar somente a UF de responsabilidade do aluno através dos dois primeiros dógitos de CODMUNRES

# Criar banco ATLAS_MUNICIPIO com as linhas dos municípios e com as seguintes variáveis:
# ANO=2016, NIVEL=MUNICIPIO, CODMUNRES, IDHM_A=NA, IDHM_CA, IDHM_CA_M=NA, IDHM_CA_F=NA

# Criar banco final ATLAS_UF "juntando" os bancos linha_estado e ATLAS_MUNICIPIO


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - ATLAS - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Exportar o banco de dados com o nome ATLAS_UF.csv (Exemplo: ATLAS_RJ.csv)
# Ao terminar a Tarefa 3 commit com o comentário "dados ATLAS_UF 2016 e script - ATLAS - tarefas 1 a 3"  e envie para o repositório Projeto_BDEM_2016



####################################
# ETAPA 5: BANCOS DE DADOS DO SINISA
####################################
# Você deve criar e estar na branch SINISA antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler o bancos de dados abaixo listado com os respectivo nome
# dados_sinisa para agua e esgoto - município - 2016.csv
# Atenção que o arquivo tem códigos e nomes de municípios e muitos NAs. 
# Repare que os valores estão com o milhar indicado por ponto, o que não deve acontecer para o R não entender como decimal

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados
# Remover a pontuação de milhar e converter para formato numérico

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SINISA - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Reduzir dados_sinisa apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinisa_1

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SINISA - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Criar um banco de dados, de nome SINISA_UF.csv (Exemplo: SINISA_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 3 - SINISA.pdf”

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SINISA - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Exportar o banco de dados com o nome SINISA_UF.csv (Exemplo: SINISA_RJ.csv)
# Ao terminar a Tarefa 4 commit com o comentário "dados SINISA_UF 2016 e script - SINISA - tarefas 1 a 4"  e enviar para o repositório Projeto_BDEM_2016



################################
# ETAPA 6: CRIAÇÃO DE BDEM_UF
################################
# Você deve estar agora em main e antes de inserir qualquer comando desta ETAPA
# deverá fazer os merges de cada uma das 5 branches. A cada merge pode fazer o comentário "merge da branch TAL"
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Agregar os arquivos SIDRA_UF, ATLAS_UF, SINASC_UF, SIM_UF, SINISA_UF no banco BDEM_UF (Exemplo: BDEM_RJ)
# Leitura dos 5 bancos de dados expeortados das etapas anteriores

# Agregação dos bancos
# Lembre-se que SIDRA e ATLAS tem CODMUNRES com 7 dígitos e SINASC, SIM e SINISA com 6 dígitos
# Além disso dentro do merge all = TRUE garante a manutenção de qualquer município presente em um dos bancos envolvidos no merge


# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - BDEM - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2: Inserir os seguintes indicadores epidemiológicos (com apenas dias casas decimais) no BDEM_UF:
# TFG: Taxa de fecundidade geral
# TMG: Taxa de mortalidade geral
# RMM: Razão de mortalidade materna
# TMM: Taxa de mortalidade materna
# TMM_P: Taxa de mortalidade materna em até 42 dias
# TMN: Taxa de mortalidade neonatal
# TMN_P: Taxa de mortalidade neonatal precoce
# TMN_T: Taxa de mortalidade neonatal tardia
# TMI: Taxa de mortalidade infantil

# Conferir o banco BDEM_UF após inserção dos indicadores

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - BDEM - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3: Exportar o banco de dados com o nome BDEM_UF.csv (Exemplo: BDEM_RJ.csv)
# Ao terminar a Tarefa 3 commit com o comentário "dados BDEM_UF 2016 e script - BDEM - tarefas 1 a 3"  e enviar para o repositório Projeto_BDEM_2016
 