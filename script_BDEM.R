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

# Tarefa 7. Criar um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 - SIM.pdf”
# Atenção: a ordem das variáveis do arquivo deve ser respeitada 

LETRA = substr(dados_sim_2$CAUSABAS, 1, 1)
NUM = as.numeric(substr(dados_sim_2$CAUSABAS, 2, 3))
<<<<<<< HEAD


# Base inicial (municípios)
base = data.frame(CODMUNRES = sort(unique(dados_sim_2$CODMUNRES)))


# TO - Total de óbitos
TO = as.data.frame(table(factor(dados_sim_2$CODMUNRES)))

names(TO) = c("CODMUNRES","TO")

base = merge(base, TO, by = "CODMUNRES", all.x = TRUE)


# TORC - Registros completos nas 87 variáveis
dados_UF = dados_sim[substr(as.character(dados_sim$CODMUNRES),1,2) == "25",]
=======
>>>>>>> SINASC

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

# Ler a tabela PIG
Tabela_PIG_Brasil = read.csv(
  "Tabela_PIG_Brasil.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Verificar a tabela
str(Tabela_PIG_Brasil)
head(Tabela_PIG_Brasil)


# Localizar P10 e P90 de acordo com idade gestacional e sexo
indice_PIG = match(
  paste(dados_sinasc_2$SEMAGESTAC,
        as.character(dados_sinasc_2$SEXO)),
  paste(Tabela_PIG_Brasil$SEMAGESTAC,
        Tabela_PIG_Brasil$SEXO)
)

dados_sinasc_2$PESO_P10 = Tabela_PIG_Brasil$PESO_P10[indice_PIG]

dados_sinasc_2$PESO_P90 = Tabela_PIG_Brasil$PESO_P90[indice_PIG]


# Criar F_PIG
dados_sinasc_2$F_PIG = ifelse(
  is.na(dados_sinasc_2$SEMAGESTAC) |
    is.na(dados_sinasc_2$PESO) |
    is.na(dados_sinasc_2$SEXO) |
    is.na(dados_sinasc_2$PESO_P10) |
    is.na(dados_sinasc_2$PESO_P90) |
    dados_sinasc_2$GRAVIDEZ != "Única",
  NA,
  ifelse(
    dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10,
    "PIG",
    ifelse(
      dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90,
      "AIG",
      "GIG"
    )
  )
)

dados_sinasc_2$F_PIG = factor(
  dados_sinasc_2$F_PIG,
  levels = c("PIG", "AIG", "GIG")
)


# Verificando
table(dados_sinasc_2$F_PIG, useNA = "ifany")

# Ao terminar a Tarefa 8 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 8" e envie para o repositório Projeto_BDEM_2016


# Tarefa 9. Criar um banco de dados, de nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 9 - SINASC.pdf”
# Atenção: a ordem das variáveis do arquivo deve ser respeitada


municipios = sort(unique(dados_sinasc_2$CODMUNRES))

g = factor(
  dados_sinasc_2$CODMUNRES,
  levels = municipios
)

base = data.frame(
  CODMUNRES = municipios
)

# FUNÇÃO PARA CONTAGEM POR MUNICÍPIO


contar = function(condicao) {
  
  if (length(condicao) == 1) {
    condicao = rep(condicao, nrow(dados_sinasc_2))
  }
  
  condicao[is.na(condicao)] = FALSE
  
  resultado = tapply(
    as.integer(condicao),
    g,
    sum
  )
  
  as.numeric(resultado)
}

# FUNÇÃO PARA ESTATÍSTICAS POR MUNICÍPIO


estatistica = function(x, funcao) {
  
  resultado = tapply(
    x,
    g,
    function(z) {
      
      if (all(is.na(z))) {
        return(NA)
      }
      
      funcao(z, na.rm = TRUE)
      
    }
  )
  
  as.numeric(resultado)
}


# INFORMAÇÕES SOBRE OS NASCIMENTOS

# 4 TN - Total de nascimentos

base$TN = contar(TRUE)


# 5 TNRC - Total de nascimentos com registros completos
# nas 61 variáveis do SINASC

dados_sinasc_UF = dados_sinasc[
  substr(
    as.character(dados_sinasc$CODMUNRES),
    1,
    2
  ) == "25",
]

dados_sinasc_UF_comp = dados_sinasc_UF[
  complete.cases(dados_sinasc_UF),
]

base$TNRC = contar(
  dados_sinasc_2$CONTADOR %in%
    dados_sinasc_UF_comp$CONTADOR
)


# 6 TNRCR - Total de nascimentos com registros completos
# nas variáveis selecionadas

variaveis_TNRCR = c(
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

dados_TNRCR = dados_sinasc_2[
  complete.cases(
    dados_sinasc_2[, variaveis_TNRCR]
  ),
]

base$TNRCR = contar(
  dados_sinasc_2$CONTADOR %in%
    dados_TNRCR$CONTADOR
)

# 7 TGI_15 - idade inferior a 15 anos

base$TGI_15 = contar(
  dados_sinasc_2$IDADEMAE < 15
)


# 8 TGI_15_19

base$TGI_15_19 = contar(
  dados_sinasc_2$IDADEMAE >= 15 &
    dados_sinasc_2$IDADEMAE <= 19
)


# 9 TGI_20_24

base$TGI_20_24 = contar(
  dados_sinasc_2$IDADEMAE >= 20 &
    dados_sinasc_2$IDADEMAE <= 24
)


# 10 TGI_25_29

base$TGI_25_29 = contar(
  dados_sinasc_2$IDADEMAE >= 25 &
    dados_sinasc_2$IDADEMAE <= 29
)


# 11 TGI_30_34

base$TGI_30_34 = contar(
  dados_sinasc_2$IDADEMAE >= 30 &
    dados_sinasc_2$IDADEMAE <= 34
)


# 12 TGI_35_39

base$TGI_35_39 = contar(
  dados_sinasc_2$IDADEMAE >= 35 &
    dados_sinasc_2$IDADEMAE <= 39
)


# 13 TGI_40_44

base$TGI_40_44 = contar(
  dados_sinasc_2$IDADEMAE >= 40 &
    dados_sinasc_2$IDADEMAE <= 44
)


# 14 TGI_45_49

base$TGI_45_49 = contar(
  dados_sinasc_2$IDADEMAE >= 45 &
    dados_sinasc_2$IDADEMAE <= 49
)


# 15 TGI_50

base$TGI_50 = contar(
  dados_sinasc_2$IDADEMAE >= 50
)


# 16 TGIF - idade fértil (15 a 49 anos)

base$TGIF = contar(
  dados_sinasc_2$IDADEMAE >= 15 &
    dados_sinasc_2$IDADEMAE <= 49
)


# 17 IM_P25 - percentil 25 da idade materna

base$IM_P25 = estatistica(
  as.numeric(dados_sinasc_2$IDADEMAE),
  function(x, na.rm) {
    quantile(x, 0.25, na.rm = na.rm)
  }
)


# 18 IM_P50

base$IM_P50 = estatistica(
  as.numeric(dados_sinasc_2$IDADEMAE),
  function(x, na.rm) {
    quantile(x, 0.50, na.rm = na.rm)
  }
)


# 19 IM_P75

base$IM_P75 = estatistica(
  as.numeric(dados_sinasc_2$IDADEMAE),
  function(x, na.rm) {
    quantile(x, 0.75, na.rm = na.rm)
  }
)


# 20 IM_MD - idade média materna

base$IM_MD = estatistica(
  as.numeric(dados_sinasc_2$IDADEMAE),
  mean
)


# 21 IM_DP - desvio-padrão da idade materna

base$IM_DP = estatistica(
  as.numeric(dados_sinasc_2$IDADEMAE),
  sd
)


# 22 EM_S - sem escolaridade

base$EM_S = contar(
  dados_sinasc_2$ESCMAE2010 == "Sem escolaridade"
)


# 23 EM_FI - fundamental I

base$EM_FI = contar(
  dados_sinasc_2$ESCMAE2010 == "Fundamental I"
)


# 24 EM_FII - fundamental II

base$EM_FII = contar(
  dados_sinasc_2$ESCMAE2010 == "Fundamental II"
)


# 25 EM_M - médio

base$EM_M = contar(
  dados_sinasc_2$ESCMAE2010 == "Médio"
)


# 26 EM_SI - superior incompleto

base$EM_SI = contar(
  dados_sinasc_2$ESCMAE2010 == "Superior incompleto"
)


# 27 EM_SC - superior completo

base$EM_SC = contar(
  dados_sinasc_2$ESCMAE2010 == "Superior completo"
)


# 28 TGRC_B - raça/cor branca

base$TGRC_B = contar(
  dados_sinasc_2$RACACOR == "Branca"
)


# 29 TGRC_PT - raça/cor preta

base$TGRC_PT = contar(
  dados_sinasc_2$RACACOR == "Preta"
)


# 30 TGRC_A - raça/cor amarela

base$TGRC_A = contar(
  dados_sinasc_2$RACACOR == "Amarela"
)


# 31 TGRC_PD - raça/cor parda

base$TGRC_PD = contar(
  dados_sinasc_2$RACACOR == "Parda"
)


# 32 TGRC_I - raça/cor indígena

base$TGRC_I = contar(
  dados_sinasc_2$RACACOR == "Indígena"
)


# 33 TGSC - sem companheiro

base$TGSC = contar(
  dados_sinasc_2$ESTCIV == "Sem companheiro"
)


# 34 TGCC - com companheiro

base$TGCC = contar(
  dados_sinasc_2$ESTCIV == "Com companheiro"
)


# 35 TGPRI - primíparas

base$TGPRI = contar(
  dados_sinasc_2$PARIDADE == "Nulípara"
)


# 36 TGNPRI - não primíparas

base$TGNPRI = contar(
  dados_sinasc_2$PARIDADE == "Multípara"
)

# INFORMAÇÕES SOBRE AS GESTAÇÕES

# 37 TGU - gestações únicas

base$TGU = contar(
  dados_sinasc_2$GRAVIDEZ == "Única"
)


# 38 TGG - gestações gemelares

base$TGG = contar(
  dados_sinasc_2$GRAVIDEZ %in%
    c(
      "Dupla",
      "Tripla ou mais"
    )
)


# 39 TGD_22 - menos de 22 semanas

base$TGD_22 = contar(
  dados_sinasc_2$SEMAGESTAC < 22
)


# 40 TGD_22_27

base$TGD_22_27 = contar(
  dados_sinasc_2$SEMAGESTAC >= 22 &
    dados_sinasc_2$SEMAGESTAC <= 27
)


# 41 TGD_28_31

base$TGD_28_31 = contar(
  dados_sinasc_2$SEMAGESTAC >= 28 &
    dados_sinasc_2$SEMAGESTAC <= 31
)


# 42 TGD_32_36

base$TGD_32_36 = contar(
  dados_sinasc_2$SEMAGESTAC >= 32 &
    dados_sinasc_2$SEMAGESTAC <= 36
)


# 43 TGD_37_41

base$TGD_37_41 = contar(
  dados_sinasc_2$SEMAGESTAC >= 37 &
    dados_sinasc_2$SEMAGESTAC <= 41
)


# 44 TGD_42

base$TGD_42 = contar(
  dados_sinasc_2$SEMAGESTAC >= 42
)


# 45 TGD_PRT - pré-termo

base$TGD_PRT = contar(
  dados_sinasc_2$SEMAGESTAC < 37
)


# 46 TGD_AT - a termo

base$TGD_AT = contar(
  dados_sinasc_2$SEMAGESTAC >= 37 &
    dados_sinasc_2$SEMAGESTAC <= 41
)


# 47 TGD_PST - pós-termo

base$TGD_PST = contar(
  dados_sinasc_2$SEMAGESTAC >= 42
)


# 48 DG_P25

base$DG_P25 = estatistica(
  as.numeric(dados_sinasc_2$SEMAGESTAC),
  function(x, na.rm) {
    quantile(x, 0.25, na.rm = na.rm)
  }
)


# 49 DG_P50

base$DG_P50 = estatistica(
  as.numeric(dados_sinasc_2$SEMAGESTAC),
  function(x, na.rm) {
    quantile(x, 0.50, na.rm = na.rm)
  }
)


# 50 DG_P75

base$DG_P75 = estatistica(
  as.numeric(dados_sinasc_2$SEMAGESTAC),
  function(x, na.rm) {
    quantile(x, 0.75, na.rm = na.rm)
  }
)


# 51 DG_MD

base$DG_MD = estatistica(
  as.numeric(dados_sinasc_2$SEMAGESTAC),
  mean
)


# 52 DG_DP

base$DG_DP = estatistica(
  as.numeric(dados_sinasc_2$SEMAGESTAC),
  sd
)


# 53 TKC_NR - não realizou pré-natal

base$TKC_NR = contar(
  dados_sinasc_2$KOTELCHUCK ==
    "Não realizou pré-natal"
)


# 54 TKC_ID - pré-natal inadequado

base$TKC_ID = contar(
  dados_sinasc_2$KOTELCHUCK ==
    "Inadequado"
)


# 55 TKC_IT - pré-natal intermediário

base$TKC_IT = contar(
  dados_sinasc_2$KOTELCHUCK ==
    "Intermediário"
)


# 56 TKC_AD - pré-natal adequado

base$TKC_AD = contar(
  dados_sinasc_2$KOTELCHUCK ==
    "Adequado"
)


# 57 TKC_MAD - mais que adequado

base$TKC_MAD = contar(
  dados_sinasc_2$KOTELCHUCK ==
    "Mais que adequado"
)

# 58 TGPRG_S - peregrinaram

base$TGPRG_S = contar(
  dados_sinasc_2$PEREG == "Sim"
)


# 59 TGPRG_N - não peregrinaram

base$TGPRG_N = contar(
  dados_sinasc_2$PEREG == "Não"
)


# 60 TPV - parto vaginal

base$TPV = contar(
  dados_sinasc_2$PARTO == "Vaginal"
)


# 61 TPC - parto cesáreo

base$TPC = contar(
  dados_sinasc_2$PARTO == "Cesáreo"
)


# 62 TRAP_C - apresentação cefálica

base$TRAP_C = contar(
  dados_sinasc_2$TPAPRESENT == "Cefálico"
)


# 63 TRAP_P - apresentação pélvica ou podálica

base$TRAP_P = contar(
  dados_sinasc_2$TPAPRESENT ==
    "Pélvica ou podálica"
)


# 64 TRAP_T - apresentação transversa

base$TRAP_T = contar(
  dados_sinasc_2$TPAPRESENT == "Transversa"
)


# 65 TGROB_1

base$TGROB_1 = contar(
  dados_sinasc_2$TPROBSON == "Grupo de Robson 1"
)


# 66 TGROB_2

base$TGROB_2 = contar(
  dados_sinasc_2$TPROBSON == "Grupo de Robson 2"
)


# 67 TGROB_3

base$TGROB_3 = contar(
  dados_sinasc_2$TPROBSON == "Grupo de Robson 3"
)


# 68 TGROB_4

base$TGROB_4 = contar(
  dados_sinasc_2$TPROBSON == "Grupo de Robson 4"
)


# 69 TGROB_5

base$TGROB_5 = contar(
  dados_sinasc_2$TPROBSON == "Grupo de Robson 5"
)


# 70 TGROB_6

base$TGROB_6 = contar(
  dados_sinasc_2$TPROBSON == "Grupo de Robson 6"
)


# 71 TGROB_7

base$TGROB_7 = contar(
  dados_sinasc_2$TPROBSON == "Grupo de Robson 7"
)


# 72 TGROB_8

base$TGROB_8 = contar(
  dados_sinasc_2$TPROBSON == "Grupo de Robson 8"
)


# 73 TGROB_9

base$TGROB_9 = contar(
  dados_sinasc_2$TPROBSON == "Grupo de Robson 9"
)


# 74 TGROB_10

base$TGROB_10 = contar(
  dados_sinasc_2$TPROBSON == "Grupo de Robson 10"
)


# 75 TNLOC_H - hospital

base$TNLOC_H = contar(
  dados_sinasc_2$LOCNASC == "Hospital"
)


# 76 TNLOC_ES - outros estabelecimentos de saúde

base$TNLOC_ES = contar(
  dados_sinasc_2$LOCNASC ==
    "Outros estabelecimentos de saúde"
)


# 77 TNLOC_D - domicílio

base$TNLOC_D = contar(
  dados_sinasc_2$LOCNASC == "Domicílio"
)


# 78 TNLOC_O - outros

base$TNLOC_O = contar(
  dados_sinasc_2$LOCNASC == "Outros"
)


# 79 TNLOC_AI - aldeia indígena

base$TNLOC_AI = contar(
  dados_sinasc_2$LOCNASC == "Aldeia indígena"
)

# 80 TRS_M - sexo masculino

base$TRS_M = contar(
  dados_sinasc_2$SEXO == "Masculino"
)


# 81 TRS_F - sexo feminino

base$TRS_F = contar(
  dados_sinasc_2$SEXO == "Feminino"
)


# 82 TRRC_B - branca

base$TRRC_B = contar(
  dados_sinasc_2$RACACOR == "Branca"
)


# 83 TRRC_PT - preta

base$TRRC_PT = contar(
  dados_sinasc_2$RACACOR == "Preta"
)


# 84 TRRC_A - amarela

base$TRRC_A = contar(
  dados_sinasc_2$RACACOR == "Amarela"
)


# 85 TRRC_PD - parda

base$TRRC_PD = contar(
  dados_sinasc_2$RACACOR == "Parda"
)


# 86 TRRC_I - indígena

base$TRRC_I = contar(
  dados_sinasc_2$RACACOR == "Indígena"
)


# 87 TRP_BP - baixo peso

base$TRP_BP = contar(
  as.numeric(dados_sinasc_2$PESO) < 2500
)


# 88 TRP_N - peso normal

base$TRP_N = contar(
  as.numeric(dados_sinasc_2$PESO) >= 2500 &
    as.numeric(dados_sinasc_2$PESO) < 4000
)


# 89 TRP_M - macrossomia

base$TRP_M = contar(
  as.numeric(dados_sinasc_2$PESO) >= 4000
)


# 90 PESO_P25

base$PESO_P25 = estatistica(
  as.numeric(dados_sinasc_2$PESO),
  function(x, na.rm) {
    quantile(x, 0.25, na.rm = na.rm)
  }
)


# 91 PESO_P50

base$PESO_P50 = estatistica(
  as.numeric(dados_sinasc_2$PESO),
  function(x, na.rm) {
    quantile(x, 0.50, na.rm = na.rm)
  }
)


# 92 PESO_P75

base$PESO_P75 = estatistica(
  as.numeric(dados_sinasc_2$PESO),
  function(x, na.rm) {
    quantile(x, 0.75, na.rm = na.rm)
  }
)


# 93 PESO_MD

base$PESO_MD = estatistica(
  as.numeric(dados_sinasc_2$PESO),
  mean
)


# 94 PESO_DP

base$PESO_DP = estatistica(
  as.numeric(dados_sinasc_2$PESO),
  sd
)


# 95 TRPIG_P - PIG em gestações únicas

base$TRPIG_P = contar(
  dados_sinasc_2$GRAVIDEZ == "Única" &
    dados_sinasc_2$F_PIG == "PIG"
)


# 96 TRPIG_A - AIG em gestações únicas

base$TRPIG_A = contar(
  dados_sinasc_2$GRAVIDEZ == "Única" &
    dados_sinasc_2$F_PIG == "AIG"
)


# 97 TRPIG_G - GIG em gestações únicas

base$TRPIG_G = contar(
  dados_sinasc_2$GRAVIDEZ == "Única" &
    dados_sinasc_2$F_PIG == "GIG"
)


# 98 TRAPG5_B - Apgar5 baixo

base$TRAPG5_B = contar(
  dados_sinasc_2$F_APGAR5 == "Baixo"
)


# 99 TRAPG5_N - Apgar5 normal

base$TRAPG5_N = contar(
  dados_sinasc_2$F_APGAR5 == "Normal"
)


# 100 APG5_MD - média do Apgar5

base$APG5_MD = estatistica(
  as.numeric(dados_sinasc_2$APGAR5),
  mean
)


# 101 APG5_DP - desvio-padrão do Apgar5

base$APG5_DP = estatistica(
  as.numeric(dados_sinasc_2$APGAR5),
  sd
)


# 102 TRAC - com anomalia congênita

base$TRAC = contar(
  dados_sinasc_2$IDANOMAL == "Sim"
)


# 103 TRSAC - sem anomalia congênita

base$TRSAC = contar(
  dados_sinasc_2$IDANOMAL == "Não"
)


# ============================================================
# CRIAR A LINHA DA UF - PARAÍBA
# ============================================================

linha_estado = data.frame(
  CODMUNRES = "25"
)


# ============================================================
# SOMAR OS INDICADORES DE CONTAGEM
# ============================================================

variaveis_contagem = c(
  "TN",
  "TNRC",
  "TNRCR",
  "TGI_15",
  "TGI_15_19",
  "TGI_20_24",
  "TGI_25_29",
  "TGI_30_34",
  "TGI_35_39",
  "TGI_40_44",
  "TGI_45_49",
  "TGI_50",
  "TGIF",
  "EM_S",
  "EM_FI",
  "EM_FII",
  "EM_M",
  "EM_SI",
  "EM_SC",
  "TGRC_B",
  "TGRC_PT",
  "TGRC_A",
  "TGRC_PD",
  "TGRC_I",
  "TGSC",
  "TGCC",
  "TGPRI",
  "TGNPRI",
  "TGU",
  "TGG",
  "TGD_22",
  "TGD_22_27",
  "TGD_28_31",
  "TGD_32_36",
  "TGD_37_41",
  "TGD_42",
  "TGD_PRT",
  "TGD_AT",
  "TGD_PST",
  "TKC_NR",
  "TKC_ID",
  "TKC_IT",
  "TKC_AD",
  "TKC_MAD",
  "TGPRG_S",
  "TGPRG_N",
  "TPV",
  "TPC",
  "TRAP_C",
  "TRAP_P",
  "TRAP_T",
  "TGROB_1",
  "TGROB_2",
  "TGROB_3",
  "TGROB_4",
  "TGROB_5",
  "TGROB_6",
  "TGROB_7",
  "TGROB_8",
  "TGROB_9",
  "TGROB_10",
  "TNLOC_H",
  "TNLOC_ES",
  "TNLOC_D",
  "TNLOC_O",
  "TNLOC_AI",
  "TRS_M",
  "TRS_F",
  "TRRC_B",
  "TRRC_PT",
  "TRRC_A",
  "TRRC_PD",
  "TRRC_I",
  "TRP_BP",
  "TRP_N",
  "TRP_M",
  "TRPIG_P",
  "TRPIG_A",
  "TRPIG_G",
  "TRAPG5_B",
  "TRAPG5_N",
  "TRAC",
  "TRSAC"
)


for (variavel in variaveis_contagem) {
  
  linha_estado[[variavel]] =
    sum(
      base[[variavel]],
      na.rm = TRUE
    )
  
}


linha_estado$IM_P25 = quantile(
  as.numeric(dados_sinasc_2$IDADEMAE),
  0.25,
  na.rm = TRUE
)

linha_estado$IM_P50 = quantile(
  as.numeric(dados_sinasc_2$IDADEMAE),
  0.50,
  na.rm = TRUE
)

linha_estado$IM_P75 = quantile(
  as.numeric(dados_sinasc_2$IDADEMAE),
  0.75,
  na.rm = TRUE
)

linha_estado$IM_MD = mean(
  as.numeric(dados_sinasc_2$IDADEMAE),
  na.rm = TRUE
)

linha_estado$IM_DP = sd(
  as.numeric(dados_sinasc_2$IDADEMAE),
  na.rm = TRUE
)

linha_estado$DG_P25 = quantile(
  as.numeric(dados_sinasc_2$SEMAGESTAC),
  0.25,
  na.rm = TRUE
)

linha_estado$DG_P50 = quantile(
  as.numeric(dados_sinasc_2$SEMAGESTAC),
  0.50,
  na.rm = TRUE
)

linha_estado$DG_P75 = quantile(
  as.numeric(dados_sinasc_2$SEMAGESTAC),
  0.75,
  na.rm = TRUE
)

linha_estado$DG_MD = mean(
  as.numeric(dados_sinasc_2$SEMAGESTAC),
  na.rm = TRUE
)

linha_estado$DG_DP = sd(
  as.numeric(dados_sinasc_2$SEMAGESTAC),
  na.rm = TRUE
)

linha_estado$PESO_P25 = quantile(
  as.numeric(dados_sinasc_2$PESO),
  0.25,
  na.rm = TRUE
)

linha_estado$PESO_P50 = quantile(
  as.numeric(dados_sinasc_2$PESO),
  0.50,
  na.rm = TRUE
)

linha_estado$PESO_P75 = quantile(
  as.numeric(dados_sinasc_2$PESO),
  0.75,
  na.rm = TRUE
)

linha_estado$PESO_MD = mean(
  as.numeric(dados_sinasc_2$PESO),
  na.rm = TRUE
)

linha_estado$PESO_DP = sd(
  as.numeric(dados_sinasc_2$PESO),
  na.rm = TRUE
)



linha_estado$APG5_MD = mean(
  as.numeric(dados_sinasc_2$APGAR5),
  na.rm = TRUE
)

linha_estado$APG5_DP = sd(
  as.numeric(dados_sinasc_2$APGAR5),
  na.rm = TRUE
)


SINASC_UF = rbind(
  linha_estado,
  base
)


SINASC_UF$NIVEL = c(
  "UF",
  rep(
    "MUNICIPIO",
    nrow(SINASC_UF) - 1
  )
)

SINASC_UF$ANO = 2016

SINASC_UF = SINASC_UF[
  ,
  c(
    "ANO",
    "NIVEL",
    "CODMUNRES",
    
    "TN",
    "TNRC",
    "TNRCR",
    
    "TGI_15",
    "TGI_15_19",
    "TGI_20_24",
    "TGI_25_29",
    "TGI_30_34",
    "TGI_35_39",
    "TGI_40_44",
    "TGI_45_49",
    "TGI_50",
    "TGIF",
    
    "IM_P25",
    "IM_P50",
    "IM_P75",
    "IM_MD",
    "IM_DP",
    
    "EM_S",
    "EM_FI",
    "EM_FII",
    "EM_M",
    "EM_SI",
    "EM_SC",
    
    "TGRC_B",
    "TGRC_PT",
    "TGRC_A",
    "TGRC_PD",
    "TGRC_I",
    
    "TGSC",
    "TGCC",
    
    "TGPRI",
    "TGNPRI",
    
    "TGU",
    "TGG",
    
    "TGD_22",
    "TGD_22_27",
    "TGD_28_31",
    "TGD_32_36",
    "TGD_37_41",
    "TGD_42",
    "TGD_PRT",
    "TGD_AT",
    "TGD_PST",
    
    "DG_P25",
    "DG_P50",
    "DG_P75",
    "DG_MD",
    "DG_DP",
    
    "TKC_NR",
    "TKC_ID",
    "TKC_IT",
    "TKC_AD",
    "TKC_MAD",
    
    "TGPRG_S",
    "TGPRG_N",
    
    "TPV",
    "TPC",
    
    "TRAP_C",
    "TRAP_P",
    "TRAP_T",
    
    "TGROB_1",
    "TGROB_2",
    "TGROB_3",
    "TGROB_4",
    "TGROB_5",
    "TGROB_6",
    "TGROB_7",
    "TGROB_8",
    "TGROB_9",
    "TGROB_10",
    
    "TNLOC_H",
    "TNLOC_ES",
    "TNLOC_D",
    "TNLOC_O",
    "TNLOC_AI",
    
    "TRS_M",
    "TRS_F",
    
    "TRRC_B",
    "TRRC_PT",
    "TRRC_A",
    "TRRC_PD",
    "TRRC_I",
    
    "TRP_BP",
    "TRP_N",
    "TRP_M",
    
    "PESO_P25",
    "PESO_P50",
    "PESO_P75",
    "PESO_MD",
    "PESO_DP",
    
    "TRPIG_P",
    "TRPIG_A",
    "TRPIG_G",
    
    "TRAPG5_B",
    "TRAPG5_N",
    "APG5_MD",
    "APG5_DP",
    
    "TRAC",
    "TRSAC"
  )
]

# CODMUNRES como character

SINASC_UF$CODMUNRES =
  as.character(SINASC_UF$CODMUNRES)

# CONFERÊNCIAS


dim(SINASC_UF)
names(SINASC_UF)
str(SINASC_UF)
head(SINASC_UF)

# Ao terminar a Tarefa 9 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 9" e envie para o repositório Projeto_BDEM_2016

# Tarefa 10. Exportar o banco de dados com o nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv)

write.csv(
  SINASC_UF,
  "SINASC_PB.csv",
  row.names = FALSE
)

# Ao terminar a Tarefa 10 commit com o comentário "dados SINASC_UF 2016 e script - SINASC - tarefas 1 a 10" e envie para o repositório Projeto_BDEM_2016

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

dados_sidra_1 = read.csv(
  "população residente estimada - UF e municípios - 2016 - SIDRA - tabela_6579.csv",
  sep = ";",
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

dados_sidra_2 = read.csv(
  "população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv",
  sep = ";",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

dados_sidra_3 = read.csv(
  "população residente censo 2010 - por faixa etária - UF - SIDRA - tabela_1552.csv",
  sep = ";",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

dados_sidra_4 = read.csv(
  "população residente censo 2010 - por faixa etária e sexo - municípios - SIDRA - tabela_1552.csv",
  sep = ";",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

str(dados_sidra_1)
str(dados_sidra_2)
str(dados_sidra_3)
str(dados_sidra_4)
head(dados_sidra_1)
head(dados_sidra_2)
head(dados_sidra_3)
head(dados_sidra_4)

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
 