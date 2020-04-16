

rm(list = ls())
com <-Sys.time()
## mundando o diret?rio
#setwd("X:/SGE/GABINETE/CONSELHO NACIONAL DE JUSTICA/JUSTICA EM NUMEROS/JUSTI?A EM N?MEROS_DADOS ANUAIS/JN ANO 2020/Arquivos Provimento 49 de 18_08_2015/Quarto Passo/Gerar quarto passo")

# ---------------------------- #
# Valores a ser alterados
# ---------------------------- #
# Valores atuais (m?s)
dia_inicio_atual=01
dia_fim_atual=31
mes_atual=03
ano_atual=2020
nome_mes_atual="Março"

#pacotes utilizados 
library(dplyr) #manipula??o de dados
library(readxl) #leitura de arquivo xls
#library(excel.link) #criando arquivo em excel
library(lubridate) # manipula??o de datas
library(stringi)  # manipula??o de strings

setwd("/home/silva/Downloads/romi_ofice/Gerar_passo_4")
# Planilha dos c?digos de serventia
# codigos_serventia=read_excel("Codigo_serventia.xls")
BD_serventias=read_excel("/home/silva/Downloads/romi_ofice/Gerar_passo_4/tables_fourth/BD serventias.xls") # Arquivo ?nico
# retirando os caracteres especias
BD_serventias$nome_serventia_sicond <- stri_trans_general(BD_serventias$nome_serventia_sicond, "Latin-ASCII")
BD_serventias$nome_serventia_egestao <- stri_trans_general(BD_serventias$nome_serventia_egestao, "Latin-ASCII")
BD_serventias$nome_serventia_desig  <- stri_trans_general(BD_serventias$nome_serventia_desig, "Latin-ASCII")


# Planilha dos c?digos de magistrados
BD_magistrados=read_excel("/home/silva/Downloads/romi_ofice/Gerar_passo_4/tables_fourth/BD magistrados.xls") # Arquivo ?nico
#Retirando os caracteres especiais
BD_magistrados$nome_magis<-stri_trans_general(BD_magistrados$nome_magis, "Latin-ASCII")  ##

# Banco de dados afastamentos
BD_afastamentos=read_excel("/home/silva/Downloads/romi_ofice/Gerar_passo_4/tables_fourth/mar/BD afastamentos.xlsx") # Arquivo mensal
colnames(BD_afastamentos)=c("nome_magis","inicio_afast","fim_afast","MOTIVO")
#Retirando os caracteres especiais
BD_afastamentos$nome_magis<-stri_trans_general(BD_afastamentos$nome_magis, "Latin-ASCII")  ##

# Banco de dados designa??es
BD_desig=read_excel("/home/silva/Downloads/romi_ofice/Gerar_passo_4/tables_fourth/mar/BD desig.xlsx") # Arquivo mensal
colnames(BD_desig)=c("nome_magis","inicio_desig","fim_desig","nome_serventia_desig","Tipo_magis")
#retirando os caracteres especiais
BD_desig$nome_magis<-stri_trans_general(BD_desig$nome_magis, "Latin-ASCII")  ##
BD_desig$nome_serventia_desig <- stri_trans_general(BD_desig$nome_serventia_desig, "Latin-ASCII")

# Buscar metas (Produtividade)
quarto_1grau=read_excel("/home/silva/Downloads/romi_ofice/Gerar_passo_4/tables_fourth/mar/Quarto passo 1º grau.xls") # Arquivo mensal


## verificar aqui se os dados est?o ok ##


colnames(quarto_1grau)[1:2]=c("nome_magis","nome_serventia_sicond")
#retirando os caracteres especiais
quarto_1grau$nome_magis<-stri_trans_general(quarto_1grau$nome_magis, "Latin-ASCII")  ##
# Adicionando coluna CPF_magis em quarto_1grau
quarto_1grau=left_join(quarto_1grau,BD_magistrados %>% select(nome_magis,CPF_magis))
# Adicionando coluna codigo_VT em quarto_1grau
quarto_1grau=left_join(quarto_1grau,BD_serventias %>% select(codigo_VT,nome_serventia_sicond))

#ordenando colunas
############ ajeiteita aqui
quarto_1grau <- quarto_1grau %>% select(codigo_VT, CPF_magis, nome_magis, nome_serventia_sicond,
                        `DECINT1º - Decisões interlocutórias no 1º grau`,                                                                
                         `RINTCJ1º - Recursos internos julgados no 1º grau na fase de conhecimento (embargos de declaração)`,             
                        `SENTCCM1º - Sentenças de conhecimento com julgamento do mérito no 1º grau`,                                     
                        `SENTCH1° - Sentenças em conhecimento homologatórias de acordos 1º grau`,                                        
                        `SENTCSM1º - Sentenças de conhecimento sem julgamento do mérito no 1º grau`,                                     
                        `SENTCSM1º - Sentenças de conhecimento sem julgamento do mérito no 1º grau`,                                     
                        `SENTEXH1º - Sentenças em execução homologatórias de acordos no 1º grau`,                                        
                        `SENTEXTFISC1º - Sentenças em execução fiscal no 1º grau`,                                                       
                        `SENTEXTNFISC1º - Sentenças em execução de título extrajudicial no 1º grau, exceto sentenças em execução fiscal`,
                        `SENTEXH1º - Sentenças em execução homologatórias de acordos no 1º grau`,                       
                        `SENTJUD1º - Sentenças em execução judicial no 1º grau`)

quarto_2grau=read_excel("/home/silva/Downloads/romi_ofice/Gerar_passo_4/tables_fourth/mar/Quarto passo 2° grau.xls") # Arquivo mensal
colnames(quarto_2grau)[1:2]=c("nome_magis","nome_serventia_sicond")
#retirando os caracteres especiais
quarto_2grau$nome_magis<-stri_trans_general(quarto_2grau$nome_magis, "Latin-ASCII")  ##
# Adicionando coluna CPF_magis em quarto_1grau
quarto_2grau=left_join(quarto_2grau,BD_magistrados %>% select(CPF_magis,nome_magis))
# Adicionando coluna codigo_VT em quarto_1grau
quarto_2grau=left_join(quarto_2grau,BD_serventias %>% select(codigo_VT,nome_serventia_sicond))

# ordenando colunas
quarto_2grau <- quarto_2grau %>% select(codigo_VT, CPF_magis, nome_magis, nome_serventia_sicond,
                        `AUDCONC2º - Audiências de conciliação realizadas em 2º grau`,
                        `DEC2º - Decisões terminativas de processo no 2º grau`,
                        `DECDC2º - Decisões terminativas proferidas nas demais classes processuais no 2º grau`,
                        `DECH2º - Decisões homologatórias de acordos no 2º grau`,
                        `DECINT2º - Decisões interlocutórias no 2º grau`,
                          `RINTJ2º - Recursos internos julgados no 2º grau`,
                        `VOTORNCRIM2º - Votos proferidos pelo relator em processos não criminais de 2º grau`)

#--------------------------------------------------------------------------------#
# A função time_function_desig entra aqui
#---------------------------------------------------------------- ----------------#
source("/home/silva/Downloads/romi_ofice/Gerar_passo_4/new_function/time_function_desig.R")
data_inicial <- ("01/03/2020")
data_final <- dmy("31/03/2020")
dias_mes <- 31
lista <- periodo_trabalhado(data_inicial, data_final, dias_mes, BD_desig)

fim <- Sys.time()
 fim-com
# BD_desig_ <- lista$desig
# 
# BD_desig_ <- left_join(BD_desig, BD_magistrados[,c(2,3)])
# BD_desig_ <- BD_desig %>% mutate(Junção=paste(codigo_VT,"-",CPF_magis))
# 
#  Quarto_passo <-readRDS("/home/silva/Downloads/romi_ofice/Gerar_passo_4/quarto_passo_mar.RDS")
# # 
# Quarto_passo<- left_join(Quarto_passo, BD_desig_[,c(7,9)])
# Quarto_passo <- left_join(Quarto_passo, lista$afastamento)
