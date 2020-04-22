

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
BD_serventias=read_excel("C:/Users/silva/Downloads/romi_ofice/data_base/BD serventias.xls") # Arquivo ?nico
# retirando os caracteres especias
BD_serventias$nome_serventia_sicond <- stri_trans_general(BD_serventias$nome_serventia_sicond, "Latin-ASCII")
# BD_serventias$nome_serventia_egestao <- stri_trans_general(BD_serventias$nome_serventia_egestao, "Latin-ASCII")
# BD_serventias$nome_serventia_desig  <- stri_trans_general(BD_serventias$nome_serventia_desig, "Latin-ASCII")


# Planilha dos c?digos de magistrados
BD_magistrados=read_excel("C:/Users/silva/Downloads/romi_ofice/data_base/BD magistrados.xls") # Arquivo ?nico
#Retirando os caracteres especiais 
BD_magistrados$nome_magis<-stri_trans_general(BD_magistrados$nome_magis, "Latin-ASCII")  ##

# Banco de dados afastamentos
BD_afastamentos=read_excel("C:/Users/silva/Downloads/romi_ofice/marc/BD afastamentos.xlsx") # Arquivo mensal
colnames(BD_afastamentos)=c("nome_magis","inicio_afast","fim_afast","MOTIVO")
#Retirando os caracteres especiais
BD_afastamentos$nome_magis<-stri_trans_general(BD_afastamentos$nome_magis, "Latin-ASCII")  ##

# Banco de dados designa??es
BD_desig=read_excel("C:/Users/silva/Downloads/romi_ofice/marc/BD desig.xlsx") # Arquivo mensal
colnames(BD_desig)=c("nome_magis","inicio_desig","fim_desig","nome_serventia_desig","Tipo_magis")
#retirando os caracteres especiais
BD_desig$nome_magis<-stri_trans_general(BD_desig$nome_magis, "Latin-ASCII")  ##
BD_desig$nome_serventia_desig <- stri_trans_general(BD_desig$nome_serventia_desig, "Latin-ASCII")

# Buscar metas (Produtividade)
quarto_1grau=read_excel("C:/Users/silva/Downloads/romi_ofice/marc//Quarto passo 1º grau.xlsx") # Arquivo mensal


## verificar aqui se os dados est?o ok ##


colnames(quarto_1grau)[1:2]=c("nome_magis","nome_serventia_sicond")
#retirando os caracteres especiais
quarto_1grau$nome_magis<-stri_trans_general(quarto_1grau$nome_magis, "Latin-ASCII")  ##
quarto_1grau$nome_serventia_sicond<-stri_trans_general(quarto_1grau$nome_serventia_sicond, "Latin-ASCII") 
# Adicionando coluna CPF_magis em quarto_1grau
quarto_1grau=left_join(quarto_1grau,BD_magistrados %>% select(nome_magis,CPF_magis))
# Adicionando coluna codigo_VT em quarto_1grau
quarto_1grau=left_join(quarto_1grau,BD_serventias %>% select(codigo_VT,nome_serventia_sicond))

#ordenando colunas
############ ajeiteita aqui


quarto_2grau=read_excel("C:/Users/silva/Downloads/romi_ofice/marc/Quarto passo 2º grau.xlsx") # Arquivo mensal
colnames(quarto_2grau)[1:2]=c("nome_magis","nome_serventia_sicond")
#retirando os caracteres especiais
quarto_2grau$nome_magis<-stri_trans_general(quarto_2grau$nome_magis, "Latin-ASCII")  ##
quarto_2grau$nome_serventia_sicond<-stri_trans_general(quarto_2grau$nome_serventia_sicond, "Latin-ASCII")  ##
# Adicionando coluna CPF_magis em quarto_1grau
quarto_2grau=left_join(quarto_2grau,BD_magistrados %>% select(CPF_magis,nome_magis))
# Adicionando coluna codigo_VT em quarto_1grau
quarto_2grau=left_join(quarto_2grau,BD_serventias %>% select(codigo_VT,nome_serventia_sicond))

# ordenando colunas

#--------------------------------------------------------------------------------#
# A funÃ§Ã£o time_function_desig entra aqui
#---------------------------------------------------------------- ----------------#
source("C:/Users/silva/Documents/Repositorio/Gerando_passos/Novo_passo_4/time_function_desig.R")
data_inicial <- ("01/03/2020")
data_final <- dmy("31/03/2020")
dias_mes <- 31
com <- Sys.time()
lista <- periodo_trabalhado(data_inicial, data_final, dias_mes, BD_desig)

fim <- Sys.time()
 fim-com
BD_desig_ <- lista$desig

BD_desig_ <- left_join(BD_desig_, BD_magistrados[,-1])

BD_desig_ <- BD_desig %>% mutate(Junção=paste(codigo_VT,"-",CPF_magis))



#  Quarto_passo <-readRDS("/home/silva/Downloads/romi_ofice/Gerar_passo_4/quarto_passo_mar.RDS")
# # 
# Quarto_passo<- left_join(Quarto_passo, BD_desig_[,c(7,9)])
# Quarto_passo <- left_join(Quarto_passo, lista$afastamento)
