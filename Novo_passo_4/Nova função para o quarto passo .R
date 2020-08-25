

rm(list = ls()) # Limpa a memória

com <-Sys.time()# iniciando contagem de tempo

## mundando o diretório # quando está no trt
#setwd("X:/SGE/GABINETE/CONSELHO NACIONAL DE JUSTICA/JUSTICA EM NUMEROS/JUSTI?A EM N?MEROS_DADOS ANUAIS/JN ANO 2020/Arquivos Provimento 49 de 18_08_2015/Quarto Passo/Gerar quarto passo")

# ---------------------------- #
# Valores a ser alterados
# ---------------------------- #

#valores inicias
dia_inicio_atual=01
dia_fim_atual=31
mes_atual=07
ano_atual=2020
nome_mes_atual="Julho"

#pacotes utilizados 
library(dplyr) #manipula??o de dados
library(readxl) #leitura de arquivo xls
#library(excel.link) #criando arquivo em excel
library(lubridate) # manipula??o de datas
library(stringi)  # manipula??o de strings
library(readODS) #ler arquivos .ods

# mudando o diretório
setwd("C:/Users/silva/Downloads/romi_ofice")

# Planilha dos c?digos de serventia
# codigos_serventia=read_excel("Codigo_serventia.xls")
BD_serventias=read_excel("C:/Users/silva/Downloads/romi_ofice/data_base/BD serventias.xls") # Arquivo ?nico
# retirando os caracteres especias
BD_serventias$nome_serventia_sicond <- stri_trans_general(BD_serventias$nome_serventia_sicond, "Latin-ASCII")
BD_serventias$nome_serventia_egestao <- stri_trans_general(BD_serventias$nome_serventia_egestao, "Latin-ASCII")
BD_serventias$nome_serventia_desig  <- stri_trans_general(BD_serventias$nome_serventia_desig, "Latin-ASCII")


# Planilha dos c?digos de magistrados
BD_magistrados=read_excel("C:/Users/silva/Downloads/romi_ofice/data_base/BD magistrados.xls") # Arquivo ?nico
#Retirando os caracteres especiais 
BD_magistrados$nome_magis<-stri_trans_general(BD_magistrados$nome_magis, "Latin-ASCII")  ##

# Banco de dados afastamentos
BD_afastamentos=read_excel("C:/Users/silva/Downloads/romi_ofice/Passo 4/julho/BD afastamentos.xls") # Arquivo mensal
colnames(BD_afastamentos)=c("nome_magis","inicio_afast","fim_afast","MOTIVO")
#Retirando os caracteres especiais
BD_afastamentos$nome_magis<-stri_trans_general(BD_afastamentos$nome_magis, "Latin-ASCII")  ##

# Banco de dados designa??es
BD_desig=read_excel("C:/Users/silva/Downloads/romi_ofice/Passo 4/julho/BD desig.xlsx") # Arquivo mensal
colnames(BD_desig)=c("nome_magis","inicio_desig","fim_desig","nome_serventia_desig","Tipo_magis")
#retirando os caracteres especiais
BD_desig$nome_magis<-stri_trans_general(BD_desig$nome_magis, "Latin-ASCII")  ##
BD_desig$nome_serventia_desig <- stri_trans_general(BD_desig$nome_serventia_desig, "Latin-ASCII")

# Buscar metas (Produtividade)
#quarto_1grau=read_excel("C:/Users/silva/Downloads/romi_ofice/Passo 4/abril/Quarto passo 1 grau.xlsx") # Arquivo mensal
quarto_1grau=read_ods("C:/Users/silva/Downloads/romi_ofice/Passo 4/julho/Quarto passo 1 grau.ods") # Arquivo mensal


## verificar aqui se os dados est?o ok ##

# mudando o nome das duas primeiras colunas
colnames(quarto_1grau)[1:2]=c("nome_magis","nome_serventia_sicond")

#retirando os caracteres especiais
quarto_1grau$nome_magis<-stri_trans_general(quarto_1grau$nome_magis, "Latin-ASCII")  ##
quarto_1grau$nome_serventia_sicond<-stri_trans_general(quarto_1grau$nome_serventia_sicond, "Latin-ASCII") 
# Adicionando coluna CPF_magis em quarto_1grau
quarto_1grau=left_join(quarto_1grau,BD_magistrados %>% select(nome_magis,CPF_magis))
# Adicionando coluna codigo_VT em quarto_1grau
quarto_1grau=left_join(quarto_1grau,BD_serventias %>% select(codigo_VT,nome_serventia_sicond))

# Buscar metas (Produtividade)
#quarto_2grau=read_excel("C:/Users/silva/Downloads/romi_ofice/Passo 4/abril/Quarto passo 2º grau.xlsx") # Arquivo mensal
quarto_2grau=read_ods("C:/Users/silva/Downloads/romi_ofice/Passo 4/julho/Quarto passo 2 grau.ods") # Arquivo mensal

## verificar aqui se os dados est?o ok ##

# mudando o nome das duas primeiras colunas
colnames(quarto_2grau)[1:2]=c("nome_magis","nome_serventia_sicond")
#retirando os caracteres especiais
quarto_2grau$nome_magis<-stri_trans_general(quarto_2grau$nome_magis, "Latin-ASCII")  ##
quarto_2grau$nome_serventia_sicond<-stri_trans_general(quarto_2grau$nome_serventia_sicond, "Latin-ASCII")  ##
# Adicionando coluna CPF_magis em quarto_1grau
quarto_2grau=left_join(quarto_2grau,BD_magistrados %>% select(CPF_magis,nome_magis))
# Adicionando coluna codigo_VT em quarto_1grau
quarto_2grau=left_join(quarto_2grau,BD_serventias %>% select(codigo_VT,nome_serventia_sicond))

## verificar aqui se os dados est?o ok ##

#--------------------------------------------------------------------------------#
# A funÃ§Ã£o time_function_desig entra aqui
#---------------------------------------------------------------- ----------------#
# Chamando a função que calcula os dias trabalhados de cada designação
source("C:/Users/silva/Documents/Repositorio/Gerando_passos/Novo_passo_4/time_function_desig.R")
# Dados de entrada
data_inicial <- dmy("01/07/2020")
data_final <- dmy("31/07/2020")
dias_mes <- 31

# A saida é uma lista com os dados de designação e de afastamento
lista <- periodo_trabalhado(data_inicial, data_final, dias_mes, BD_desig)

# Renomeando e ajustando os novo dados de deignação
BD_desig_ <- (lista$desig)
BD_desig_ <- left_join(BD_desig_, BD_magistrados %>% select(-codigo_magis))
BD_desig_ <- BD_desig_ %>% mutate(Junção=paste(codigo_VT,"-",CPF_magis))
#retirando os repetidos
BD_desig_ <- (BD_desig_ %>% distinct(Junção,inicio_desig, .keep_all = T))


#codigo do tipo de juiz
Tipo_magis=c("Magistrado Titular","Juiz no Exercício da Titularidade",
             "Juiz Vinculado","Juiz Substituto","Substituto TRT")
codigo_TJ=c(0,1,3,3,8)
BD_Tipo_de_Juiz=data.frame(codigo_TJ,Tipo_magis)

BD_desig_ <- left_join(BD_desig_, BD_Tipo_de_Juiz)



# --------------------------------------------------------- #
# ----- TRABALHANDO COM AS VARIÁVEIS DO PRIMEIRO GRAU ----- #
# --------------------- Primeiro Grau --------------------- #

# Funções utilizadas para verificar se uma determinada
# variável está presente o banco de dados ou não.
library(lazyeval)
verificar_coluna <- function(data, coluna){
  coluna_texto <- lazyeval::expr_text(coluna)
  coluna_texto %in% names(data)
}

verificar=function(data,coluna,retorno){
  if(verificar_coluna(data,coluna)==T){
    return(retorno)
  }else{
    return(NA)
  }
}

# Variáveis do Quarto passo 1º grau
AudConc1º=verificar(quarto_1grau,`AUDCONC1º - Audiências de conciliação realizadas em 1º grau`,
                    quarto_1grau$`AUDCONC1º - Audiências de conciliação realizadas em 1º grau`)
AudNConc1º=verificar(quarto_1grau,`AUDNCONC1º - Audiências realizadas em 1º grau - exceto de conciliações`,
                     quarto_1grau$`AUDNCONC1º - Audiências realizadas em 1º grau - exceto de conciliações`)
DecInt1º=verificar(quarto_1grau,`DECINT1º - Decisões interlocutórias no 1º grau`,
                   quarto_1grau$`DECINT1º - Decisões interlocutórias no 1º grau`)
RIntCJ1º=verificar(quarto_1grau,`RINTCJ1º - Recursos internos julgados no 1º grau na fase de conhecimento (embargos de declaração)`,
                   quarto_1grau$`RINTCJ1º - Recursos internos julgados no 1º grau na fase de conhecimento (embargos de declaração)`)
SentCCM1º=verificar(quarto_1grau,`SENTCCM1º - Sentenças de conhecimento com julgamento do mérito no 1º grau`,
                    quarto_1grau$`SENTCCM1º - Sentenças de conhecimento com julgamento do mérito no 1º grau`)
SentCSM1º=verificar(quarto_1grau,`SENTCSM1º - Sentenças de conhecimento sem julgamento do mérito no 1º grau`,
                    quarto_1grau$`SENTCSM1º - Sentenças de conhecimento sem julgamento do mérito no 1º grau`)
SentDC1º=verificar(quarto_1grau,`SENTDC1º - Sentenças proferidas nas demais classes processuais no 1º grau`,
                   quarto_1grau$`SENTDC1º - Sentenças proferidas nas demais classes processuais no 1º grau`)
SentExH1º=verificar(quarto_1grau,`SENTEXH1º - Sentenças em execução homologatórias de acordos no 1º grau`,
                    quarto_1grau$`SENTEXH1º - Sentenças em execução homologatórias de acordos no 1º grau`)
SentExtFisc1º=verificar(quarto_1grau,`SENTEXTFISC1º - Sentenças em execução fiscal no 1º grau`,
                        quarto_1grau$`SENTEXTFISC1º - Sentenças em execução fiscal no 1º grau`)
SentJud1º=verificar(quarto_1grau,`SENTJUD1º - Sentenças em execução judicial no 1º grau`,
                    quarto_1grau$`SENTJUD1º - Sentenças em execução judicial no 1º grau`)
SentExtNFisc1º=verificar(quarto_1grau,`SENTEXTNFISC1º - Sentenças em execução de título extrajudicial no 1º grau, exceto sentenças em execução fiscal`,
                         quarto_1grau$`SENTEXTNFISC1º - Sentenças em execução de título extrajudicial no 1º grau, exceto sentenças em execução fiscal`)
SentHDC1º=verificar(quarto_1grau,`SENTHDC1º - Sentenças homologatórias de acordo proferidas nas demais classes no 1º grau`,
                    quarto_1grau$`SENTHDC1º - Sentenças homologatórias de acordo proferidas nas demais classes no 1º grau`)
SentCH1º=verificar(quarto_1grau,`SENTCH1° - Sentenças em conhecimento homologatórias de acordos 1º grau`,
                   quarto_1grau$`SENTCH1° - Sentenças em conhecimento homologatórias de acordos 1º grau`)

# Juntando as variáveis
dados1=data.frame(nome_serventia_sicond=quarto_1grau$nome_serventia_sicond,
                  nome_magis=quarto_1grau$nome_magis,
                  codigo_VT=quarto_1grau$codigo_VT,
                  CPF_magis=quarto_1grau$CPF_magis,
                  AudConc1º,
                  AudNConc1º,
                  DecInt1º,
                  RIntCJ1º,
                  SentCCM1º,
                  SentCSM1º,
                  SentDC1º,
                  SentExH1º,
                  SentExtFisc1º,
                  SentJud1º,
                  SentExtNFisc1º,
                  SentHDC1º,
                  SentCH1º)

# Adicionando uma coluna com a junção dos codigos de serventia e CPF em dados1
dados1=dados1 %>% mutate(Junção=paste(codigo_VT,"-",CPF_magis))

# Adicionando coluna iniciais
dados1$`Tipo Juiz`=NA
dados1$Mes=mes_atual
dados1$Ano=ano_atual
dados1$`Quantidade dias corridos`=NA
dados1$Observação=NA

# Variáveis do 2º grau
dados1$AudConc2º=NA
dados1$AudNConc2º=NA
dados1$Dec2º=NA
dados1$DecDC2º=NA
dados1$DecH2º=NA
dados1$DecHDC2º=NA
dados1$DecInt2º=NA
dados1$RintJ2º=NA
dados1$VotoR2º=NA

# Organizando o banco de dados 1º grau
dados1 = dados1 %>% select(Junção,nome_magis,nome_serventia_sicond,
                           CPF_magis,codigo_VT,`Tipo Juiz`,Mes,
                           Ano,`Quantidade dias corridos`,Observação,AudConc2º,AudNConc2º,Dec2º,DecDC2º,
                           DecH2º,DecHDC2º,DecInt2º,RintJ2º,VotoR2º,AudConc1º,AudNConc1º,DecInt1º,RIntCJ1º,
                           SentCCM1º,SentCH1º,SentCSM1º,SentDC1º,SentExH1º,SentExtFisc1º,SentExtNFisc1º,
                           SentHDC1º,SentJud1º)

# ----------------------------------------------------------- #
# ------------------- Segundo Grau -------------------------- #
# ------- Trabalhando com variáveis do segundo grau --------- #

# Variáveis do Quarto passo 2º grau
AudConc2º=verificar(quarto_2grau,`AUDCONC2º - Audiências de conciliação realizadas em 2º grau`,
                    quarto_2grau$`AUDCONC2º - Audiências de conciliação realizadas em 2º grau`)
Dec2º=verificar(quarto_2grau,`DEC2º - Decisões terminativas de processo no 2º grau`,
                quarto_2grau$`DEC2º - Decisões terminativas de processo no 2º grau`)
DecH2º=verificar(quarto_2grau,`DECH2º - Decisões homologatórias de acordos no 2º grau`,
                 quarto_2grau$`DECH2º - Decisões homologatórias de acordos no 2º grau`)
DecInt2º=verificar(quarto_2grau,`DECINT2º - Decisões interlocutórias no 2º grau`,
                   quarto_2grau$`DECINT2º - Decisões interlocutórias no 2º grau`)
RintJ2º=verificar(quarto_2grau,`RINTJ2º - Recursos internos julgados no 2º grau`,
                  quarto_2grau$`RINTJ2º - Recursos internos julgados no 2º grau`)
VotoR2º=verificar(quarto_2grau,`VOTORNCRIM2º - Votos proferidos pelo relator em processos não criminais de 2º grau`,
                  quarto_2grau$`VOTORNCRIM2º - Votos proferidos pelo relator em processos não criminais de 2º grau`)
AudNConc2º=verificar(quarto_2grau,`AUDNCON2º - Audiências realizadas em 2º grau - exceto de cpmciliação`,
                     quarto_2grau$`AUDNCON2º - Audiências realizadas em 2º grau - exceto de cpmciliação`)
DecDC2º=verificar(quarto_2grau,`DECDC2º - Decisões terminativas proferidas nas demais classes processuais no 2º grau`,
                  quarto_2grau$`DECDC2º - Decisões terminativas proferidas nas demais classes processuais no 2º grau`)
DecHDC2º=verificar(quarto_2grau,`DECDC2º - Decisões terminativas proferidas nas demais classes processuais no 2º grau`,
                   quarto_2grau$`DECDC2º - Decisões terminativas proferidas nas demais classes processuais no 2º grau`)

# Juntando as variáveis
dados2 = data.frame(nome_serventia_sicond=quarto_2grau$nome_serventia_sicond,
                    nome_magis=quarto_2grau$nome_magis,
                    codigo_VT=quarto_2grau$codigo_VT,
                    CPF_magis=quarto_2grau$CPF_magis,
                    AudConc2º,
                    Dec2º,
                    DecH2º,
                    DecInt2º,
                    RintJ2º,
                    VotoR2º,
                    AudNConc2º,
                    DecDC2º,
                    DecHDC2º)

# Variáveis do 1 Grau
dados2$AudConc1º=NA
dados2$AudNConc1º=NA
dados2$DecInt1º=NA
dados2$RIntCJ1º=NA
dados2$SentCCM1º=NA
dados2$SentCH1º=NA
dados2$SentCSM1º=NA
dados2$SentDC1º=NA
dados2$SentExH1º=NA
dados2$SentExtFisc1º=NA
dados2$SentExtNFisc1º=NA
dados2$SentHDC1º=NA
dados2$SentJud1º=NA

# Adicionando coluna iniciais  
dados2$`Tipo Juiz`=NA
dados2$Mes=mes_atual
dados2$Ano=ano_atual
dados2$`Quantidade dias corridos`=NA
dados2$Observação=NA

# Adicionando uma coluna com a junção dos codigos de serventia e CPF em dados2
dados2 = dados2 %>% mutate(Junção=paste(codigo_VT,"-",CPF_magis))

# Organizando o banco de dados 2º grau
dados2 = dados2 %>% select(Junção,nome_magis,nome_serventia_sicond,
                           CPF_magis,codigo_VT,`Tipo Juiz`,Mes,
                           Ano,`Quantidade dias corridos`,Observação,AudConc2º,AudNConc2º,Dec2º,DecDC2º,
                           DecH2º,DecHDC2º,DecInt2º,RintJ2º,VotoR2º,AudConc1º,AudNConc1º,DecInt1º,RIntCJ1º,
                           SentCCM1º,SentCH1º,SentCSM1º,SentDC1º,SentExH1º,SentExtFisc1º,SentExtNFisc1º,
                           SentHDC1º,SentJud1º)

# # # Juntando ambos os bancos de dados # # # 
# # # se precisar reorganizar o 'Quarto passo' começar aqui # # # 
Quarto_passo=rbind(dados1,dados2)

#colocando o codigo tj
Quarto_passo <- (left_join(Quarto_passo, BD_desig_ %>% select(Junção, codigo_TJ)))

# Colocando código 4 para todos os espaços com NA,
Quarto_passo$codigo_TJ=ifelse(is.na(Quarto_passo$codigo_TJ),4,Quarto_passo$codigo_TJ)

# # # #  # colocando  os que estão somente no do quarto passo  # # # # # # # # # ## # # # 

# preparando os dias trabalhados e código_tj
# aglutinando os dias trabalhados na mesma vt
info <- BD_desig_ %>% select("Junção","nome_magis","codigo_TJ","nome_serventia_sicond","CPF_magis","codigo_VT","dias_desig" = tempo_trabalhado)

#colocando os dias desig

Quarto_passo <- left_join(Quarto_passo, info %>% select("Junção","codigo_TJ","dias_desig"))

# # # # # # colocando os que estão em designação e nao no quarto passo # # # # # # # # # # # # # # 

# Adicionando coluna iniciais
info$`Tipo Juiz`=NA
info$Mes=mes_atual
info$Ano=ano_atual
info$`Quantidade dias corridos` = NA
info$Observação=NA

# Variáveis do 2º grau
info$AudConc2º=NA
info$AudNConc2º=NA
info$Dec2º=NA
info$DecDC2º=NA
info$DecH2º=NA
info$DecHDC2º=NA
info$DecInt2º=NA
info$RintJ2º=NA
info$VotoR2º=NA

# Variáveis do 1 Grau
info$AudConc1º=NA
info$AudNConc1º=NA
info$DecInt1º=NA
info$RIntCJ1º=NA
info$SentCCM1º=NA
info$SentCH1º=NA
info$SentCSM1º=NA
info$SentDC1º=NA
info$SentExH1º=NA
info$SentExtFisc1º=NA
info$SentExtNFisc1º=NA
info$SentHDC1º=NA
info$SentJud1º=NA

# ordenando
info <-  info  %>% select(Junção,nome_magis,nome_serventia_sicond,
                              CPF_magis,codigo_VT,`Tipo Juiz`,Mes,
                              Ano,`Quantidade dias corridos`,Observação,AudConc2º,AudNConc2º,Dec2º,DecDC2º,
                              DecH2º,DecHDC2º,DecInt2º,RintJ2º,VotoR2º,AudConc1º,AudNConc1º,DecInt1º,RIntCJ1º,
                              SentCCM1º,SentCH1º,SentCSM1º,SentDC1º,SentExH1º,SentExtFisc1º,SentExtNFisc1º,
                              SentHDC1º,SentJud1º,codigo_TJ,dias_desig)

Quarto_passo <- rbind(Quarto_passo, info)

Quarto_passo <- Quarto_passo[order(Quarto_passo$nome_magis),]

Quarto_passo <- (Quarto_passo %>% distinct(Junção, codigo_TJ, .keep_all= T))




# colocando os dias de afastamentos e as observações
Quarto_passo <- (left_join(Quarto_passo, lista$afastamento))

# ordenando as colunas
Quarto_passo <- Quarto_passo %>% select(`CPF Magistrado`=CPF_magis,`Código Serventia`=codigo_VT,nome_magis,nome_serventia_sicond,
                                        `Tipo Juiz`=codigo_TJ,Mes,
                                        Ano,`Quantidade dias corridos`=dias_desig,
                                        `Dias de Afastamento`=tempo_afastado,Observação=Descrição,
                                        AudConc2º,AudNConc2º,Dec2º,DecDC2º,
                                        DecH2º,DecHDC2º,DecInt2º,RintJ2º,VotoR2º,AudConc1º,AudNConc1º,DecInt1º,RIntCJ1º,
                                        SentCCM1º,SentCH1º,SentCSM1º,SentDC1º,SentExH1º,SentExtFisc1º,SentExtNFisc1º,
                                        SentHDC1º,SentJud1º)
# Criando função que converte um vetor de encoding qualquer para encoding latin1
# sendo este necessário para vizualização correta no excel
Converter_em_latin1=function(vetor){
  vetor=as.character(vetor)
  n=length(vetor)
  a=vector(length = n)
  for(i in 1:n){
    if(Encoding(vetor[i])!="unknown"){
      a[i]=iconv(vetor[i], from=Encoding(vetor[i]), to="latin1", sub="byte")
    }else{
      a[i]=vetor[i]
    }
  }
  return(a)
}

# colocando o código para os desembargadores
for (i in 1:nrow(Quarto_passo)) {
  if(str_detect(Quarto_passo$nome_serventia_sicond[i],Quarto_passo$nome_magis[i] ) == TRUE)
    Quarto_passo$`Tipo Juiz`[i] <- 6
}

# Transformando colunas formadas por strings
Quarto_passo$nome_serventia_sicond=Converter_em_latin1(Quarto_passo$nome_serventia_sicond)
Quarto_passo$nome_magis=Converter_em_latin1(Quarto_passo$nome_magis)
Quarto_passo$Observação=Converter_em_latin1(Quarto_passo$Observação)
Quarto_passo$`CPF Magistrado` <- as.numeric(paste0(Quarto_passo$`CPF Magistrado`) )

Quarto_passo <- Quarto_passo %>% select(`CPF Magistrado`,`Código Serventia`, nome_magis,
                                        nome_serventia_sicond,`Tipo Juiz` ,Mes,
                                        Ano,`Quantidade dias corridos`,`Dias de Afastamento`,
                                        Observação,
                                        AudConc2º,AudNConc2º,Dec2º,DecDC2º,
                                        DecH2º,DecHDC2º,DecInt2º,RintJ2º,VotoR2º,AudConc1º,AudNConc1º,DecInt1º,RIntCJ1º,
                                        SentCCM1º,SentCH1º,SentCSM1º,SentDC1º,SentExH1º,SentExtFisc1º,SentExtNFisc1º,
                                        SentHDC1º,SentJud1º)



library(openxlsx)
write.xlsx(Quarto_passo, "C:/Users/silva/Downloads/romi_ofice/Passo 4/julho/Quarto_passo_saida_julho.xlsx")

fim <- Sys.time()
fim-com