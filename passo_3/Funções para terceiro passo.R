
rm(list = ls()) # lipando a mem�ria
# definindo as datas


### Terceiro passo ### 
library(dplyr)
library(readxl)
library(readODS)
library(lubridate) # manipula??o de datas

# datas de entrada
inicio_mes_referencia <- dmy("1/03/2021") # entra com a data inicial do mes de refer�ncia
mes_atual <- month(inicio_mes_referencia) 
ano_atual <- year(inicio_mes_referencia)


# setwd(): Muda o diret�rio
# getwd(): Mostra o diret�rio

#Gerar_terceiro_passo=function(mes_tual,ano_atual){

# Buscar Metas
#setwd("X:/SGE/GABINETE/CONSELHO NACIONAL DE JUSTICA/JUSTICA EM NUMEROS/JUSTI�A EM N�MEROS_DADOS ANUAIS/JN ANO 2020/Arquivos Provimento 49 de 18_08_2015/Terceiro Passo/Gerar terceiro passo")
#setwd("/home/raulls/R_Diretorio/romi-office-trt") 
setwd("D:/romi_ofice/Passo 3")
# para arquivo em ods
terceiro_1grau=read_ods("2021/mar�o/Terceiro Passo 1 grau.ods")
terceiro_2grau=read_ods("2021/mar�o/Terceiro Passo 2 grau.ods")
# para arquivo em xls/xlsx
# terceiro_1grau=read_excel("2021/mar�o/Terceiro Passo 1 grau.xls")
# terceiro_2grau=read_excel("2021/mar�o/Terceiro Passo 2 grau.xls")

# Planilha dos C�digos
codigos=read_excel("codigos.xls")

# --------------------- Primeiro Grau -------------------- #

# Fun��es utilizadas para verificar se uma determinada 
# vari�vel est� presente o banco de dados ou n�o.
library(lazyeval)

# Fun��o verificar coluna, retorna True para caso de a coluna 
# existir no Banco de dados (BD), False caso contr�rio.
verificar_coluna <- function(BD, coluna){
  coluna_texto <- lazyeval::expr_text(coluna)
  coluna_texto %in% names(BD)
}

verificar=function(BD,coluna,retorno){
  if(verificar_coluna(BD,coluna)==T){
    return(retorno)
  }else{
    return(NA)
  }
}

# Vari�veis do Quarto passo 1� grau
CartaD1�=verificar(terceiro_1grau,`CARTAD1� - Cartas precat�rias, de ordem e rogat�rias devolvidas`,
                   terceiro_1grau$`CARTAD1� - Cartas precat�rias, de ordem e rogat�rias devolvidas`)
CartaN1�=verificar(terceiro_1grau,`CARTAN1� - Cartas precat�rias, de ordem e rogat�rias que ingressaram no 1� grau`,
                   terceiro_1grau$`CARTAN1� - Cartas precat�rias, de ordem e rogat�rias que ingressaram no 1� grau`)
CnC1� =verificar(terceiro_1grau,`CNC1� - Casos movos de conhecimento 1� grau`,
                 terceiro_1grau$`CNC1� - Casos movos de conhecimento 1� grau`)
CpC1� =verificar(terceiro_1grau,`CPC1� - Casos pendentes de conhecimento em 1� grau`,
                 terceiro_1grau$`CPC1� - Casos pendentes de conhecimento em 1� grau`)
CpExtFisc1� =verificar(terceiro_1grau,`CPEXTFISC1� - Casos pendentes de execu��o fiscal no 1� grau`,
                       terceiro_1grau$`CPEXTFISC1� - Casos pendentes de execu��o fiscal no 1� grau`)
CpExtNFisc1� =verificar(terceiro_1grau,`CPEXTNFISC1� - Casos pendentes de execu��o de t�tulo extrajudicial no 1� grau, exceto execu��es fiscais`,
                        terceiro_1grau$`CPEXTNFISC1� - Casos pendentes de execu��o de t�tulo extrajudicial no 1� grau, exceto execu��es fiscais`)
ExeJud1� =verificar(terceiro_1grau,`EXEJUD1� - Execu��es judiciais no 1� grau`,
                    terceiro_1grau$`EXEJUD1� - Execu��es judiciais no 1� grau`)
ExeJudP1� =verificar(terceiro_1grau,`EXEJUDP1� - Execu��es judiciais pendentes no 1� grau`,
                     terceiro_1grau$`EXEJUDP1� - Execu��es judiciais pendentes no 1� grau`)
PRedC1� =verificar(terceiro_1grau,`PREDC1� - Processos de conhecimento no 1� grau encaminhados a outra unidade judici�ria por motivo de redistribui��o`,
                   terceiro_1grau$`PREDC1� - Processos de conhecimento no 1� grau encaminhados a outra unidade judici�ria por motivo de redistribui��o`)
PRedRC1�=verificar(terceiro_1grau,`PREDRC1� - Processos de conhecimento no 1� grau recebidos de outra unidade judici�ria por motivo de redistribui��o`,
                   terceiro_1grau$`PREDRC1� - Processos de conhecimento no 1� grau recebidos de outra unidade judici�ria por motivo de redistribui��o`)
SuSC1� =verificar(terceiro_1grau,`SUSC1� - Processos de conhecimento suspensos ou sobrestados ou em arquivo provis�rio no 1� grau`,
                  terceiro_1grau$`SUSC1� - Processos de conhecimento suspensos ou sobrestados ou em arquivo provis�rio no 1� grau`)
SuSExFisc1� =verificar(terceiro_1grau,`SUSEXFISC1� - Execu��es fiscais suspensas ou sobrestadas ou em arquivo provis�rio`,
                       terceiro_1grau$`SUSEXFISC1� - Execu��es fiscais suspensas ou sobrestadas ou em arquivo provis�rio`)
SuSExNfisc1� =verificar(terceiro_1grau,`SUSEXNFISC1� - Execu��es judiciais e extrajudiciais suspensas ou sobrestadas ou em arquivo provis�rio, exceto execu��es fiscais`,
                        terceiro_1grau$`SUSEXNFISC1� - Execu��es judiciais e extrajudiciais suspensas ou sobrestadas ou em arquivo provis�rio, exceto execu��es fiscais`)
TBaixC1� =verificar(terceiro_1grau,`TBAIXC1� - Processos de conhecimento baixados no 1� grau`,
                    terceiro_1grau$`TBAIXC1� - Processos de conhecimento baixados no 1� grau`)
TBaixExtFisc1� =verificar(terceiro_1grau,`TBAIXEXTFISC1� - Total de processos baixados de execu��o fiscal no 1� grau`,
                          terceiro_1grau$`TBAIXEXTFISC1� - Total de processos baixados de execu��o fiscal no 1� grau`)
TBaixExtNFisc1� =verificar(terceiro_1grau,`TBAIXEXTNFISC1� - Total de processos baixados de execu��o de t�tulos extrajudiciais no 1� grau, exceto execu��es fiscais`,
                           terceiro_1grau$`TBAIXEXTNFISC1� - Total de processos baixados de execu��o de t�tulos extrajudiciais no 1� grau, exceto execu��es fiscais`)
TBaixJud1� =verificar(terceiro_1grau,`TBAIXJUD1� - Total de processos baixados de execu��o judicial no 1� grau`,
                      terceiro_1grau$`TBAIXJUD1� - Total de processos baixados de execu��o judicial no 1� grau`)
RIntC1�=verificar(terceiro_1grau,`RINTC1� - Recursos internos no 1� grau na fase de conhecimento (embargos de declara��o)`,
                  terceiro_1grau$`RINTC1� - Recursos internos no 1� grau na fase de conhecimento (embargos de declara��o)`)
RIntCP1�=verificar(terceiro_1grau,`RINTCP1� - Recursos internos pendentes no 1� grau na fase de conhecimento (embargos de declara��o)`,
                   terceiro_1grau$`RINTCP1� - Recursos internos pendentes no 1� grau na fase de conhecimento (embargos de declara��o)`)
CnExtFisc1�=verificar(terceiro_1grau,`CNEXTFISC1� - Casos novos de execu��o fiscal no 1� grau`,
                      terceiro_1grau$`CNEXTFISC1� - Casos novos de execu��o fiscal no 1� grau`)
CnExtNFisc1�=verificar(terceiro_1grau,`CNEXTNFISC1� - Casos novos de execu��o de t�tulo extrajudicial no 1� grau, exceto execu��es fiscais`,
                       terceiro_1grau$`CNEXTNFISC1� - Casos novos de execu��o de t�tulo extrajudicial no 1� grau, exceto execu��es fiscais`)
PRedExtNFisc1�=verificar(terceiro_1grau,`PREDEXTNFISC1� - Processos de execu��o de t�tulo extrajudicial n�o-fiscais no 1� grau encaminhados a outra unidade judici�ria por motivo de redistribui��o`,
                         terceiro_1grau$`PREDEXTNFISC1� - Processos de execu��o de t�tulo extrajudicial n�o-fiscais no 1� grau encaminhados a outra unidade judici�ria por motivo de redistribui��o`)
PRedRExtNFisc1�=verificar(terceiro_1grau,`PREDEXTNFISC1� - Processos de execu��o de t�tulo extrajudicial n�o-fiscais no 1� grau recebidos de outra unidade judici�ria por motivo de redistribui��o`,
                          terceiro_1grau$`PREDEXTNFISC1� - Processos de execu��o de t�tulo extrajudicial n�o-fiscais no 1� grau recebidos de outra unidade judici�ria por motivo de redistribui��o`)
PRedExtFisc1�=verificar(terceiro_1grau,`PREDEXTFISC1� - Processos de execu��o fiscal no 1� grau encaminhados a outra unidade judici�ria por motivo de redistribui��o`,
                        terceiro_1grau$`PREDEXTFISC1� - Processos de execu��o fiscal no 1� grau encaminhados a outra unidade judici�ria por motivo de redistribui��o`)
PRedRExtFisc1�=verificar(terceiro_1grau,`PREDREXTFISC1� - Processos de execu��o fiscal no 1� grau recebidos de outra unidade judici�ria por motivo de redistribui��o`,
                         terceiro_1grau$`PREDREXTFISC1� - Processos de execu��o fiscal no 1� grau recebidos de outra unidade judici�ria por motivo de redistribui��o`)

dados1=data.frame(`�rg�o Estat�stica`=terceiro_1grau$`�rg�o Estat�stica`, # Estat�stica, #`�rg�o Estat�stica`,
                  CartaD1�,
                  CartaN1� = terceiro_1grau$`CARTAN1� - Cartas precat�rias, de ordem e rogat�rias que ingressaram no 1� grau`,
                  CnC1�,
                  CpC1�,
                  CpExtFisc1�,
                  CpExtNFisc1�,
                  ExeJud1�,
                  ExeJudP1�,
                  PRedC1�,
                  PRedRC1�,
                  SuSC1�,
                  SuSExFisc1�,
                  SuSExNfisc1�,
                  TBaixC1�,
                  TBaixExtFisc1�,
                  TBaixExtNFisc1�,
                  TBaixJud1�,
                  RIntC1�,
                  RIntCP1�,
                  CnExtFisc1�,
                  CnExtNFisc1�,
                  PRedExtNFisc1�,
                  PRedRExtNFisc1�,
                  PRedExtFisc1�,
                  PRedRExtFisc1�)

colnames(dados1)[1]="�rg�o Estat�stica"
# Adicionando coluna de C�digos e nomes com base no  
dados1=left_join(dados1,codigos %>% select(`C�digo Serventia`,`�rg�o Estat�stica`))

# Criando colunas
dados1$Mes=mes_atual
dados1$Ano=ano_atual
dados1$Observa��o=NA

# Vari�veis do 2� grau
dados1$CartaD2�=NA # 2� grau
dados1$CartaN2�=NA # 2� grau
dados1$CnO2�=NA # 2� grau
dados1$CnR2�=NA # 2� grau
dados1$Cp2�=NA # 2� grau
dados1$PRed2�=NA # 2� grau
dados1$PRedR2�=NA # 2� grau
dados1$Rint2�=NA # 2� grau
dados1$RintP2�=NA # 2� grau
dados1$SuS2�=NA # 2� grau
dados1$TBaix2�=NA # 2� grau
dados1$VPfG2�=NA # 2� grau
dados1$VPnG2�=NA # 2� grau

# Organizando o banco de dados 1� grau
dados1 = dados1 %>% select(
  `C�digo Serventia`=`C�digo Serventia`,`�rg�o Estat�stica`=`�rg�o Estat�stica`, Mes, Ano, Observa��o,CartaD2�,CartaN2�,CnO2�,CnR2�,Cp2�,
  PRed2�,PRedR2�,Rint2�,RintP2�,SuS2�,TBaix2�,VPfG2�,VPnG2�,
  CartaD1�,CartaN1�,CnC1�,CnExtFisc1�,CnExtNFisc1�,CpC1�,CpExtFisc1�,
  CpExtNFisc1�,ExeJud1�,ExeJudP1�,PRedC1�,PRedExtFisc1�,PRedExtNFisc1�,
  PRedRC1�,PRedRExtFisc1�,PRedRExtNFisc1�,RIntC1�,RIntCP1�,SuSC1�,
  SuSExFisc1�,SuSExNfisc1�,TBaixC1�,TBaixExtFisc1�,TBaixExtNFisc1�,TBaixJud1�)

# ------------------- Segundo Grau -------------------------- #
## Trabalhando com vari�veis do segundo grau
CnO2�=verificar(terceiro_2grau,`CNO2� - Casos novos origin�rios de 2� grau`,
                terceiro_2grau$`CNO2� - Casos novos origin�rios de 2� grau`)
CnR2�=verificar(terceiro_2grau,`CNR2� - Casos novos recursais de 2� grau`,
                terceiro_2grau$`CNR2� - Casos novos recursais de 2� grau`)
Cp2�=verificar(terceiro_2grau,`CP2� - Casos pendentes no 2� grau`,
               terceiro_2grau$`CP2� - Casos pendentes no 2� grau`)
PRed2�=verificar(terceiro_2grau,`PRED2� - Processos de 2� grau encaminhados a outra unidade judici�ria por motivo de redistribui��o`,
                 terceiro_2grau$`PRED2� - Processos de 2� grau encaminhados a outra unidade judici�ria por motivo de redistribui��o`)
SuS2�=verificar(terceiro_2grau,`SUS2� - Processos suspensos ou sobrestados ou em arquivo provis�rio no 2� grau`,
                terceiro_2grau$`SUS2� - Processos suspensos ou sobrestados ou em arquivo provis�rio no 2� grau`)
TBaix2�=verificar(terceiro_2grau,`TBAIX2� - Total de processos baixados no 2� grau`,
                  terceiro_2grau$`TBAIX2� - Total de processos baixados no 2� grau`)
PRedR2�=verificar(terceiro_2grau,`PREDR2� - Processos de 2� grau recebidos de outra unidade judici�ria por motivo de redistribui��o`,
                  terceiro_2grau$`PREDR2� - Processos de 2� grau recebidos de outra unidade judici�ria por motivo de redistribui��o`)
Rint2�=verificar(terceiro_2grau,`RINT2� - Recursos internos no 2� grau`,
                 terceiro_2grau$`RINT2� - Recursos internos no 2� grau`)
RintP2�=verificar(terceiro_2grau,`RINTP2� - Recursos internos pendentes no 2� grau`,
                  terceiro_2grau$`RINTP2� - Recursos internos pendentes no 2� grau`)
CartaD2�=verificar(terceiro_2grau,`CARTAD2� - Cartas precat�rias, de ordem e rogat�rias devolvidas`,
                   terceiro_2grau$`CARTAD2� - Cartas precat�rias, de ordem e rogat�rias devolvidas`)
CartaN2�=verificar(terceiro_2grau,`CARTAD2� - Cartas precat�rias, de ordem e rogat�rias devolvidas`,
                   terceiro_2grau$`CARTAD2� - Cartas precat�rias, de ordem e rogat�rias devolvidas`)
VPfG2�=verificar(terceiro_2grau,`VPFGNCRIM2� - Vistas pendentes fora do gabinete em processos n�o criminais no 2� grau`,
                 terceiro_2grau$`VPFGNCRIM2� - Vistas pendentes fora do gabinete em processos n�o criminais no 2� grau`)
VPnG2�=verificar(terceiro_2grau,`VPNGNCRIM2� - Vistas pendentes no gabinete em processos n�o criminais no 2� grau`,
                 terceiro_2grau$`VPNGNCRIM2� - Vistas pendentes no gabinete em processos n�o criminais no 2� grau`)

# Juntando as vari�veis
dados2 = data.frame(terceiro_2grau$Gabinete,
                    CnO2�,
                    CnR2�,
                    Cp2�,
                    PRed2�,
                    SuS2�,
                    TBaix2�,
                    PRedR2�,
                    Rint2�,
                    RintP2�,
                    CartaD2�,
                    CartaN2�,
                    VPfG2�,
                    VPnG2�)

# Vari�veis do 1 Grau
dados2$CartaD1�=NA
dados2$CartaN1�=NA
dados2$CnC1�=NA
dados2$CnExtFisc1�=NA
dados2$CnExtNFisc1�=NA
dados2$CpC1�=NA
dados2$CpExtFisc1�=NA
dados2$CpExtNFisc1�=NA
dados2$ExeJud1�=NA
dados2$ExeJudP1�=NA
dados2$PRedC1�=NA
dados2$PRedExtFisc1�=NA
dados2$PRedExtNFisc1�=NA
dados2$PRedRC1�=NA
dados2$PRedRExtFisc1�=NA
dados2$PRedRExtNFisc1�=NA
dados2$RIntC1�=NA
dados2$RIntCP1�=NA
dados2$SuSC1�=NA
dados2$SuSExFisc1�=NA
dados2$SuSExNfisc1�=NA
dados2$TBaixC1�=NA
dados2$TBaixExtFisc1�=NA
dados2$TBaixExtNFisc1�=NA
dados2$TBaixJud1�=NA

# Adicionando coluna de C�digos e nomes com base no  
colnames(dados2)[1]="�rg�o Estat�stica"
dados2=left_join(dados2, codigos %>% select(`C�digo Serventia`,`�rg�o Estat�stica`))

# Criando colunas
dados2$Mes=mes_atual
dados2$Ano=ano_atual
dados2$Observa��o=NA

# Organizando o banco de dados 1� grau
dados2 = dados2 %>% select(
  `C�digo Serventia`=`C�digo Serventia`,`�rg�o Estat�stica`=`�rg�o Estat�stica`, Mes, Ano, Observa��o,CartaD2�,CartaN2�,CnO2�,CnR2�,Cp2�,
  PRed2�,PRedR2�,Rint2�,RintP2�,SuS2�,TBaix2�,VPfG2�,VPnG2�,
  CartaD1�,CartaN1�,CnC1�,CnExtFisc1�,CnExtNFisc1�,CpC1�,CpExtFisc1�,
  CpExtNFisc1�,ExeJud1�,ExeJudP1�,PRedC1�,PRedExtFisc1�,PRedExtNFisc1�,
  PRedRC1�,PRedRExtFisc1�,PRedRExtNFisc1�,RIntC1�,RIntCP1�,SuSC1�,
  SuSExFisc1�,SuSExNfisc1�,TBaixC1�,TBaixExtFisc1�,TBaixExtNFisc1�,TBaixJud1�)

# codigo que faltou
dados2$`C�digo Serventia`[1] <- 78374

# Juntando ambos os bancos de dados
Terceiro_passo=rbind(dados1,dados2)

# Criando fun��o que converte um vetor de encoding qualquer para encoding latin1
# sendo este necess�rio para vizualiza��o correta no excel
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

# Transformando colunas formadas por strings
Terceiro_passo$`�rg�o Estat�stica`=Converter_em_latin1(Terceiro_passo$`�rg�o Estat�stica`)

# Passando para excel

write.csv(Terceiro_passo, "2021/mar�o/terceiro_passo_saida_mar.csv", na = "")
library(openxlsx)
write.xlsx(Terceiro_passo, "2021/mar�o/terceiro_passo_saida_mar.xls")
# library(WriteXLS)
# WriteXLS(Terceiro_passo, "2021/janeiro/terceiro_passo_saida_mar�o.xlsx")

