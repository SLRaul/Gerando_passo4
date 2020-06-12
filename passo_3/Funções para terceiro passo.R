
rm(list = ls())
mes_atual=05
ano_atual=2020
### Terceiro passo ### 
library(dplyr)
library(readxl)
#library(excel.link)
library(readODS)

# setwd(): Muda o diretºrio
# getwd(): Mostra o diretºrio

#Gerar_terceiro_passo=function(mes_tual,ano_atual){

# Buscar Metas
#setwd("X:/SGE/GABINETE/CONSELHO NACIONAL DE JUSTICA/JUSTICA EM NUMEROS/JUSTIºA EM NºMEROS_DADOS ANUAIS/JN ANO 2020/Arquivos Provimento 49 de 18_08_2015/Terceiro Passo/Gerar terceiro passo")
setwd("/home/raulls/R_Diretorio/romi-office-trt") 
terceiro_1grau=read_ods("Terceiro Passo  1 grau.ods")
terceiro_2grau=read_ods("Terceiro Passo 2 grau.ods")

# Planilha dos Códigos
codigos=read_excel("codigos.xls")

# --------------------- Primeiro Grau -------------------- #

# Funººes utilizadas para verificar se uma determinada 
# variºvel estº presente o banco de dados ou não.
library(lazyeval)

# Funººo verificar coluna, retorna True para caso de a coluna 
# existir no Banco de dados (BD), False caso contrºrio.
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

# Variºveis do Quarto passo 1º grau
CartaD1º=verificar(terceiro_1grau,`CARTAD1º - Cartas precatórias, de ordem e rogatórias devolvidas`,
                   terceiro_1grau$`CARTAD1º - Cartas precatórias, de ordem e rogatórias devolvidas`)
CartaN1º=verificar(terceiro_1grau,`CARTAN1º - Cartas precatórias, de ordem e rogatórias que ingressaram no 1º grau`,
                   terceiro_1grau$`CARTAN1º - Cartas precatórias, de ordem e rogatórias que ingressaram no 1º grau`)
CnC1º =verificar(terceiro_1grau,`CNC1º - Casos movos de conhecimento 1º grau`,
                 terceiro_1grau$`CNC1º - Casos movos de conhecimento 1º grau`)
CpC1º =verificar(terceiro_1grau,`CPC1º - Casos pendentes de conhecimento em 1º grau`,
                 terceiro_1grau$`CPC1º - Casos pendentes de conhecimento em 1º grau`)
CpExtFisc1º =verificar(terceiro_1grau,`CPEXTFISC1º - Casos pendentes de execução fiscal no 1º grau`,
                       terceiro_1grau$`CPEXTFISC1º - Casos pendentes de execução fiscal no 1º grau`)
CpExtNFisc1º =verificar(terceiro_1grau,`CPEXTNFISC1º - Casos pendentes de execução de título extrajudicial no 1º grau, exceto execuções fiscais`,
                        terceiro_1grau$`CPEXTNFISC1º - Casos pendentes de execução de título extrajudicial no 1º grau, exceto execuções fiscais`)
ExeJud1º =verificar(terceiro_1grau,`EXEJUD1º - execuções judiciais no 1º grau`,
                    terceiro_1grau$`EXEJUD1º - execuções judiciais no 1º grau`)
ExeJudP1º =verificar(terceiro_1grau, `EXEJUDP1º - execuções judiciais pendentes no 1º grau`,
                     terceiro_1grau$`EXEJUDP1º - execuções judiciais pendentes no 1º grau`)
PRedC1º =verificar(terceiro_1grau,`PREDC1º - Processos de conhecimento no 1º grau encaminhados a outra unidade judiciária por motivo de redistribuição`,
                   terceiro_1grau$`PREDC1º - Processos de conhecimento no 1º grau encaminhados a outra unidade judiciária por motivo de redistribuição`)
PRedRC1º=verificar(terceiro_1grau,`PREDRC1º - Processos de conhecimento no 1º grau recebidos de outra unidade judiciária por motivo de redistribuição`,
                   terceiro_1grau$`PREDRC1º - Processos de conhecimento no 1º grau recebidos de outra unidade judiciária por motivo de redistribuição`)
SuSC1º =verificar(terceiro_1grau,`SUSC1º - Processos de conhecimento suspensos ou sobrestados ou em arquivo provisório no 1º grau`,
                  terceiro_1grau$`SUSC1º - Processos de conhecimento suspensos ou sobrestados ou em arquivo provisório no 1º grau`)
SuSExFisc1º =verificar(terceiro_1grau,`SUSEXFISC1º - execuções fiscais suspensas ou sobrestadas ou em arquivo provisório`,
                       terceiro_1grau$`SUSEXFISC1º - execuções fiscais suspensas ou sobrestadas ou em arquivo provisório`)
SuSExNfisc1º =verificar(terceiro_1grau,`SUSEXNFISC1º - execuções judiciais e extrajudiciais suspensas ou sobrestadas ou em arquivo provisório, exceto execuções fiscais`,
                        terceiro_1grau$`SUSEXNFISC1º - execuções judiciais e extrajudiciais suspensas ou sobrestadas ou em arquivo provisório, exceto execuções fiscais`)
TBaixC1º =verificar(terceiro_1grau,`TBAIXC1º - Processos de conhecimento baixados no 1º grau`,
                    terceiro_1grau$`TBAIXC1º - Processos de conhecimento baixados no 1º grau`)
TBaixExtFisc1º =verificar(terceiro_1grau,`TBAIXEXTFISC1º - Total de processos baixados de execução fiscal no 1º grau`,
                          terceiro_1grau$`TBAIXEXTFISC1º - Total de processos baixados de execução fiscal no 1º grau`)
TBaixExtNFisc1º =verificar(terceiro_1grau,`TBAIXEXTNFISC1º - Total de processos baixados de execução de títulos extrajudiciais no 1º grau, exceto execuções fiscais`,
                           terceiro_1grau$`TBAIXEXTNFISC1º - Total de processos baixados de execução de títulos extrajudiciais no 1º grau, exceto execuções fiscais`)
TBaixJud1º =verificar(terceiro_1grau,`TBAIXJUD1º - Total de processos baixados de execução judicial no 1º grau`,
                      terceiro_1grau$`TBAIXJUD1º - Total de processos baixados de execução judicial no 1º grau`)
RIntC1º=verificar(terceiro_1grau,`RINTC1º - Recursos internos no 1º grau na fase de conhecimento (embargos de declaração)`,
                  terceiro_1grau$`RINTC1º - Recursos internos no 1º grau na fase de conhecimento (embargos de declaração)`)
RIntCP1º=verificar(terceiro_1grau,`RINTCP1º - Recursos internos pendentes no 1º grau na fase de conhecimento (embargos de declaração)`,
                   terceiro_1grau$`RINTCP1º - Recursos internos pendentes no 1º grau na fase de conhecimento (embargos de declaração)`)
CnExtFisc1º=verificar(terceiro_1grau,`CNEXTFISC1º - Casos novos de execução fiscal no 1º grau`,
                      terceiro_1grau$`CNEXTFISC1º - Casos novos de execução fiscal no 1º grau`)
CnExtNFisc1º=verificar(terceiro_1grau,`CNEXTNFISC1º - Casos novos de execução de título extrajudicial no 1º grau, exceto execuções fiscais`,
                       terceiro_1grau$`CNEXTNFISC1º - Casos novos de execução de título extrajudicial no 1º grau, exceto execuções fiscais`)
PRedExtNFisc1º=verificar(terceiro_1grau,`PREDEXTNFISC1º - Processos de execução de título extrajudicial não-fiscais no 1º grau encaminhados a outra unidade judiciária por motivo de redistribuição`,
                         terceiro_1grau$`PREDEXTNFISC1º - Processos de execução de título extrajudicial não-fiscais no 1º grau encaminhados a outra unidade judiciária por motivo de redistribuição`)
PRedRExtNFisc1º=verificar(terceiro_1grau,`PREDEXTNFISC1º - Processos de execução de título extrajudicial não-fiscais no 1º grau recebidos de outra unidade judiciária por motivo de redistribuição`,
                          terceiro_1grau$`PREDEXTNFISC1º - Processos de execução de título extrajudicial não-fiscais no 1º grau recebidos de outra unidade judiciária por motivo de redistribuição`)
PRedExtFisc1º=verificar(terceiro_1grau,`PREDEXTFISC1º - Processos de execução fiscal no 1º grau encaminhados a outra unidade judiciária por motivo de redistribuição`,
                        terceiro_1grau$`PREDEXTFISC1º - Processos de execução fiscal no 1º grau encaminhados a outra unidade judiciária por motivo de redistribuição`)
PRedRExtFisc1º=verificar(terceiro_1grau,`PREDREXTFISC1º - Processos de execução fiscal no 1º grau recebidos de outra unidade judiciária por motivo de redistribuição`,
                         terceiro_1grau$`PREDREXTFISC1º - Processos de execução fiscal no 1º grau recebidos de outra unidade judiciária por motivo de redistribuição`)

dados1=data.frame(`Órgão Estatística`=terceiro_1grau$`Órgão Estatística`, #`Órgão Estatística`,
                  CartaD1º,
                  CartaN1º,
                  CnC1º,
                  CpC1º,
                  CpExtFisc1º,
                  CpExtNFisc1º,
                  ExeJud1º,
                  ExeJudP1º,
                  PRedC1º,
                  PRedRC1º,
                  SuSC1º,
                  SuSExFisc1º,
                  SuSExNfisc1º,
                  TBaixC1º,
                  TBaixExtFisc1º,
                  TBaixExtNFisc1º,
                  TBaixJud1º,
                  RIntC1º,
                  RIntCP1º,
                  CnExtFisc1º,
                  CnExtNFisc1º,
                  PRedExtNFisc1º,
                  PRedRExtNFisc1º,
                  PRedExtFisc1º,
                  PRedRExtFisc1º)

colnames(dados1)[1]="Órgão Estatística"
# Adicionando coluna de Códigos e nomes com base no  
dados1=left_join(dados1,codigos %>% select(`Código Serventia`,`Órgão Estatística`))

# Criando colunas
dados1$Mes=mes_atual
dados1$Ano=ano_atual
dados1$Observação=NA

# Variºveis do 2º grau
dados1$CartaD2º=NA # 2º grau
dados1$CartaN2º=NA # 2º grau
dados1$CnO2º=NA # 2º grau
dados1$CnR2º=NA # 2º grau
dados1$Cp2º=NA # 2º grau
dados1$PRed2º=NA # 2º grau
dados1$PRedR2º=NA # 2º grau
dados1$Rint2º=NA # 2º grau
dados1$RintP2º=NA # 2º grau
dados1$SuS2º=NA # 2º grau
dados1$TBaix2º=NA # 2º grau
dados1$VPfG2º=NA # 2º grau
dados1$VPnG2º=NA # 2º grau

# Organizando o banco de dados 1º grau
dados1 = dados1 %>% select(
  `Código Serventia`=`Código Serventia`,`Órgão Estatística`=`Órgão Estatística`, Mes, Ano, Observação,CartaD2º,CartaN2º,CnO2º,CnR2º,Cp2º,
  PRed2º,PRedR2º,Rint2º,RintP2º,SuS2º,TBaix2º,VPfG2º,VPnG2º,
  CartaD1º,CartaN1º,CnC1º,CnExtFisc1º,CnExtNFisc1º,CpC1º,CpExtFisc1º,
  CpExtNFisc1º,ExeJud1º,ExeJudP1º,PRedC1º,PRedExtFisc1º,PRedExtNFisc1º,
  PRedRC1º,PRedRExtFisc1º,PRedRExtNFisc1º,RIntC1º,RIntCP1º,SuSC1º,
  SuSExFisc1º,SuSExNfisc1º,TBaixC1º,TBaixExtFisc1º,TBaixExtNFisc1º,TBaixJud1º)

# ------------------- Segundo Grau -------------------------- #
## Trabalhando com variºveis do segundo grau
CnO2º=verificar(terceiro_2grau,`CNO2º - Casos novos originários de 2º grau`,
                terceiro_2grau$`CNO2º - Casos novos originários de 2º grau`)
CnR2º=verificar(terceiro_2grau,`CNR2º - Casos novos recursais de 2º grau`,
                terceiro_2grau$`CNR2º - Casos novos recursais de 2º grau`)
Cp2º=verificar(terceiro_2grau,`CP2º - Casos pendentes no 2º grau`,
               terceiro_2grau$`CP2º - Casos pendentes no 2º grau`)
PRed2º=verificar(terceiro_2grau,`PRED2º - Processos de 2º grau encaminhados a outra unidade judiciária por motivo de redistribuição`,
                 terceiro_2grau$`PRED2º - Processos de 2º grau encaminhados a outra unidade judiciária por motivo de redistribuição`)
SuS2º=verificar(terceiro_2grau,`SUS2º - Processos suspensos ou sobrestados ou em arquivo provisório no 2º grau`,
                terceiro_2grau$`SUS2º - Processos suspensos ou sobrestados ou em arquivo provisório no 2º grau`)
TBaix2º=verificar(terceiro_2grau,`TBAIX2º - Total de processos baixados no 2º grau`,
                  terceiro_2grau$`TBAIX2º - Total de processos baixados no 2º grau`)
PRedR2º=verificar(terceiro_2grau,`PREDR2º - Processos de 2º grau recebidos de outra unidade judiciária por motivo de redistribuição`,
                  terceiro_2grau$`PREDR2º - Processos de 2º grau recebidos de outra unidade judiciária por motivo de redistribuição`)
Rint2º=verificar(terceiro_2grau,`RINT2º - Recursos internos no 2º grau`,
                 terceiro_2grau$`RINT2º - Recursos internos no 2º grau`)
RintP2º=verificar(terceiro_2grau,`RINTP2º - Recursos internos pendentes no 2º grau`,
                  terceiro_2grau$`RINTP2º - Recursos internos pendentes no 2º grau`)
CartaD2º=verificar(terceiro_2grau,`CARTAD2º - Cartas precatórias, de ordem e rogatórias devolvidas`,
                   terceiro_2grau$`CARTAD2º - Cartas precatórias, de ordem e rogatórias devolvidas`)
CartaN2º=verificar(terceiro_2grau,`CARTAD2º - Cartas precatórias, de ordem e rogatórias devolvidas`,
                   terceiro_2grau$`CARTAD2º - Cartas precatórias, de ordem e rogatórias devolvidas`)
VPfG2º=verificar(terceiro_2grau,`VPFGNCRIM2º - Vistas pendentes fora do gabinete em processos não criminais no 2º grau`,
                 terceiro_2grau$`VPFGNCRIM2º - Vistas pendentes fora do gabinete em processos não criminais no 2º grau`)
VPnG2º=verificar(terceiro_2grau,`VPNGNCRIM2º - Vistas pendentes no gabinete em processos não criminais no 2º grau`,
                 terceiro_2grau$`VPNGNCRIM2º - Vistas pendentes no gabinete em processos não criminais no 2º grau`)

# Juntando as variºveis
dados2 = data.frame(terceiro_2grau$Gabinete,
                    CnO2º,
                    CnR2º,
                    Cp2º,
                    PRed2º,
                    SuS2º,
                    TBaix2º,
                    PRedR2º,
                    Rint2º,
                    RintP2º,
                    CartaD2º,
                    CartaN2º,
                    VPfG2º,
                    VPnG2º)

# Variºveis do 1 Grau
dados2$CartaD1º=NA
dados2$CartaN1º=NA
dados2$CnC1º=NA
dados2$CnExtFisc1º=NA
dados2$CnExtNFisc1º=NA
dados2$CpC1º=NA
dados2$CpExtFisc1º=NA
dados2$CpExtNFisc1º=NA
dados2$ExeJud1º=NA
dados2$ExeJudP1º=NA
dados2$PRedC1º=NA
dados2$PRedExtFisc1º=NA
dados2$PRedExtNFisc1º=NA
dados2$PRedRC1º=NA
dados2$PRedRExtFisc1º=NA
dados2$PRedRExtNFisc1º=NA
dados2$RIntC1º=NA
dados2$RIntCP1º=NA
dados2$SuSC1º=NA
dados2$SuSExFisc1º=NA
dados2$SuSExNfisc1º=NA
dados2$TBaixC1º=NA
dados2$TBaixExtFisc1º=NA
dados2$TBaixExtNFisc1º=NA
dados2$TBaixJud1º=NA

# Adicionando coluna de Códigos e nomes com base no  
colnames(dados2)[1]="Órgão Estatística"
dados2=left_join(dados2, codigos %>% select(`Código Serventia`,`Órgão Estatística`))

# Criando colunas
dados2$Mes=mes_atual
dados2$Ano=ano_atual
dados2$Observação=NA

# Organizando o banco de dados 1º grau
dados2 = dados2 %>% select(
  `Código Serventia`=`Código Serventia`,`Órgão Estatística`=`Órgão Estatística`, Mes, Ano, Observação,CartaD2º,CartaN2º,CnO2º,CnR2º,Cp2º,
  PRed2º,PRedR2º,Rint2º,RintP2º,SuS2º,TBaix2º,VPfG2º,VPnG2º,
  CartaD1º,CartaN1º,CnC1º,CnExtFisc1º,CnExtNFisc1º,CpC1º,CpExtFisc1º,
  CpExtNFisc1º,ExeJud1º,ExeJudP1º,PRedC1º,PRedExtFisc1º,PRedExtNFisc1º,
  PRedRC1º,PRedRExtFisc1º,PRedRExtNFisc1º,RIntC1º,RIntCP1º,SuSC1º,
  SuSExFisc1º,SuSExNfisc1º,TBaixC1º,TBaixExtFisc1º,TBaixExtNFisc1º,TBaixJud1º)

# Juntando ambos os bancos de dados
Terceiro_passo=rbind(dados1,dados2)

# Criando funººo que converte um vetor de encoding qualquer para encoding latin1
# sendo este necessºrio para vizualizaººo correta no excel
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
Terceiro_passo$`Órgão Estatística`=Converter_em_latin1(Terceiro_passo$`Órgão Estatística`)

# Passando para excel
# xls = xl.get.excel()
# rng = xls[["Activesheet"]]$Cells(1,1)
# xl.write(Terceiro_passo,rng,row.names = FALSE)
#return()
#}

library(WriteXLS)
WriteXLS(Terceiro_passo, "terceiro_passo_saida_maio.xls")
write.csv(Terceiro_passo, "terceiro_passo_saida_maio.csv")
