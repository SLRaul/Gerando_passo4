#rm(list=ls())


periodo_trabalhado_afastamento <- function(data_entrada, BD_afastamentos, BD_desig ){
  library(dplyr) #manipular os dados # nao deu
  library(stringr) # manipularstrings
  library(lubridate)
  
  #gerando um periodo observado
  periodo_corrente <- as.Date(data_entrada, format= "%d/%m/%Y" ) + days(0:30) #data inicial mais 30 dias
  
  
  # fun??o que ja puxa os dados e cria o calend?rio para cara da linha
  #source("X:/SGE/GABINETE/CONSELHO NACIONAL DE JUSTICA/JUSTICA EM NUMEROS/JUSTI?A EM N?MEROS_DADOS ANUAIS/JN ANO 2020/Arquivos Provimento 49 de 18_08_2015/Quarto Passo/Gerar quarto passo/Nova fun??o/hollydays.R")
  source("/home/silva/Downloads/romi_ofice/Gerar_passo_4/new_function/hollydays.R")
  # colocando as serventias para saber o municipio
  BD_afastamentos <- left_join(BD_afastamentos, BD_desig[,c(1,4)]) 
  BD_afastamentos <- BD_afastamentos  %>% distinct(nome_magis, inicio_afast, .keep_all = T)
  # retirando os caracters expeciais
  BD_afastamentos$nome_serventia_desig <- iconv(BD_afastamentos$nome_serventia_desig, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  #data das f?rias/afastamento
  inicio_f <- BD_afastamentos$inicio_afast
  fim_f <- BD_afastamentos$fim_afast
  
  data_ferias <- interval(inicio_f,fim_f)
  periodo_ferias <- data_ferias/ddays(1)
  
  #periodo_ferias <- (inicio_f[1]) + days(0:periodo_ferias[1])
  
  tempo_trabalhado <- 0
  for (k in 1:(length(periodo_ferias))) {
    # para mais de um afastamento
    if(is.na(BD_afastamentos$nome_magis[k]) == T){ #se o indice for vazio sai do la?o
      break()}
    # decidindo qual municipio o magistrado trabalha #machine_learning
    if(substring(BD_afastamentos$nome_serventia_desig[k], 1, 3) == "GAB" ||
       str_detect(BD_afastamentos$nome_serventia_desig[k], "SEC") == T ||
       str_detect(BD_afastamentos$nome_serventia_desig[k], "FORTALEZA") == T ||
       str_detect(BD_afastamentos$nome_serventia_desig[k], "PRESIDENCIA") ==T ||
       str_detect(BD_afastamentos$nome_serventia_desig[k], "VICE-PRESIDENCIA") ==T ||
       str_detect(BD_afastamentos$nome_serventia_desig[k], "CENTRO JUDICIARIO") ==T ||
       is.na(BD_afastamentos$nome_serventia_desig[k]) ==T ){
      municipio <- "FORTALEZA"}else{
        if(str_detect(BD_afastamentos$nome_serventia_desig[k], "CAUCAIA") == T){
          municipio <- "CAUCAIA"}else{
            if(str_detect(BD_afastamentos$nome_serventia_desig[k], "MARACANAU") == T){
              municipio <- "MARACANAU"}else{
                if(str_detect(BD_afastamentos$nome_serventia_desig[k], "MARACANAU") == T){
                  municipio <- "MARACANAU"}else{
                    if(str_detect(BD_afastamentos$nome_serventia_desig[k], "REGIAO DO CARIRI") == T){
                      municipio <- "JUAZEIRO DO NORTE"}else{
                        if(str_detect(BD_afastamentos$nome_serventia_desig[k], "SOBRAL") == T){
                          municipio <- "SOBRAL"}else{
                            if(str_detect(BD_afastamentos$nome_serventia_desig[k], "ARACATI") == T){
                              municipio <- "ARACATI"}else{
                                if(str_detect(BD_afastamentos$nome_serventia_desig[k], "BATURITE") == T){
                                  municipio <- "BATURITE"}else{
                                    if(str_detect(BD_afastamentos$nome_serventia_desig[k], "CRATEUS") == T){
                                      municipio <- "CRATEUS"}else{
                                        if(str_detect(BD_afastamentos$nome_serventia_desig[k], "IGUATU") == T){
                                          municipio <- "IGUATU"}else{
                                            if(str_detect(BD_afastamentos$nome_serventia_desig[k], "LIMOEIRO DO NORTE") == T){
                                              municipio <- "LIMOEIRO DO NORTE"}else{
                                                if(str_detect(BD_afastamentos$nome_serventia_desig[k], "PACAJUS") == T){
                                                  municipio <- "PACAJUS"}else{
                                                    if(str_detect(BD_afastamentos$nome_serventia_desig[k], "QUIXADA") == T){
                                                      municipio <- "QUIXADA"}else{
                                                        if(str_detect(BD_afastamentos$nome_serventia_desig[k], "SAO GONCALO") == T){
                                                          municipio <- "SAO GONCALO"}else{
                                                            if(str_detect(BD_afastamentos$nome_serventia_desig[k], "TIANGUA") == T){
                                                              municipio <- "TIANGUA"}else{
                                                                if(str_detect(BD_afastamentos$nome_serventia_desig[k], "TIANGUA") == T){
                                                                  municipio <- "EUSEBIO"}
                                                              }
                                                          }
                                                      }
                                                  }
                                              }
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
    
    #fun??o que retona ao as f?rias referentes ao mun?cio de refer?ncia
    data_feriados <- Hollydays(municipio, BD_afastamentos, BD_desig)
    
    
    periodo_afastado <- (inicio_f[k]) + days(0:periodo_ferias[k]) #k
    periodo_corrente <- as.Date(data_entrada, format= "%d/%m/%Y" ) + days(0:30) #data inicial mais 30 dias
    
    for (i in 1:length(periodo_corrente)) {
      
      for (j in 1:length(data_feriados)) {# removendo os feriados e recessos
        #print(j)
        if(periodo_corrente[i] == data_feriados[j]){
          periodo_corrente <- periodo_corrente[-i]}
        
      }
      if(is.na(periodo_corrente[i+1]) == T) #se o proximo indice for vazio sai do la?o
        break()
    }
    
    for (i in 1:(length(periodo_corrente)) ) {
      
      for (l in 1:length(periodo_afastado)) {# removendo as  f?rias # variavel, diferente para cada "afastamento"
        if(is.na(periodo_corrente[i]) == T){ #se o proximo indice for vazio sai do la?o
          break()}
        if(periodo_corrente[i] == periodo_afastado[l]){
          periodo_corrente <- periodo_corrente[-i]}
      }
      
    }
    
    #k=2
    #verificando se uma pessoa tem mais de um afastamento
    if(k != 1){#pulando o primeiro
      if(BD_afastamentos$nome_magis[k] == BD_afastamentos$nome_magis[k-1]){
        periodo_corrente <- intersect(aux, periodo_corrente)
      }
    }
    if( k!=(length(periodo_ferias))){ #pulando o ultimo
      if(BD_afastamentos$nome_magis[k] == BD_afastamentos$nome_magis[k+1]){
        aux <- periodo_corrente
      }    
    }
    tempo_trabalhado[k] <- length(periodo_corrente)
    
  }
  
  BD_afastamentos <- cbind(BD_afastamentos, tempo_trabalhado)
  
  # retirando os repetidos
  BD_afastamentos <- BD_afastamentos %>%  distinct(nome_magis, .keep_all = T)
  
  return(BD_afastamentos)
}# a fun??o est? entregando avisos


#data_entrada <- "01/02/2020"
#periodo_trabalhado_afastamento(data_entrada, BD_afastamentos, BD_desig)



