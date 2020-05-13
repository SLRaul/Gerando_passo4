#rm(list=ls())


periodo_trabalhado_afastamento <- function(data_entrada, BD_afastamentos, BD_desig, dias_mes ){
  library(dplyr) #manipular os dados # nao deu
  library(stringr) # manipularstrings
  library(lubridate)
  
  #gerando um periodo observado
  periodo_corrente <- as.Date(data_entrada, format= "%d/%m/%Y" ) + days(0:(dias_mes-1)) #data inicial mais 30 dias
  
  
  #data das f?rias/afastamento
  inicio_f <- BD_afastamentos$inicio_afast
  fim_f <- BD_afastamentos$fim_afast
  
  
  data_ferias <- interval(inicio_f,fim_f)
  periodo_ferias <- data_ferias/ddays(1)
  
  #periodo_ferias <- (inicio_f[1]) + days(0:periodo_ferias[1])
  tempo_trabalhado <- 0
  for (k in 1:(length(periodo_ferias))) {
    
    periodo_afastado <- (inicio_f[k]) + days(0:periodo_ferias[k]) #k
    periodo_corrente <- as.Date(data_entrada, format= "%d/%m/%Y" ) + days(0:30) #data inicial mais 30 dias
    
    
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
  BD_afastamentos$tempo_afastado <- dias_mes - BD_afastamentos$tempo_trabalhado 
  
  # retirando os repetidos
  #BD_afastamentos <- BD_afastamentos %>%  distinct(nome_magis, .keep_all = T)
  
  return(BD_afastamentos)
}# a fun??o est? entregando avisos


# data_entrada <- "01/03/2020"
# dias_mes <- 31
# periodo_trabalhado_afastamento(data_entrada, BD_afastamentos, BD_desig, dias_mes)



