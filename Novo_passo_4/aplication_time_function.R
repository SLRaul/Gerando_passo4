#rm(list=ls())


periodo_trabalhado_afastamento <- function(data_entrada, BD_afastamentos, BD_desig, dias_mes ){
  library(dplyr) #manipular os dados # nao deu
  library(stringr) # manipularstrings
  library(lubridate)
  
  #gerando um periodo observado
  periodo_corrente <- data_entrada + days(0:(dias_mes-1)) #data inicial mais 30 dias
  
  #tranformando em datas
  BD_afastamentos$inicio_afast <- as.Date(BD_afastamentos$inicio_afast)
  BD_afastamentos$fim_afast <- as.Date(BD_afastamentos$fim_afast)
 
  
  # Ajustando os limites
  for(i in 1:length(BD_afastamentos$inicio_afast)){
    if(month((BD_afastamentos$inicio_afast[i])) < month(data_entrada)){
      BD_afastamentos$inicio_afast[i] <- data_entrada
    }
    if(year((BD_afastamentos$fim_afast[i])) > year(data_final)){
      BD_afastamentos$fim_afast[i] <- data_final
    }
    if(month((BD_afastamentos$fim_afast[i])) > month(data_final)){
      BD_afastamentos$fim_afast[i] <- data_final
    }
  }
  
  
  #data das f?rias/afastamento
  inicio_f <- BD_afastamentos$inicio_afast
  fim_f <- BD_afastamentos$fim_afast
  
  
  data_ferias <- interval(inicio_f,fim_f)
  periodo_ferias <- data_ferias/ddays(1)
  
  # organizando o vetor com as descrições de afastamento para cada linha
  BD_afastamentos=BD_afastamentos %>%
    mutate(Descrição=paste0(MOTIVO," DE ",day(inicio_afast),"/",month(inicio_afast),"/",year(inicio_afast),
                            " ATÉ ",day(fim_afast),"/",month(fim_afast),"/",year(fim_afast)))
  
   
  BD_afastamentos$tempo_afastado <- NA
  BD_afastamentos$tempo_trabalhado <- NA
  for (i in 1: nrow(BD_afastamentos)){
    
    # calculando tempo de férias de cada linha
    BD_afastamentos$tempo_afastado[i] <- periodo_ferias[i] +1
    
    # organizando o vetor com as descrições de afastamento para cada magistrado
    if(is.na(BD_afastamentos$nome_magis[i+1]) == T){
      break()
    }else{
      if(BD_afastamentos$nome_magis[i] == BD_afastamentos$nome_magis[i+1]){
        BD_afastamentos$Descrição[i] <- paste0(BD_afastamentos$Descrição[i], " - ", BD_afastamentos$Descrição[i+1])
        #print(paste0(BD_afastamentos$Descrição[i], " - ", BD_afastamentos$Descrição[i+1]))
      }
    }
  }
  # somando as ferias
  tempo_afastado <- aggregate(BD_afastamentos$tempo_afastado,
            by=list(BD_afastamentos$nome_magis),
            FUN=sum)$x
  # retirando os repetidos
  BD_afastamentos <- BD_afastamentos %>%  distinct(nome_magis, .keep_all = T)
  BD_afastamentos$tempo_afastado <- tempo_afastado
  # calculando o tempo trabalhado
  BD_afastamentos$tempo_trabalhado <- dias_mes - BD_afastamentos$tempo_afastado
  
  BD_afastamentos <- BD_afastamentos %>% select(-MOTIVO )
  return(BD_afastamentos)
}# a fun??o est? entregando avisos


# data_entrada <- "01/03/2020"
# dias_mes <- 31
# periodo_trabalhado_afastamento(data_entrada, BD_afastamentos, BD_desig, dias_mes)



