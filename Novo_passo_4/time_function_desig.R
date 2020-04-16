setwd("/home/silva/Downloads/romi_ofice")

periodo_trabalhado <- function(data_inicial, data_final, dias_mes, BD_desig ){
  # procurando a fun??o periodo_trabalhado_afastamento
  #source("X:/SGE/GABINETE/CONSELHO NACIONAL DE JUSTICA/JUSTICA EM NUMEROS/JUSTI?A EM N?MEROS_DADOS ANUAIS/JN ANO 2020/Arquivos Provimento 49 de 18_08_2015/Quarto Passo/Gerar quarto passo/Nova fun??o/aplication_time_function.R")
  source("/home/silva/Downloads/romi_ofice/Gerar_passo_4/new_function/aplication_time_function.R")
  #colocando o tempo trabalhado dos magistrados afastados
  BD_afastamentos <- periodo_trabalhado_afastamento(data_inicial, BD_afastamentos, BD_desig, dias_mes)
  
  # colancando o tempo trabalhado de quem teve afastamento
  BD_desig <- left_join(BD_desig, BD_afastamentos[,c(1,6)])
  #BD_desig$nome_serventia_desig <- iconv(BD_desig$nome_serventia_desig, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  # Ajustando os limites
  for(i in 1:length(BD_desig$inicio_desig)){
    if(BD_desig$inicio_desig[i] < data_inicial){
      BD_desig$inicio_desig[i] <- data_inicial
    }
    if(BD_desig$fim_desig[i] > data_final){
      BD_desig$fim_desig[i] <- data_final
    }
  }
  
  #ajustando os dias para a fun??o
  dias_mes <- dias_mes - 1
  
  #data das designa??es
  inicio_d <- as.Date(BD_desig$inicio_desig)
  fim_d <- as.Date(BD_desig$fim_desig)
  
  data_designado <- interval(inicio_d,fim_d)
  dias_designado <- data_designado/ddays(1)
  
  tempo_designado <- 0
  ###### aqui vai ter um for #####
  
  for (k in 1:(length(dias_designado))) {
    #k=6
    if(is.na(BD_desig$nome_magis[k]) == T){ #se o indice for vazio sai do la?o
      break()}
    # decidindo qual municipio o magistrado trabalha #machine_learning
    if(substring(BD_desig$nome_serventia_desig[k], 1, 3) == "GAB" ||
       str_detect(BD_desig$nome_serventia_desig[k], "SEC") == T ||
       str_detect(BD_desig$nome_serventia_desig[k], "FORTALEZA") == T ||
       str_detect(BD_desig$nome_serventia_desig[k], "PRESIDENCIA") ==T ||
       str_detect(BD_desig$nome_serventia_desig[k], "VICE-PRESIDENCIA") ==T ||
       str_detect(BD_desig$nome_serventia_desig[k], "CENTRO JUDICIARIO") ==T ||
       is.na(BD_desig$nome_serventia_desig[k]) ==T ){
      municipio <- "FORTALEZA"}else{
        if(str_detect(BD_desig$nome_serventia_desig[k], "CAUCAIA") == T){
          municipio <- "CAUCAIA"}else{
            if(str_detect(BD_desig$nome_serventia_desig[k], "MARACANAU") == T){
              municipio <- "MARACANAU"}else{
                if(str_detect(BD_desig$nome_serventia_desig[k], "MARACANAU") == T){
                  municipio <- "MARACANAU"}else{
                    if(str_detect(BD_desig$nome_serventia_desig[k], "REGIAO DO CARIRI") == T){
                      municipio <- "JUAZEIRO DO NORTE"}else{
                        if(str_detect(BD_desig$nome_serventia_desig[k], "SOBRAL") == T){
                          municipio <- "SOBRAL"}else{
                            if(str_detect(BD_desig$nome_serventia_desig[k], "ARACATI") == T){
                              municipio <- "ARACATI"}else{
                                if(str_detect(BD_desig$nome_serventia_desig[k], "BATURITE") == T){
                                  municipio <- "BATURITE"}else{
                                    if(str_detect(BD_desig$nome_serventia_desig[k], "CRATEUS") == T){
                                      municipio <- "CRATEUS"}else{
                                        if(str_detect(BD_desig$nome_serventia_desig[k], "IGUATU") == T){
                                          municipio <- "IGUATU"}else{
                                            if(str_detect(BD_desig$nome_serventia_desig[k], "LIMOEIRO DO NORTE") == T){
                                              municipio <- "LIMOEIRO DO NORTE"}else{
                                                if(str_detect(BD_desig$nome_serventia_desig[k], "PACAJUS") == T){
                                                  municipio <- "PACAJUS"}else{
                                                    if(str_detect(BD_desig$nome_serventia_desig[k], "QUIXADA") == T){
                                                      municipio <- "QUIXADA"}else{
                                                        if(str_detect(BD_desig$nome_serventia_desig[k], "SAO GONCALO") == T){
                                                          municipio <- "SAO GONCALO"}else{
                                                            if(str_detect(BD_desig$nome_serventia_desig[k], "TIANGUA") == T){
                                                              municipio <- "TIANGUA"}else{
                                                                if(str_detect(BD_desig$nome_serventia_desig[k], "TIANGUA") == T){
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
    
    
    periodo_designado <- (inicio_d[k]) + days(0:dias_designado[k]) #k
    periodo_corrente <- as.Date(data_inicial, format= "%d/%m/%Y" ) + days(0:dias_mes) #data inicial mais 30 dias
    
    for (i in 1:length(periodo_corrente)) {
      
      for (j in 1:length(data_feriados)) {# removendo os feriados e recessos
        #print(j)
        if(periodo_corrente[i] == data_feriados[j]){
          periodo_corrente <- periodo_corrente[-i]}
        
      }
      if(is.na(periodo_corrente[i+1]) == T) #se o proximo indice for vazio sai do la?o
        break()
    }
    
    periodo_corrente_row <-0
    t <- 0
    for (i in 1:(length(periodo_corrente)) ) { #################### 
      
      for (l in 1:length(periodo_designado)) {# removendo as  f?rias # variavel, diferente para cada "afastamento"
        if(is.na(periodo_corrente[i]) == T){ #se o proximo indice for vazio sai do la?o
          break()}else{
            if(periodo_corrente[i] == periodo_designado[l]){
              t<- t+1
              periodo_corrente_row[t] <- as.Date(as.character(periodo_corrente[i]))
              break()}
          }
        
      }
      
    }
    
    tempo_designado[k] <- length(periodo_corrente_row)
  }
  
  for (i in 1:nrow(BD_desig)) {
    if(is.na(BD_desig$tempo_trabalhado[i]) == T){
      BD_desig$tempo_trabalhado[i] <- tempo_designado[i]
    }
  }
  #BD_desig$Dias_de_Afastamento <- max(BD_desig$tempo_trabalhado) - BD_desig$tempo_trabalhado
  tabela <- list()
  tabela$afastamento <- BD_afastamentos[,c(1,7)]
  tabela$desig <- left_join(BD_desig, BD_serventias, "nome_serventia_desig") %>%
    select(nome_magis, inicio_desig, fim_desig, codigo_VT, nome_serventia_sicond,Tipo_magis, tempo_trabalhado)
  return(tabela)
}


#data inicial do mes de refer?ncia
# data_inicial <- ("01/03/2020")
# data_final <- dmy("31/03/2020")
# dias_mes <- 31

#o retor ser? em rela??o as dias trabalhados
# com <-Sys.time()
# oi <- periodo_trabalhado(data_inicial, data_final, dias_mes, BD_desig )
# fim <- Sys.time()
# fim-com

#write.xlsx(oi, "jobdays.xlsx")
