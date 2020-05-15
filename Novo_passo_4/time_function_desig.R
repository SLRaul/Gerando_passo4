

periodo_trabalhado <- function(data_inicial, data_final, dias_mes, BD_desig ){
  # procurando a fun??o periodo_trabalhado_afastamento
  #source("X:/SGE/GABINETE/CONSELHO NACIONAL DE JUSTICA/JUSTICA EM NUMEROS/JUSTI?A EM N?MEROS_DADOS ANUAIS/JN ANO 2020/Arquivos Provimento 49 de 18_08_2015/Quarto Passo/Gerar quarto passo/Nova fun??o/aplication_time_function.R")
  source("C:/Users/silva/Documents/Repositorio/Gerando_passos/Novo_passo_4/aplication_time_function.R")
  #colocando o tempo trabalhado dos magistrados afastados
  BD_afastamentos <- periodo_trabalhado_afastamento(data_inicial, BD_afastamentos, BD_desig, dias_mes)
  
  # colancando o tempo trabalhado de quem teve afastamento
  BD_desig <- left_join(BD_desig, BD_afastamentos %>% select(nome_magis, tempo_trabalhado))#[,c(1,6)])
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
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # # # # # # #  ajeitar a partir daqui  # # # # # # # 
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  
  #ajustando os dias para a fun??o
  dias_mes <- dias_mes - 1
  
  #data das designa??es
  inicio_d <- as.Date(BD_desig$inicio_desig)
  fim_d <- as.Date(BD_desig$fim_desig)
  
  data_designado <- interval(inicio_d,fim_d)
  dias_designado <- data_designado/ddays(1) +1
  
  
  for (i in 1:nrow(BD_desig)) {
    if(is.na(BD_desig$tempo_trabalhado[i]) == T){
      BD_desig$tempo_trabalhado[i] <- dias_designado[i]
    }
  }
  #BD_desig$Dias_de_Afastamento <- max(BD_desig$tempo_trabalhado) - BD_desig$tempo_trabalhado
  tabela <- list()
  tabela$afastamento <- BD_afastamentos %>% select(nome_magis, Descrição, tempo_afastado) #[,c(1,7)]
  tabela$desig <- left_join(BD_desig, BD_serventias, "nome_serventia_desig") %>%
    select(nome_magis, inicio_desig, fim_desig, codigo_VT, nome_serventia_sicond,Tipo_magis, tempo_trabalhado)
  return(tabela)
}



#data inicial do mes de refer?ncia
# data_inicial <- ("01/03/2020")
# data_final <- dmy("31/03/2020")
# dias_mes <- 31
# 
# #o retor ser? em rela??o as dias trabalhados
# com <-Sys.time()
# oi <- periodo_trabalhado(data_inicial, data_final, dias_mes, BD_desig )
# fim <- Sys.time()
# fim-com

#write.xlsx(oi, "jobdays.xlsx")
