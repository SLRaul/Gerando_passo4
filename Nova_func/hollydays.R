
Hollydays <- function(municipio, BD_afastamentos, BD_desig){
  library(dplyr)
  library(stringr)
  library(data.table) 
  
  # Importanto o banco de dados de feriados_recesso
  #feriados_recessos <- fread("X:/SGE/GABINETE/CONSELHO NACIONAL DE JUSTICA/JUSTICA EM NUMEROS/JUSTI?A EM N?MEROS_DADOS ANUAIS/JN ANO 2020/Arquivos Provimento 49 de 18_08_2015/Quarto Passo/Gerar quarto passo/Nova fun??o/Feriados_recessos_2020.txt",
   #                          skip = 1, sep = ' ')
  feriados_recessos <- fread("/home/silva/Downloads/romi_ofice/Gerar_passo_4/new_function/Feriados_recessos_2020.txt",
                             skip = 1, sep = ' ')
  #retira os cracters especiais
  feriados_recessos$Municipios <- gsub("_", " ", feriados_recessos$Municipios )

  local_hollydays <- 0
  for (i in 1:nrow(feriados_recessos)) {
    if(i == 1){
      local_hollydays <- feriados_recessos[i,1]
    }else{
      if(str_detect(feriados_recessos[i,2], municipio) == T || str_detect(feriados_recessos[i,2], "TODOS") == T){
        local_hollydays <- rbind(local_hollydays, feriados_recessos[i,1])
      }
    }
  }
  
  # crindao a coluna do no formato da data
  local_hollydays <- data.frame(Datas = local_hollydays$Datas)
  local_hollydays$Data <- ymd(local_hollydays$Datas)
  
  return(local_hollydays$Data)
  
}
