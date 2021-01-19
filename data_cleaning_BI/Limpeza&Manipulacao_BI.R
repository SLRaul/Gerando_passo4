rm(list = ls())
# read a list of tables and join in one table
library(dplyr)
library(readxl)
library(openxlsx)

# lendo a lista de tabelas e unido-as em uma
# list.files() -> copia todos os arquivos(do padrão escolhido) da pasta que o caminho leva
# lapply() -> aplica a função de leitura de tabelas (read_excel) em cada caminho salvo no passo anterior
# bind_rows -> uni as tabelas pelas linhas

## Tabela das conciliações ##
list_table_concil <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Conciliações-por-VT-mês-am-ês",
                        pattern = "*.xlsx",full.names=T,) %>% 
  lapply(read_excel) %>%  bind_rows

#adicionado a coluna referante ao mes
Mes <- rep(c(04,08,12,02,01,07,06,05,03,11,10,09), each=37)
# juntando o mes a tabela final
list_table_concil <- cbind(list_table_concil, Mes )

write.xlsx(list_table_concil, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_concil.xlsx")




## Recebidos por Região Judiciária ##
list_table_receb <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Recebidos por Região Judiciária",
                                pattern = "*.xlsx",full.names=T,) %>% 
  lapply(read_excel) %>%  bind_rows

#adicionado a coluna referante ao mes
Mes <- rep(c(04,08,12,02,01,07,06,05,03,11,10,09), each=37)
# juntando o mes a tabela final
list_table_receb <- cbind(list_table_receb, Mes )

write.xlsx(list_table_receb, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_receb.xlsx")




## Solucionados por VT ##
list_table_Soluci <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Solucionados por VT",
                                pattern = "*.xlsx",full.names=T) %>% 
  lapply(read_excel) %>%  bind_rows

#adicionado a coluna referante ao mes
Mes <- rep(c(04,08,12,02,01,07,06,05,03,11,10,09), each=37)
# juntando o mes a tabela final
list_table_Soluci <- cbind(list_table_Soluci, Mes )

write.xlsx(list_table_Soluci, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_Soluci.xlsx")




## Valores Totais aos Reclamantes por VT ##
list_table_Reclam <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Valores Totais aos Reclamantes por VT",
                                pattern = "*.xlsx",full.names=T) %>% 
  lapply(read_excel) %>%  bind_rows

#adicionado a coluna referante ao mes
Mes <- rep(c(04,08,12,02,01,07,06,05,03,11,10,09), each=37)
# juntando o mes a tabela final
list_table_Reclam <- cbind(list_table_Reclam, Mes )

write.xlsx(list_table_Reclam, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_Reclam.xlsx")
