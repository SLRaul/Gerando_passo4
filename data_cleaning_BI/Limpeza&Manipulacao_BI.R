rm(list = ls())
# read a list of tables and join in one table
library(dplyr)
library(readxl)
library(openxlsx)
library(dplyr)

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

list_table_concil <- list_table_concil %>% select(- `Descrição da Região Judiciária`)

write.xlsx(list_table_concil, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_concil.xlsx")




## Recebidos por Região Judiciária ##
list_table_receb <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Recebidos por Região Judiciária",
                                pattern = "*.xlsx",full.names=T,) %>% 
  lapply(read_excel) %>%  bind_rows

#adicionado a coluna referante ao mes
Mes <- rep(c(04,08,12,02,01,07,06,05,03,11,10,09), each=37)
# juntando o mes a tabela final
list_table_receb <- cbind(list_table_receb, Mes )

list_table_receb <- list_table_receb %>% select(-UF, -`Número do Orgão (Estatística)`, -`Data da última remessa`)
list_table_receb$`Casos Novos por Distribuição(%)` <- list_table_receb$`Casos Novos por Distribuição(%)`*100
list_table_receb$`Redistribuídos(%)` <- list_table_receb$`Redistribuídos(%)`*100
list_table_receb$`Sentença reformada ou anulada(%)` <- list_table_receb$`Sentença reformada ou anulada(%)`*100
list_table_receb$`Total(%)` <- list_table_receb$`Total(%)`*100

write.xlsx(list_table_receb, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_receb.xlsx")




## Solucionados por VT ##
list_table_Soluci <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Solucionados por VT",
                                pattern = "*.xlsx",full.names=T) %>% 
  lapply(read_excel) %>%  bind_rows

#adicionado a coluna referante ao mes
Mes <- rep(c(04,08,12,02,01,07,06,05,03,11,10,09), each=37)
# juntando o mes a tabela final
list_table_Soluci <- cbind(list_table_Soluci, Mes )

list_table_Soluci <- list_table_Soluci %>% select(-UF)

write.xlsx(list_table_Soluci, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_Soluci.xlsx")




## Valores Totais aos Reclamantes por VT ##
list_table_Reclam <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Valores Totais aos Reclamantes por VT",
                                pattern = "*.xlsx",full.names=T) %>% 
  lapply(read_excel) %>%  bind_rows

#adicionado a coluna referante ao mes
Mes <- rep(c(04,08,12,02,01,07,06,05,03,11,10,09), each=37)
# juntando o mes a tabela final
list_table_Reclam <- cbind(list_table_Reclam, Mes )

list_table_Reclam <- list_table_Reclam %>% select(-`Região Judiciária`)

write.xlsx(list_table_Reclam, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_Reclam.xlsx")
