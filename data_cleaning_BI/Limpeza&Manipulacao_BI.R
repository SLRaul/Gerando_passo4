rm(list = ls())
# read a list of tables and join in one table
library(dplyr)
library(readxl)
library(openxlsx)
library(dplyr)

# lendo a lista de tabelas e unido-as em uma
# list.files() -> copia todos os arquivos(do padr�o escolhido) da pasta que o caminho leva
# lapply() -> aplica a fun��o de leitura de tabelas (read_excel) em cada caminho salvo no passo anterior
# bind_rows -> uni as tabelas pelas linhas

## Tabela das concilia��es ##
list_table_concil <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Concilia��es-por-VT-m�s-am-�s",
                        pattern = "*.xlsx",full.names=T,) %>% 
  lapply(read_excel) %>%  bind_rows

#adicionado a coluna referante ao mes
Mes <- rep(c(04,08,12,02,01,07,06,05,03,11,10,09), each=37)
# juntando o mes a tabela final
list_table_concil <- cbind(list_table_concil, Mes )

list_table_concil <- list_table_concil %>% select(- `Descri��o da Regi�o Judici�ria`)

write.xlsx(list_table_concil, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_concil.xlsx")




## Recebidos por Regi�o Judici�ria ##
list_table_receb <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Recebidos por Regi�o Judici�ria",
                                pattern = "*.xlsx",full.names=T,) %>% 
  lapply(read_excel) %>%  bind_rows

#adicionado a coluna referante ao mes
Mes <- rep(c(04,08,12,02,01,07,06,05,03,11,10,09), each=37)
# juntando o mes a tabela final
list_table_receb <- cbind(list_table_receb, Mes )

list_table_receb <- list_table_receb %>% select(-UF, -`N�mero do Org�o (Estat�stica)`, -`Data da �ltima remessa`)

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

list_table_Reclam <- list_table_Reclam %>% select(-`Regi�o Judici�ria`)

write.xlsx(list_table_Reclam, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_Reclam.xlsx")