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

###### Tabela das conciliações ######
list_table_concil <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Conciliações-por-VT-mês-am-ês/Conciliações por VT mês am ês",
                        pattern = "*.xlsx",full.names=T,) %>% 
  lapply(read_excel) %>%  bind_rows

#
list_table_concil$`Qtde-não-Conciliações` <- list_table_concil$`Qtde-Total`- list_table_concil$`Qtde-Conciliações` 

#adicionado a coluna referante ao mes
Mes <- rep(c(1:12), each=37)
# juntando o mes a tabela final
list_table_concil <- cbind(list_table_concil, Mes )

list_table_concil <- list_table_concil %>% select(- `Descrição da Região Judiciária`)

# Ajustando a soma do trt7 de cada mes
TRT7_1_instancia <- list_table_concil %>% group_by(Mes) %>% summarise(Qtde_Conciliações = sum(`Qtde-Conciliações`),
                                                  `Percentual de Conciliação` = 100,
                                                  Qtde_Total = sum(`Qtde-Total`),
                                                  Qtde_não_Conciliações = sum(`Qtde-não-Conciliações`))

TRT7_1_instancia$`Vara Trabalhista` <- rep("TRT7 1 instância", 12)

TRT7_1_instancia <- TRT7_1_instancia %>% select(`Vara Trabalhista`, `Qtde-Conciliações`=Qtde_Conciliações ,
                                                `Percentual de Conciliação`=`Percentual de Conciliação`,
                                                `Qtde-Total`=Qtde_Total, `Qtde-não-Conciliações`= Qtde_não_Conciliações, Mes)

# Juntando as tabelas que er por vara com as as totais do trt7
list_table_concil <- rbind(list_table_concil, TRT7_1_instancia)

list_table_concil <- list_table_concil %>% mutate(mes = ifelse(Mes%in%c(1), "Jan", 
                                          ifelse(Mes%in%c(2), 'Fev', 
                                           ifelse(Mes%in%c(3), "Mar",
                                            ifelse(Mes%in%c(4), "Abril",
                                             ifelse(Mes%in%c(5), "Maio",
                                              ifelse(Mes%in%c(6), "Jun",
                                               ifelse(Mes%in%c(7), "Jul",
                                                ifelse(Mes%in%c(8), "Ago",
                                                 ifelse(Mes%in%c(9), "Set",
                                                  ifelse(Mes%in%c(10), "Out",
                                                   ifelse(Mes%in%c(11), "Nov","Dez"))))))))))))

write.xlsx(list_table_concil, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_concil.xlsx")




###### Recebidos por Região Judiciária ######
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

# Ajustando a soma do trt7 de cada mes
TRT7_1_receb <- list_table_receb %>% group_by(Mes) %>% summarise(`Qtde-Casos Novos por Distribuição-(A)` = sum(`Qtde-Casos Novos por Distribuição-(A)`),
                                                                `Casos Novos por Distribuição(%)` = 100,
                                                                `Qtde-Redistribuídos-(B)` = sum(`Qtde-Redistribuídos-(B)`),
                                                                `Redistribuídos(%)` = 100,
                                                                `Qtde_Sentença reformada ou anulada(C)` = sum(`Qtde_Sentença reformada ou anulada(C)`),
                                                                `Sentença reformada ou anulada(%)` = 100,
                                                                `Qtde Total(D)` = sum(`Qtde Total(D)`),
                                                                `Total(%)` = 100)

TRT7_1_receb$`Vara Trabalhista` <- rep("TRT7 1 instância", 12)

TRT7_1_receb <- TRT7_1_receb %>% select(`Vara Trabalhista`, `Qtde-Casos Novos por Distribuição-(A)`,
                                        `Casos Novos por Distribuição(%)`, `Qtde-Redistribuídos-(B)`,
                                        `Redistribuídos(%)`, `Qtde_Sentença reformada ou anulada(C)`,
                                        `Sentença reformada ou anulada(%)`, `Qtde Total(D)`,
                                        `Total(%)`, Mes)

# Juntando as tabelas que er por vara com as as totais do trt7
list_table_receb <- rbind(list_table_receb, TRT7_1_receb)

list_table_receb <- list_table_receb %>% mutate(mes = ifelse(Mes%in%c(1), "Jan", 
                                                               ifelse(Mes%in%c(2), 'Fev', 
                                                                      ifelse(Mes%in%c(3), "Mar",
                                                                             ifelse(Mes%in%c(4), "Abril",
                                                                                    ifelse(Mes%in%c(5), "Maio",
                                                                                           ifelse(Mes%in%c(6), "Jun",
                                                                                                  ifelse(Mes%in%c(7), "Jul",
                                                                                                         ifelse(Mes%in%c(8), "Ago",
                                                                                                                ifelse(Mes%in%c(9), "Set",
                                                                                                                       ifelse(Mes%in%c(10), "Out",
                                                                                                                              ifelse(Mes%in%c(11), "Nov","Dez"))))))))))))

write.xlsx(list_table_receb, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_receb.xlsx")




###### Solucionados por VT ######
list_table_Soluci <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Solucionados por VT",
                                pattern = "*.xlsx",full.names=T) %>% 
  lapply(read_excel) %>%  bind_rows

#adicionado a coluna referante ao mes
Mes <- rep(c(04,08,12,02,01,07,06,05,03,11,10,09), each=37)
# juntando o mes a tabela final
list_table_Soluci <- cbind(list_table_Soluci, Mes )

list_table_Soluci <- list_table_Soluci %>% select(-UF)

# Ajustando a soma do trt7 de cada mes
TRT7_1_Soluci <- list_table_Soluci %>% group_by(Mes) %>% summarise(`Quantidade - Com exame de mérito - Solucionados` = sum(`Quantidade - Com exame de mérito - Solucionados`),
                                                                 `Com exame de mérito - Solucionados - %` = 100,
                                                                 `Quantidade - Sem exame de mérito - Solucionados...5` = sum(`Quantidade - Sem exame de mérito - Solucionados...5`),
                                                                 `Sem exame de mérito - Solucionados - %...6` = 100,
                                                                 `Quantidade - Sem exame de mérito - Solucionados...7` = sum(`Quantidade - Sem exame de mérito - Solucionados...7`),
                                                                 `Sem exame de mérito - Solucionados - %...8` = 100)

TRT7_1_Soluci$`Vara do Trabalho` <- rep("TRT7 1 instância", 12)

TRT7_1_Soluci <- TRT7_1_Soluci %>% select(`Vara do Trabalho`,`Quantidade - Com exame de mérito - Solucionados`,
                                        `Com exame de mérito - Solucionados - %`,`Quantidade - Sem exame de mérito - Solucionados...5`,
                                        `Sem exame de mérito - Solucionados - %...6`, `Quantidade - Sem exame de mérito - Solucionados...7`,
                                        `Sem exame de mérito - Solucionados - %...8`, Mes)

# Juntando as tabelas que er por vara com as as totais do trt7
list_table_Soluci <- rbind(list_table_Soluci, TRT7_1_Soluci)

list_table_Soluci <- list_table_Soluci %>% mutate(mes = ifelse(Mes%in%c(1), "Jan", 
                                                             ifelse(Mes%in%c(2), 'Fev', 
                                                                    ifelse(Mes%in%c(3), "Mar",
                                                                           ifelse(Mes%in%c(4), "Abril",
                                                                                  ifelse(Mes%in%c(5), "Maio",
                                                                                         ifelse(Mes%in%c(6), "Jun",
                                                                                                ifelse(Mes%in%c(7), "Jul",
                                                                                                       ifelse(Mes%in%c(8), "Ago",
                                                                                                              ifelse(Mes%in%c(9), "Set",
                                                                                                                     ifelse(Mes%in%c(10), "Out",
                                                                                                                            ifelse(Mes%in%c(11), "Nov","Dez"))))))))))))

write.xlsx(list_table_Soluci, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_Soluci.xlsx")




###### Valores Totais aos Reclamantes por VT ######
list_table_Reclam <- list.files(path = "D:/romi_ofice/BI/TrabalhoNovoBI/Valores Totais aos Reclamantes por VT",
                                pattern = "*.xlsx",full.names=T) %>% 
  lapply(read_excel) %>%  bind_rows

#adicionado a coluna referante ao mes
Mes <- rep(c(04,08,12,02,01,07,06,05,03,11,10,09), each=37)
# juntando o mes a tabela final
list_table_Reclam <- cbind(list_table_Reclam, Mes )

list_table_Reclam <- list_table_Reclam %>% select(-`Região Judiciária`)

# Ajustando a soma do trt7 de cada mes
TRT7_1_Reclam <- list_table_Reclam %>% group_by(Mes) %>% summarise(`Decorrentes de Execução` = sum(`Decorrentes de Execução`, na.rm = T),
                                                                   `Decorrentes de Acordo` = sum(`Decorrentes de Acordo`, na.rm = T),
                                                                   `Decorrentes de Pagamento Espontâneo` = sum(`Decorrentes de Pagamento Espontâneo`, na.rm = T),
                                                                   Total = sum(Total, na.rm = T))

TRT7_1_Reclam$`Descrição da Vara/Foro` <- rep("TRT7 1 instância", 12)

TRT7_1_Reclam <- TRT7_1_Reclam %>% select(`Descrição da Vara/Foro`,`Decorrentes de Execução`,
                                          `Decorrentes de Acordo`, `Decorrentes de Pagamento Espontâneo`,
                                          Total, Mes)

# Juntando as tabelas que er por vara com as as totais do trt7
list_table_Reclam <- rbind(list_table_Reclam, TRT7_1_Reclam)

list_table_Reclam <- list_table_Reclam %>% mutate(mes = ifelse(Mes%in%c(1), "Jan", 
                                                               ifelse(Mes%in%c(2), 'Fev', 
                                                                      ifelse(Mes%in%c(3), "Mar",
                                                                             ifelse(Mes%in%c(4), "Abril",
                                                                                    ifelse(Mes%in%c(5), "Maio",
                                                                                           ifelse(Mes%in%c(6), "Jun",
                                                                                                  ifelse(Mes%in%c(7), "Jul",
                                                                                                         ifelse(Mes%in%c(8), "Ago",
                                                                                                                ifelse(Mes%in%c(9), "Set",
                                                                                                                       ifelse(Mes%in%c(10), "Out",
                                                                                                                              ifelse(Mes%in%c(11), "Nov","Dez"))))))))))))

write.xlsx(list_table_Reclam, "D:/romi_ofice/BI/TrabalhoNovoBI/list_table_Reclam.xlsx")
