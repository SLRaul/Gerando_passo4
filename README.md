# Gerando_passos
A pasta "Novo_passo4" contem quatro funções, em que juntas,calculam os dias trabalhados com restrição de feriados municipais.
Com a atualização de maio foi retirado a função "Hollydays" visto que os dias trabalhados eram referentes apenas em relação as designações, sendo diminuidas apenas quando o magistrado possui periodo de férias.

Em resumo:

a função "Hollydays" seleciona as datas referentes a feriados/recessos de uma determinada região (Não sendo mais usada desde atualização de maio de 2020);

a função "aplication_time_function" contabiliza o período afastado/férias do funcionário baseado na sua designação;

a função "time_function_desig" que concatena os resultados de afastamento/férias já calculado na função "aplicatio_time_fuction"
e calcula os dias trabalhados dos funcioários que foram/tiveram afastados/férias como dos que não foram/tiveram afastados/férias;

e a função "Nova função para o quarto passo" é que faz toda os ajustes dos dados para serem usando nas funções antreriores e toda a preparação final para a construção da tabela final.

Atualmente essa função funciona para um determinado contexto, contudo a idéia é, com o tempo, torna-lá para mais geral.
