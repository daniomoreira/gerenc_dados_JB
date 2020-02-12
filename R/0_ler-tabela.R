
# Script para ler tabelas

## os dados originais estão em ex_04_original.xlxs

# Para descobrir a pasta de trabalho
getwd()

# Para definir a pasta de trabalho (se precisar, pois aqui, não é necessário)
# setwd("./data")


# Para ler uma tabela csv

?read.table
# ou
help(read.table)

Tab_medidas <- read.csv ("./data/ex_04_formatado.csv", header = T, sep = ";")
Tab_medidas

# Fim
