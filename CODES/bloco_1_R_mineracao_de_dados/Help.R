# Help

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/{caminho/no/seu/computador}/bloco_1_R_meneracao_de_dados")
getwd()


# Se souber o nome da função
help(mean)
?mean


# Para buscar mais opções sobre uma função, use o pacote SOS
install.packages("sos") # Já instalado
library(sos)
findFn("fread")


# Se não souber o nome da função
help.search('randomForest')
help.search('matplot')
??matplot
RSiteSearch('matplot')
example('matplot')


# Sair
q()



