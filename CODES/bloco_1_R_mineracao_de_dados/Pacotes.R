# Primeiros Passos na Linguagem R

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/{caminho/no/seu/computador}/bloco_1_R_meneracao_de_dados")
getwd()

# Instalar pacotes separadamente
install.packages("data.table")
install.packages("devtools")
install.packages("dplyr")
install.packages("gapminder")
install.packages('ggplot2')
install.packages("ggvis")
install.packages("gridExtra")
install.packages('lattice')
install.packages("lubridate")
install.packages("mapdata")
install.packages("maps")
install.packages("moments")
install.packages("plotly")
install.packages("plotrix")
install.packages('plyr')
install.packages("quantmod")
install.packages('randomForest')
install.packages("reshape2")
install.packages('rvest')
install.packages("sos")
install.packages('sqldf')
install.packages("tidyr")
install.packages("tm")
install.packages("xts")

# Instalar pacotes em lote
install.packages(c("data.table", "devtools", "dplyr",
			"gapminder", "ggplot2", "ggvis",
			"gridExtra", "lattice", "lubridate",
			"mapdata", "maps", "moments",
			"plotly", "plotrix", "plyr",
			"quantmod", "randomForest", "reshape2",
			"rvest", "sos", "sqldf",
			"tidyr", "tm", "xts"))


# Carregar o pacote
library(ggvis)
library(tm)
require(dplyr)
?require

search()


# Lista o conteúdo dos pacotes
?ls
ls(pos = "package:tm")
ls(getNamespace("tm"), all.names = TRUE)


# Lista as funções de um pacote
lsf.str("package:tm")
lsf.str("package:ggplot2")
library(ggplot2)
lsf.str("package:ggplot2")


# Descarregar o pacote
detach(package:ggplot2)
detach(package:ggvis)
detach(package:tm)
detach(package:dplyr)


# R possui um conjunto de datasets preinstalados. 

library(MASS)
data()

?lynx
head(lynx)
head(iris)
tail(lynx)
summary(lynx)

plot(lynx)
hist(lynx)
head(iris)
iris$Sepal.Length
sum(Sepal.Length)

?attach
attach(iris)
sum(Sepal.Length)


