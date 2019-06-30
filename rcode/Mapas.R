# Mapas

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/{caminho/no/seu/computador}/bloco_3_R_meneracao_de_dados")
getwd()

# Instala os pacotes
install.packages(c("ggplot2", "maps", "mapdata")) # Já instalados
install.packages("dplyr")

# Carrega os pacotes
library(ggplot2)
library(maps)
library(mapdata)
library("dplyr")

# Função para buscar as coordenadas dos países
?map_data
mapa <- map_data("world")

# Visualizando o dataframe
dim(mapa)
View(mapa)

# Gerando o Mapa
ggplot() + geom_polygon(data = mapa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

ggplot() + 
  geom_polygon(data = mapa, aes(x=long, y = lat, group = group), fill = NA, color = "blue") + 
  coord_fixed(1.3)

gg1 <- ggplot() + 
  geom_polygon(data = mapa, aes(x=long, y = lat, group = group), fill = "seagreen1", color = "blue") + 
  coord_fixed(1.3)
gg1

# Marcando alguns pontos no mapa
# Podemos usar um shapefile
labs <- data.frame(
  long = c(69.24140, -2.8456051),
  lat = c(-78.38995, 22.44512),
  names = c("Ponto1", "Ponto2"),
  stringsAsFactors = FALSE
)  

# Pontos no mapa
gg1 + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 2) +
  geom_point(data = labs, aes(x = long, y = lat), color = "yellow", size = 2)



# Divisão por países
ggplot(data = mapa) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  geom_polygon(data = mapa[which(mapa$region == "Brazil"),] ,aes(x = long, y = lat, group = group), fill = "black", color = "black") +
  coord_fixed(1.3) +
  guides(fill=FALSE)

# rMaps 
# http://rmaps.github.io

# função que pinta países selecionados no mapa
fill_selected_region <- function (mapa1, vector_countries, border1, fill1){
  ggplot(data = mapa1) + 
    geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
    geom_polygon(data = mapa[which(mapa$region %in% vector_countries),] ,aes(x = long, y = lat, group = group), fill = fill1, color = border1) +
    coord_fixed(1.3) +
    guides(fill=FALSE)
}

fill_selected_region(mapa,dt_woman$country,"black","black")

# ordenar os feminicídios em ordem desc de total de mortes
### group mtcars by cylinders and return some averages
cars <- mtcars %>%
  select(cyl, mpg, hp, qsec) %>%
  group_by(cyl) %>%
  summarise(mpg = mean(mpg), hp = mean(hp), qsec = mean(qsec))


dt_woman_sorted_numbers <- dt_woman[with(dt_woman, order(-dt_woman$sum.of.selected)), ] 

View(dt_woman_sorted_numbers)  
  
