# Mineração de dados

setwd("/home/r/codigos_curso_r/")
getwd()
dt <- data.frame(read.csv(file = 'who.csv', header = TRUE, sep = ","))
dt$X5to9

# Remoção de linhas que não interessam as análises e aumentam o custo de trabalho
# do Dataset
nomes_col <- c("ICD.rev.and.list", "format","unknown")
dt <- dt[ , -which(names(dt) %in% nomes_col), drop = F]
sum (table(dt$sex))

# Criação de um subset com as linhas que tem as causas de morte de feminicidio 
# Série X85 to Y09 (Assault)
death_code <- c("X85","X86","X87","X88","X89","X90","X91","X92","X93","X94","X95",
                "X96","X97","X98","X99","Y00","Y01","Y02","Y03","Y04","Y05","Y06",
                "Y060","Y061","Y062","Y068","Y069","Y07","Y070","Y071","Y072",
                "Y073","Y078","Y079","Y08","Y09") 
dt <- dt[which(dt$cause.details %in% death_code), ]

# dataset do World Bank que tem a população dos países
wb <- data.frame(read.csv(file = 'worldb.csv', header = TRUE, sep = ","))

# Função de plotagem
par (bg = "lightblue")
boxplota <- function(dataframe, coly, colx, ylab1, xlab1, title) {
  boxplot(data = dataframe, coly ~ colx ,
          main = title ,
          col.main = "red", ylab = ylab1 , xlab = xlab1)
}
histplot <- function(columns, title1, names1, ylab1, xlab1, border1, colours) {
  barplot(columns, main = title1, names.arg = names1,
           xlab = xlab1, ylab = ylab1 , border = border1, col = colours)
}

# Subset 
death_married <- c("Y07","Y070") 
dm <- dt[which(dt$cause.details %in% death_married), ]
boxplota(dm, dm$sum.of.selected, dm$sex, "Quantidade de mortes",
         "Sexo", "Total de assassinatos por conjugue")

# total de mulheres mortas na base
dt_woman <- dt[which(dt$sex == "F"), ]
sum_f <- sum(dt_woman$sum.of.selected)
print(sum_f) # 21582 mulheres
# total de homens mortos
dt_man <- dt[which(dt$sex == "M"), ]
sum_m <- sum(dt_man$sum.of.selected) 
print(sum_m) # 108065 homens
murder_tot <- c(sum_f,sum_m)

# Total relativo de mulheres e homens na base
proporcao_mulheres = sum_f / (sum_m + sum_f)
proporcao_homens = sum_m / (sum_m + sum_f)
proporcao_por_sexo = sum_f / sum_m
print(proporcao_mulheres) # 16,64%  mulheres (21582) 
# 21582 85,46% homens
print(proporcao_por_sexo) # Mulheres correspondem a apenas 19% da presença masculina
# na base


histplot(murder_tot,  "Total de assassinatos por sexo", 
         c("F","M"), "Sexo", "Total", "red")

# total de mortos por conjugue
# total de mulheres mortas na base
death_married <- c("Y07","Y070","Y06","Y060") 
dt_woman1 <- dt[which(dt$sex == "F" & dt$cause.details %in% death_married), ]
sum_f1 <- sum(dt_woman1$sum.of.selected)
print(sum_f) 

# total de homens mortos
dt_man1 <- dt[which(dt$sex == "M" & dt$cause.details %in% death_married), ]
sum_m1 <- sum(dt_man1$sum.of.selected) 
print(sum_m1)
murder_tot1 <- c(sum_f1,sum_m1)

par ( bg = "gray" )
histplot(murder_tot1,  "Assassinatos causados por conjugue no mundo entre 2006 e 2016", 
         c("mulheres","homens"), "mortes", "sexo", "gray", c("pink","lightblue"))

# total de mortos por conhecidos
# total de mulheres mortas na base
death_know <- c("Y07","Y070", "Y071","Y072", "Y060","Y06", "Y061","Y062") 
dt_woman2 <- dt[which(dt$sex == "F" & dt$cause.details %in% death_know), ]
sum_f2 <- sum(dt_woman2$sum.of.selected)
print(sum_f2) 
# total de homens mortos
dt_man2 <- dt[which(dt$sex == "M" & dt$cause.details %in% death_know), ]
sum_m2 <- sum(dt_man2$sum.of.selected) 
print(sum_m2)
murder_tot2 <- c(sum_f2,sum_m2)
# Considerando a p

histplot(murder_tot2,  "Assassinatos causados por conhecidos no mundo", 
         c("Mulheres","Homens"), "Total", "Sexo", "red", c("navy","pink"))
# Considerando a proporção em há 5 vezes mais homens na base o total de 
# assassinatos de mulheres seria 5 vezes maior do que o de homens

# Por negligência e abandono
# total de mulheres mortas na base
death_neg <- c("Y07","Y070", "Y071","Y072") 
dt_woman3 <- dt[which(dt$sex == "F" & dt$cause.details %in% death_neg), ]
sum_f3 <- sum(dt_woman3$sum.of.selected) # 398
print(sum_f3) 
# total de homens mortos
dt_man3 <- dt[which(dt$sex == "M" & dt$cause.details %in% death_know), ]
sum_m3 <- sum(dt_man3$sum.of.selected)  # 361
print(sum_m3)
murder_tot3 <- c(sum_f3,sum_m3)

histplot(murder_tot3,  "Assassinatos causados por negligência no mundo", 
         c("Mulheres","Homens"), "Total", "Sexo", "red", c("blue","pink"))

# Total de casos de estupro
# total de mulheres mortas na base
death_rape <- c("Y05") 
dt_woman4 <- dt[which(dt$sex == "F" & dt$cause.details %in% death_rape), ]
sum_f4 <- sum(dt_woman4$sum.of.selected) # 398
print(sum_f4) # 158
# total de homens mortos
dt_man4 <- dt[which(dt$sex == "M" & dt$cause.details %in% death_know), ]
sum_m4 <- sum(dt_man4$sum.of.selected)  # 361
print(sum_m4) # 361
murder_tot4 <- c(sum_f4,sum_m4)

histplot(murder_tot4,  "Assassinatos causados por estupro no mundo", 
         c("mulheres","homens"), "total", "sexo", "red", c("blue","pink"))
# 158 para mulheres contra 361 para homens

# Total de mulheres e homens na base por idade 
#child_woman = c(sum(dt_woman$less1, na.rm =T), sum(dt_woman$X2,na.rm = T),
                #sum(dt_woman$X3,na.rm = T), sum(dt_woman$X4, na.rm= T),
                #sum(dt_woman$X5to9, na.rm = T)) #crianças
#child_woman = sum(child_woman, na.rm = TRUE)

#child_man = c(sum(dt_man$less1, na.rm =T), sum(dt_man$X2,na.rm = T),
                #sum(dt_man$X3,na.rm = T), sum(dt_man$X4, na.rm= T),
                #sum(dt_man$X5to9, na.rm = T)) #crianças de 1 a 9 anos
#child_man = sum(child_man, na.rm = TRUE)

#teen_woman = c(sum(dt_woman$X10to14, na.rm =T), sum(dt_woman$X15to19,na.rm = T))
#adolescentes de 10 a 19
#teen_woman = sum(teen_woman, na.rm = TRUE)

# teen_man = c(sum(dt_man$X10to14, na.rm =T), sum(dt_man$X15to19,na.rm = T))
#adolescentes
#teen_man = sum(teen_man, na.rm = TRUE)

# Progressão de tempo assassinatos
# criando um vetor com os totais de casa ano por sexo
woman_2006 = dt_woman[which(dt_woman$year == 2006),]
woman_2007 = dt_woman[which(dt_woman$year == 2007),]
woman_2008 = dt_woman[which(dt_woman$year == 2008),]
woman_2009 = dt_woman[which(dt_woman$year == 2009),]
woman_2010 = dt_woman[which(dt_woman$year == 2010),]
woman_2011 = dt_woman[which(dt_woman$year == 2011),]
woman_2012 = dt_woman[which(dt_woman$year == 2012),]
woman_2013 = dt_woman[which(dt_woman$year == 2013),]
woman_2014 = dt_woman[which(dt_woman$year == 2014),]
woman_2015 = dt_woman[which(dt_woman$year == 2015),]
woman_2016 = dt_woman[which(dt_woman$year == 2016),]

woman_years = c(sum(woman_2006$sum.of.selected, na.rm = T), sum(woman_2007$sum.of.selected, na.rm = T),
                sum(woman_2008$sum.of.selected, na.rm = T), sum(woman_2009$sum.of.selected, na.rm = T),
                sum(woman_2012$sum.of.selected, na.rm = T), sum(woman_2013$sum.of.selected, na.rm = T),
                sum(woman_2014$sum.of.selected, na.rm = T), sum(woman_2015$sum.of.selected, na.rm = T),
                sum(woman_2016$sum.of.selected, na.rm = T))

par (bg = "pink")
histplot(woman_years, "Assassinatos de mulheres no mundo entre 2006 e 2016", 
         years, "total", "anos", "black", c("black"))

man_2006 = dt_man[which(dt_man$year == 2006),]
man_2007 = dt_man[which(dt_man$year == 2007),]
man_2008 = dt_man[which(dt_man$year == 2008),]
man_2009 = dt_man[which(dt_man$year == 2009),]
man_2010 = dt_man[which(dt_man$year == 2010),]
man_2011 = dt_man[which(dt_man$year == 2011),]
man_2012 = dt_man[which(dt_man$year == 2012),]
man_2013 = dt_man[which(dt_man$year == 2013),]
man_2014 = dt_man[which(dt_man$year == 2014),]
man_2015 = dt_man[which(dt_man$year == 2015),]
man_2016 = dt_man[which(dt_man$year == 2016),]

man_years = c(sum(man_2006$sum.of.selected, na.rm = T), sum(man_2007$sum.of.selected, na.rm = T),
                sum(man_2008$sum.of.selected, na.rm = T), sum(man_2009$sum.of.selected, na.rm = T),
                sum(man_2010$sum.of.selected, na.rm = T),  sum(man_2011$sum.of.selected, na.rm = T),  
                sum(man_2012$sum.of.selected, na.rm = T), sum(man_2013$sum.of.selected, na.rm = T),
                sum(man_2014$sum.of.selected, na.rm = T), sum(man_2015$sum.of.selected, na.rm = T),
                sum(man_2016$sum.of.selected, na.rm = T))

par (bg = "lightyellow")

histplot(man_years, "Assassinatos de homens no mundo entre 2006 e 2016", 
         years, "total", "anos", "black", c("black"))
  
years_d = matrix (, ncol = 11, nrow = 2)  
years_d[1,] = woman_years
years_d[2,] = man_years

colnames(years_d) = years
rownames(years_d) = c("F","M")

par ( bg = "lightgray" )
barplot(years_d,
        main = "Comparativo de assassinatos no mundo por ano",
        xlab = "anos",
        ylab = "mortes",
        col = c("pink","lightblue"),

)
legend("topright",
       c("Mulheres","Homens"),
       fill = c("pink","lightblue")
)
  