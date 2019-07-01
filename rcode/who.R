# Mineração de dados

setwd("/home/r/codigos_curso_r/")
getwd()
dt <- data.frame(read.csv(file = 'who.csv', header = TRUE, sep = ","))

# Remoção de linhas que não interessam as análises e aumentam o custo de trabalho
# do Dataset
nomes_col <- c("ICD.rev.and.list", "format","unknown")
dt <- dt[ , -which(names(dt) %in% nomes_col), drop = F]
sum (table(dt$sex))

# Criação de um subset com as linhas que tem as causas de morte de feminicidio 
# Série X85 to Y09 (Assault)
death_code <- c("X85","X850","X851","X852","X853","X854","X855","X856","X857","X859","X86","X860","X87","X870",
                "X88","X880","X881","X882","X883","X884","X885","X886","X887","X888","X89",
                "X890","X90","X901","X902","X903","X904","X905","X906","X907","X908","X909",
                "X91","X910","X911","X912","X913","X914","X915","X916","X917",
                "X92","X920","X921","X922","X923","X924","X925","X926","X927","X928","X929",
                "X93","X930","X931","X932","X933","X934","X935","X933","X937","X938","X939",
                "X94","X940","X941","X942","X943","X944","X945","X946","X947","X948","X949",
                "X95","X950","X951","X952","X953","X954","X955","X956","X957","X958","X959",
                "X96","X960","X961","X962","X963","X964","X965","X966","X967","X968","X969",
                "X97","X970","X971","X972","X973","X974","X975","X976","X977","X978","x979",
                "X98","X980","X981","X982","X983","X984","X985","X986","X987","X988","X989",
                "X99","X990","X991","X992","X993","X994","X995","X996","X997","X998","X999",
                "Y00","Y000","Y001","Y002","Y003","Y004","Y005","Y006","Y007","Y008","Y009",
                "Y01","Y010","Y011","Y012","Y013","Y014","Y015","Y016","Y017","Y018","Y019",
                "Y02","Y020","Y021","Y022","Y023","Y024","Y025","Y026","Y027","Y028","Y029",
                "Y03","Y030","Y031","Y032","Y033","Y034","Y035","Y036","Y037","Y038","Y039",
                "Y04","Y040","Y041","Y042","Y043","Y044","Y045","Y046","Y047","Y048","Y049",
                "Y05","Y050","Y051","Y052","Y053","Y054","Y055","Y056","Y057","Y058","Y539",
                "Y06","Y060","Y061","Y062","Y633","Y064","Y065","Y066","Y067","Y068","Y069",
                "Y07","Y070","Y071","Y072","Y073","Y074","Y075","Y076","Y077","Y078","Y079",
                "Y08","Y080","Y081","Y082","Y683","Y084","Y085","Y086","Y087","Y088","Y089",
                "Y09","Y090","Y091","Y092","Y933","Y094","Y069","Y096","Y097","Y098","Y099") 
dt <- dt[which(dt$cause.details %in% death_code), ]

# dataset do World Bank que tem a população dos países
wb <- data.frame(read.csv(file = 'worldb.csv', header = TRUE, sep = ","))

# Função de plotagem
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
death_married <- c("Y07","Y070","Y060","Y06") 
boxplota(dt, dt$sum.of.selected, dt$sex, "Quantidade de mortes",
         "Sexo", "Total de assassinatos na base")

# total de mulheres mortas na base
dt_woman <- dt[which(dt$sex == "F"), ] 
print(nrow(dt)) # 29475
print(nrow(dt_woman)) # 11580
print(nrow(dt_man)) # 17895
sum_f <- sum(dt_woman$sum.of.selected)
print(sum_f) # 21582 mulheres
# total de homens mortos
dt_man <- dt[which(dt$sex == "M"), ]
sum_m <- sum(dt_man$sum.of.selected) 
print(sum_m) # 108065 homens
murder_tot <- c(sum_f,sum_m)
print(sum_f+sum_m)

# Total relativo de mulheres e homens na base
proporcao_mulheres = sum_f / (sum_m + sum_f)
proporcao_homens = sum_m / (sum_m + sum_f)
proporcao_por_sexo = sum_f / sum_m
print(proporcao_mulheres) # 11,44%  mulheres na base (
# 21582 85,46% homens
print(proporcao_por_sexo) # Mulheres correspondem a apenas 12.92% da presença masculina
# na base

histplot(murder_tot,  "Total de assassinatos por sexo", 
         c("F","M"), "Sexo", "Total","darkgrey","darkgrey")

# total de mortos por conjugue
# total de mulheres mortas na base
death_married <- c("Y06","Y060",
                   "Y07","Y070") 
dt_woman1 <- dt[which(dt$sex == "F" & dt$cause.details %in% death_married), ]
sum_f1 <- sum(dt_woman1$sum.of.selected)
print(sum_f1) 

# total de homens mortos
dt_man1 <- dt[which(dt$sex == "M" & dt$cause.details %in% death_married), ]
sum_m1 <- sum(dt_man1$sum.of.selected) 
print(sum_m1)
murder_tot1 <- c(sum_f1,sum_m1)

histplot(murder_tot1,  "Mortes causadas por conjugue no mundo", 
         c(paste("mulheres"," - ",sum_f1, "casos \n",round(proporcao_por_sexo*100),"% do homens na base"),paste("homens"," - ",sum_m1, "casos \n",floor(100-(proporcao_por_sexo*100)),"% a mais")), "mortes", "sexo", "#8A0829", c("#8A0829", "#DF013A"))

# total de mortos por conhecidos
# total de mulheres mortas na base
death_know <- c("Y072", "Y062") 
dt_woman2 <- dt[which(dt$sex == "F" & dt$cause.details %in% death_know), ]
sum_f2 <- sum(dt_woman2$sum.of.selected)
print(sum_f2) 
# total de homens mortos
dt_man2 <- dt[which(dt$sex == "M" & dt$cause.details %in% death_know), ]
sum_m2 <- sum(dt_man2$sum.of.selected) 
print(sum_m2)
murder_tot2 <- c(sum_f2,sum_m2)
# Considerando a p

par( bg= "white")
histplot(murder_tot2,  "Mortes causadas por conhecidos no mundo", 
       c(paste("mulheres","-",sum_f2, " casos","\n ",floor(proporcao_mulheres*100),"% dos homens"),paste("homens","-",sum_m2," casos", "\n", round(100-proporcao_mulheres*100),"% mais")), "mortes", "sexo", "#8A0829", c("#8A0829", "#DF013A"))
# Considerando a proporção em há 5 vezes mais homens na base o total de 
# assassinatos de mulheres seria 5 vezes maior do que o de homens

# Por negligência e abandono
# total de mulheres mortas na base
death_neg <- c("Y06","Y060", "Y061","Y062","Y068","Y069") 
dt_woman3 <- dt[which(dt$sex == "F" & dt$cause.details %in% death_neg), ]
sum_f3 <- sum(dt_woman3$sum.of.selected) # 398
print(sum_f3) 
# total de homens mortos
dt_man3 <- dt[which(dt$sex == "M" & dt$cause.details %in% death_know), ]
sum_m3 <- sum(dt_man3$sum.of.selected)  # 361
print(sum_m3)
murder_tot3 <- c(sum_f3,sum_m3)

histplot(murder_tot3,  "Mortes causadas por negligência no mundo", 
         c("Mulheres","Homens"), "mortes", "sexo",  "#8A0829", c("#8A0829", "#DF013A"))

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

par(bg = "white")
histplot(murder_tot4,  "Assassinatos causados por estupro no mundo", 
         c(paste("mulheres","-",sum_f4, "casos","\n",round((sum_f4/sum_m4)* 100)," % dos casos masculinos"),paste("homens","-",sum_m4, "casos")), "mortes", "sexo", "#8A0829", c("#DF013A","#8A0829"))
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
                sum(woman_2010$sum.of.selected, na.rm = T), sum(woman_2011$sum.of.selected, na.rm = T), 
                sum(woman_2012$sum.of.selected, na.rm = T), sum(woman_2013$sum.of.selected, na.rm = T),
                sum(woman_2014$sum.of.selected, na.rm = T), sum(woman_2015$sum.of.selected, na.rm = T),
                sum(woman_2016$sum.of.selected, na.rm = T))

years = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015",
          "2016")

View(woman_years)

histplot(woman_years, "Assassinatos de mulheres no mundo entre 2006 e 2016", 
         years, "total", "anos", "#8A0829", c("#8A0829"))

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


histplot(man_years, "Assassinatos de homens no mundo entre 2006 e 2016", 
         years, "mortes", "anos", "black", c("black"))
  
years_d = matrix (, ncol = 11, nrow = 2)  
years_d[1,] = woman_years
years_d[2,] = man_years

colnames(years_d) = years
rownames(years_d) = c("F","M")

par ( bg = "white" )
barplot(years_d,
        main = "Comparativo de assassinatos no mundo por ano",
        xlab = "anos",
        ylab = "mortes",
        col = c("#DF013A","#8A0829"),

)
legend("topright",
       c("Mulheres","Homens"),
       fill = c("#DF013A","#8A0829")
)

# média da idade dos homens e mulheres no dataset

# casamentos infantis
death_married <- c("Y07","Y070","Y060","Y06") 
print(death_married)
dt_woman_child_married <- dt_woman[which(dt_woman$cause.details %in% death_married 
                                   & (dt_woman$less1 > 0 | dt_woman$X1 > 0 |
                                        dt_woman$X2 > 0 | dt_woman$X3 > 0 |
                                        dt_woman$X4 > 0 | dt_woman$X5to9 > 0 |
                                        dt_woman$X10to14 > 0)),]

dt_man_child_married <- dt_man[which(dt_man$cause.details %in% death_married 
                                         & (dt_man$less1 > 0 | dt_man$X1 > 0 |
                                              dt_man$X2 > 0 | dt_man$X3 > 0 |
                                              dt_man$X4 > 0 | dt_man$X5to9 > 0 |
                                              dt_man$X10to14 > 0)),]

# Países com mais casamentos infantis

dt_child_married <- dt[which(dt$cause.details %in% death_married 
                                     & (dt$less1 > 0 | dt$X1 > 0 |
                                          dt$X2 > 0 | dt$X3 > 0 |
                                          dt$X4 > 0 | dt$X5to9 > 0 |
                                          dt$X10to14 > 0)),]

countries_top_woman_marriage <- unique(dt_woman_child_married$country)
View(dt_child_married)  


