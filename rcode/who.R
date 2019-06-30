# Mineração de dados

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
par (bg = "lightgray")
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
dt_woman <- dt[, which(dt$sex == "F")]
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
print(proporcao_mulheres)

histplot(murder_tot,  "Total de assassinatos por sexo", 
         c("F","M"), "Sexo", "Total", "red")

# total de mortos por conjugue
# total de mulheres mortas na base
death_married <- c("Y07","Y070") 
dt_woman1 <- dt[which(dt$sex == "F" & dt$cause.details %in% death_married), ]
sum_f <- sum(dt_woman1$sum.of.selected)
print(sum_f) 

# total de homens mortos
dt_man1 <- dt[which(dt$sex == "M" & dt$cause.details %in% death_married), ]
sum_m <- sum(dt_man1$sum.of.selected) 
print(sum_m)
murder_tot <- c(sum_f,sum_m)

histplot(murder_tot,  "Assassinatos causados por conjugue", 
         c("Mulheres","Homens"), "Total", "Sexo", "red", c("blue","pink"))

# total de mortos por conhecidos
# total de mulheres mortas na base
death_know <- c("Y07","Y070", "Y071","Y072") 
dt_woman2 <- dt[which(dt$sex == "F" & dt$cause.details %in% death_know), ]
sum_f <- sum(dt_woman2$sum.of.selected)
print(sum_f) 
# total de homens mortos
dt_man2 <- dt[which(dt$sex == "M" & dt$cause.details %in% death_know), ]
sum_m <- sum(dt_man2$sum.of.selected) 
print(sum_m)
murder_tot <- c(sum_f,sum_m)

histplot(murder_tot,  "Assassinatos causados por conhecidos", 
         c("Mulheres","Homens"), "Total", "Sexo", "red", c("blue","pink"))
  