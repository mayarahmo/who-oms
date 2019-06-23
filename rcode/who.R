# Mineração de dados

getwd()
dt <- data.frame(read.csv(file = 'who.csv', header = TRUE, sep = ","))
View(dt$cause.details)

dt$X5to9

# Remoção de linhas que não interessam as análises e aumentam o custo de trabalho
# do Dataset
nomes_col <- c("ICD.rev.and.list", "format")
dt <- dt[ , -which(names(dt) %in% nomes_col), drop = F]

  