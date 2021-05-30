
# inicialização -----------------------------------------------------------


wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variÃ¡vel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)

getwd()
# 

rm(list = ls())


# Importando bibliotecas --------------------------------------------------

# install.packages("ggplot2")

library("dplyr")
library("glue")
library("ggplot2")

options(repr.plot.width=7, repr.plot.width=4)

# Importando dados --------------------------------------------------------
dados <- read.csv("dados.csv")

sessionInfo()

head(dados, 10)


unique(select(dados, Anos.de.Estudo))

# qualitativas ordinais
arrange(unique(select(dados, Anos.de.Estudo)), Anos.de.Estudo)

# Nominais
arrange(unique(select(dados, UF)), UF)
arrange(unique(select(dados, Cor)), Cor)

# Quantitativas discretas
sprintf('De %s até %s anos', min(dados$Idade), max(dados$Idade))

glue("De {min(dados$Idade)} até {max(dados$Idade)} anos")

# Quantitativa continua
glue("De {min(dados$Altura)} até {max(dados$Altura)} metros")

table(dados$Sexo)
prop.table(table(dados$Sexo)) * 100

dist_freq_qualitativas <- cbind(freq = table(dados$Sexo), percent = prop.table(table(dados$Sexo)) * 100)

colnames(dist_freq_qualitativas) <-  c("Frequencia", "Porcentagem")
rownames(dist_freq_qualitativas) <- c("Masculino", "Feminino")
dist_freq_qualitativas


frequencia <- table(dados$Sexo, dados$Cor)
rownames(frequencia) <- c("Masculino", "Feminino")
colnames(frequencia) <- c("Indigina", "Branca", "Preta", "Amarela", "Parda")
frequencia < cbind(frequencia)
frequencia

percentual <- prop.table(frequencia) * 100
percentual

list(c(1,2,3,4), c(5,6,7))

medias <- tapply(dados$Renda, list(dados$Sexo, dados$Cor), mean)
rownames(medias) <- c("Masculino", "Feminino")
colnames(medias) <- c("Indigina", "Branca", "Preta", "Amarela", "Parda")
medias


classes <- c(0, 1576, 152, 7880, 15760, 200000)
labels <- c('E', 'D', 'C', 'B', 'A')


frequencia <- table(
  cut(x = dados$Renda,
      breaks = classes,
      labels = labels,
      include.lowest = T)  
)
percentual <- prop.table(frequencia) * 100
percentual

dist_freq_quantitativas_personalizadas <-
  cbind("Frequencia" = frequencia, 'Porcentagem  ' = percentual)
dist_freq_quantitativas_personalizadas[order(row.names(dist_freq_quantitativas_personalizadas)),]


# Regra de Sturges
n <- nrow(dados)
k <- 1 + (10 /3) * log10(n)
k <- round(k,0)
k

# Histograma
hist(dados$Altura)

mean(dados$Renda)
aggregate(list("Renda" = dados$Renda), list("Sexo" = dados$Sexo), mean)

quantile(dados$Renda, c(0.25, 0.5, 0.75))

decis <- c()
for(i in 1:9) {
  decis <- c(decis, i/10)
}
quantile(dados$Renda, decis)


centis <- c()
for(i in 1:99) {
  centis <- c(centis, i/100)
}
quantile(dados$Renda, centis)


length(dados$Renda[dados$Renda <= 788/2]) / length(dados$Renda) * 100

