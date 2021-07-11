
# Carregar bibliotecas ----------------------------------------------------


wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())

options(scipen=999)

library(moments)
library(dplyr)
library(readxl)

consorcio <- read_excel("base1/Consorcio.xlsx", sheet = "Base de Dados")

# Analise Exploratoria ----------------------------------------------------
summary(consorcio)

table(consorcio$Contratou)
prop.table(table(consorcio$Contratou))

# (b) Faca a analise bivariada das variaveis explicativas (covariaveis) versus variavel resposta. 

par(mfrow = c(1, 5))
boxplot(consorcio$DI ~consorcio$Contratou, col = "darkturquoise", main = "DI")
boxplot(consorcio$Financiamento ~consorcio$Contratou, col = "darkturquoise", main = "Financiamento")
boxplot(consorcio$Poupanca ~consorcio$Contratou, col = "darkturquoise", main = "Poupanca")
boxplot(consorcio$Salario ~consorcio$Contratou, col = "darkturquoise", main = "Salario")
boxplot(consorcio$CC ~consorcio$Contratou, col = "darkturquoise", main = "CC")

# (c) Obtenha o modelo de regressao logistica, utilizando 90% de confianca.

modelo <- glm(Contratou ~
                DI +
                Financiamento +
                Poupanca +
                Salario +
                CC,
              family = binomial(link = 'logit'), data = consorcio)
summary(modelo)

# Retirar a variavel 'CC'
modelo <- glm(Contratou ~
                DI +
                Financiamento +
                Poupanca +
                Salario,
              family = binomial(link = 'logit'), data = consorcio)
summary(modelo)

# Retirar a variavel 'Financiamento'
modelo <- glm(Contratou ~
                DI +
                Poupanca +
                Salario,
              family = binomial(link = 'logit'), data = consorcio)
summary(modelo)

# (d) Obtenha a probabilidade estimada.

consorcio$probabilidade = predict(modelo, consorcio, type = "response")

# Criar a resposta final usando o ponto de corte
consorcio$predito <- as.factor(ifelse(consorcio$probabilidade > 0.5, 1, 0))  # exemplo: considerando 0,5 como ponto de corte
View(consorcio)

# (e) Obtenha a tabela de classificacao

table(consorcio$Contratou, consorcio$predito)
round(prop.table(table(consorcio$Contratou, consorcio$predito)), 3) # para acuracia
round(prop.table(table(consorcio$Contratou, consorcio$predito), 1), 3) # para sensibilidade e especificidade

