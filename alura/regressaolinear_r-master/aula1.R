setwd("D:/projetos/github.com/datascience/alura/regressaolinear_r-master")
getwd()

install.packages("car")
install.packages("lmtest")

library(readxl)
library("car")
library(lmtest)

dados  <- read_excel("dados.xlsx")

plot(dados$area, dados$preco, 
     main="Diagrama de Dispersao",
     xlab="Área",
     ylab="Preço das casas",
     pch=19)

cor(dados$area, dados$preco)

cor.test(dados$area, dados$preco)

plot(dados$tempo, dados$preco)

cor(dados$tempo, dados$preco)

cor.test(dados$tempo, dados$preco)


boxplot(dados$preco)
summary(dados$preco)

quantile(dados$preco, 0.75)

which(dados$preco > quantile(dados$preco, 0.75))

mod1 <- lm(preco ~area, data = dados)
summary(mod1)
mod1

mod1$coefficients[[1]] + mod1$coefficients[[2]]*70


plot(dados$area, dados$preco, 
     main = "Diagrama e Reta")
abline(mod1, col="red")

names(summary(mod1))

plot(mod1$residuals)

identify(mod1$residuals, n=2)

dados_59 <- dados[-59,]
dados_59_82 <- dados[c(-59,-82),]

dwtest(mod1)

plot(mod1$fitted.values, mod1$residuals)

bptest(mod1)

plot(mod1, 2)

shapiro.test(mod1$residuals)

dados_novos  = data.frame(area = c(60,70))
predict(mod1, newdata = dados_novos)

predict(mod1, new_data = dados_novos, interval="confidence")
