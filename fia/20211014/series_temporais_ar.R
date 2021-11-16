wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())

options(scipen=999)
options(width = 1024)

library(utils)
library(tseries)

options(scipen=999) #Retira notação científica

#install.packages("tseries")
library(tseries)
serie <- read.table("dados/serie14.txt", header = FALSE, sep = "", skip = 0)


#Análise Exploratória da Série
summary(serie$V1)

# Gráfico da serie
ts.plot(serie)

# Teste de Estacionariedade
adf.test(serie$V1)


#Gráfico de Autocorrelação e Autocorrelação Parcial da série
par(mfrow=c(1,2))
print(acf(serie$V1))
print(pacf(serie$V1))

#Modelo MA(1)
modelo <- arima(serie, order = c(0,0,1), fixed = c(NA,NA), method = c("ML"))


#Teste de hipótese dos parâmetros
library(lmtest)
coeftest(modelo)

#Retira o intercepto
modelo <- arima(serie, order = c(0,0,1), fixed = c(NA,0), method = c("ML"))
coeftest(modelo) #apresenta o modelo e os p-valores


#Gráfico de Autocorrelação e Autocorrelação Parcial dos resíduos
acf(residuals(modelo))
pacf(residuals(modelo))

