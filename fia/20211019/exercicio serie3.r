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

#install.packages("tseries")
library(tseries)
serie <- read.ts("dados/serie3.txt", header = FALSE, sep = "", skip = 0)

#Análise Exploratória da Série
summary(serie)

# Gráfico da serie
par(mfrow=c(1,1))
ts.plot(serie)

# Teste de Estacionariedade
adf.test(serie)

#Identificação
#Gráfico de Autocorrelação e Autocorrelação Parcial da série
par(mfrow=c(1,2))
acf(serie, main="ACF")
pacf(serie,main="PACF")

#Modelo AR(1)
modelo <- arima(serie, order = c(1,0,0), fixed = c(NA,NA), method = c("ML"))

#Teste de hipótese dos parâmetros
library(lmtest) 
coeftest(modelo) #apresenta o modelo e os p-valores

#Retira o intercepto
modelo <- arima(serie, order = c(1,0,0), fixed = c(NA,0), method = c("ML"))
coeftest(modelo) #apresenta o modelo e os p-valores

#Análise de Resíduos
par(mfrow=c(1,2))
acf(residuals(modelo), main="ACF dos Resíduos")
pacf(residuals(modelo), main="PACF dos Resíduos")

# AR(22)
modelo<- arima(serie, order = c(22,0,0), 
               fixed =
                 c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,0), method = c("ML"))
coeftest(modelo)


#Análise de Resíduos
par(mfrow=c(1,2))
acf(residuals(modelo), main="ACF dos Resíduos")
pacf(residuals(modelo), main="PACF dos Resíduos")

# ma(12)
modelo <- arima(serie, order = c(22,0,12), 
                fixed = c(NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,NA,0), method = c("ML"))
coeftest(modelo) #apresenta o modelo e os p-valores


#Análise de Resíduos
par(mfrow=c(1,2))
acf(residuals(modelo), main="ACF dos Resíduos")
pacf(residuals(modelo), main="PACF dos Resíduos")

#Modelo ARMA(1,22)
modelo<- arima(serie, order = c(1,0,22), fixed =                 c(NA,0,0,0,0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,0,NA,0), method = c("ML"))
