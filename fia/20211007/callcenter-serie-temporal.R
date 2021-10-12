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


# Leitura de arquivo ------------------------------------------------------
chamados_tlmk <- read.table("dados/Operacao_telemarketing.txt", header = FALSE)

summary(chamados_tlmk$V1)

#Gráfico temporal
ts.plot(chamados_tlmk$V1)

#Teste de Estacionariedade
adf.test(chamados_tlmk$V1)

#Gráfico de Autocorrelação e Autocorrelação Parcial da série
par(mfrow=c(1,2))
print(acf(chamados_tlmk$V1))
print(pacf(chamados_tlmk$V1))

#Ajuste do modelo: 'order = c(X,0,0)' em que X é a ordem do AR e cada NA indica os parâmetros a serem estimados 'fixed = c(NA,NA,NA)'
(modelo <- arima(chamados_tlmk$V1, order = c(2,0,0), fixed = c(NA,NA,NA)))

#Teste de hipótese dos parâmetros
library(lmtest)
coeftest(modelo)

#Gráfico de Autocorrelação e Autocorrelação Parcial dos resíduos
par(mfrow=c(1,2))
acf(residuals(modelo))
pacf(residuals(modelo))

#Projeção N passos para frente
library(forecast)
(projecao <- forecast(modelo, h=5))

alpha_0 <- 877.396897*(1-0.381796+0.103981)
alpha_0

(previsao_467 <-alpha_0 + 0.3817957*627-0.1039812*711)
(previsao_468 <-alpha_0 + 0.381796*799.1-0.103981*627)

