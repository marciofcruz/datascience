
# inicialização -----------------------------------------------------------
wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())

# install.packages("GGally")

library(ggplot2)
library(GGally)


# Importar Base Dados -----------------------------------------------------
library(readxl) #Avisar o R que utilizará este pacote. Deve-se avisar toda vez que abrir o R
dados_lim_cred  <- read_excel("doc/Regressão linear múltipla.xlsx",sheet="Limite_Credito (1)" ) #Função que lê o arquivo xls e salva em uma base de dados R 

dados_lim_cred
#Verificar variáveis
names(dados_lim_cred)

# Matriz de Gráfico de Dispersão
#Matriz de Scatter Plot
ggpairs(dados_lim_cred, title="correlogram with ggpairs()") 

#Regressão Linear Múltipla
#Modelo de Regressão Linear Múltipla
regressao <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ Idade+Salario+LimitedeCreditoImediato)
summary(regressao)

#Modelo anterior sem Idade
regressao_1 <- lm(data=dados_lim_cred, LimitedoChequeEspecial ~ Salario+LimitedeCreditoImediato)
summary(regressao_1)

