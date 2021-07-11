
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
library(HH)

library(readxl) #Avisar o R que utilizará este pacote. Deve-se avisar toda vez que abrir o R
library(Information) 

salario <- read_excel("base2/Fatores_Impacto_Salario.xlsx",sheet="BASE DE DADOS" ) #Função que lê o arquivo 
names(salario) #Apresenta as variáveis da base 


round(cor(salario), 2)

modelo_salario <- glm(OVER10 ~ .,
                      family = binomial(link = "logit"),
                      data = salario[,-5]
                       )
summary(modelo_salario)
vif(modelo_salario)
