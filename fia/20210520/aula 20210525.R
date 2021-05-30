
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

library("dplyr")
library("glue")
library(readxl)


dados <- read_excel("dados/People_Analytics_v2.xlsx")

dados

dados[dados$Idade > 50,]


