
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

cosmeticos <- read_excel("base1/Cosmeticos.xlsx", sheet = "Base de Dados")

# Analise Exploratoria ----------------------------------------------------
#Sexo
table(cosmeticos$Sexo)
prop.table(table(cosmeticos$Sexo))

#Variável Idade
summary(cosmeticos$Idade)
boxplot(cosmeticos$Idade)

# Variavel 'Cidade'
table(cosmeticos$Cidade)
prop.table(table(cosmeticos$Cidade))

# Variavel resposta
table(cosmeticos$Resposta) 
prop.table(table(cosmeticos$Resposta)) 


# Modelo Regressao Logistica ----------------------------------------------

### Modelo de Regressao Logistica

modelo <- glm(Resposta ~
                Sexo +
                Idade +
                Cidade, 
              family = binomial(link = 'logit'), data = cosmeticos)

summary(modelo)

