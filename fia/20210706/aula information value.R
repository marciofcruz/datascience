
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

library(readxl) #Avisar o R que utilizará este pacote. Deve-se avisar toda vez que abrir o R
library(Information) 

base <- read_excel("base2/Cancelamento.xlsx",sheet="Banco de Dados" ) #Função que lê o arquivo 
names(base) #Apresenta as variáveis da base 
base[-1]

# IV das variáveis --------------------------------------------------------
IV <- create_infotables(data = base, y = "Cancelou")
IV$Summary


# Modelo regressao logistica ----------------------------------------------
modelo <- glm(Cancelou ~
                Score_serasa
              + Sexo
              + Idade
              + Tempo_relacionamento
              + Possui_internet
              + Salario_anual,
              family=binomial(link='logit'),data=base)
summary(modelo)


# Retirar a variável Possui_internet --------------------------------------
modelo <- glm(Cancelou ~
                Score_serasa
              + Sexo
              + Idade
              + Tempo_relacionamento
              #+ Possui_internet
              + Salario_anual,
              family=binomial(link='logit'),data=base)
summary(modelo)


# Retirar a variável Tempo_relacionamento ---------------------------------
modelo <- glm(Cancelou ~
                Score_serasa
              + Sexo
              + Idade
              #+ Tempo_relacionamento
              #+ Possui_internet
              + Salario_anual,
              family=binomial(link='logit'),data=base)
summary(modelo)


# Retirar a variável Salario_anual ----------------------------------------
modelo <- glm(Cancelou ~
                Score_serasa
              + Sexo
              + Idade
              #+ Tempo_relacionamento
              #+ Possui_internet
              #+ Salario_anual
              ,   family=binomial(link='logit'),data=base)
summary(modelo)


# Probabilidade estimada --------------------------------------------------
base$probabilidade = predict(modelo,base, type = "response")


# Criar a resposta final usando o ponto de corte 0,2 ----------------------
base$predito <- as.factor(ifelse(base$probabilidade>0.2, 1, 0))


# Tabela de classificação -------------------------------------------------
library(expss)
cro(base$Cancelou,base$predito)

prop.table(table(base$Cancelou, base$predito))


# Multicolinearidade e estatística VIF ------------------------------------
library(HH)
vif(modelo)

#Ponto de corte
#Simulação com diversos pontos de corte
base$predito <- as.factor(ifelse(base$probabilidade>0.1, 1, 0))
cro(base$Cancelou,base$predito)

base$predito <- as.factor(ifelse(base$probabilidade>0.15, 1, 0))
cro(base$Cancelou,base$predito)

base$predito <- as.factor(ifelse(base$probabilidade>0.2, 1, 0))
cro(base$Cancelou,base$predito)

base$predito <- as.factor(ifelse(base$probabilidade>0.25, 1, 0))
cro(base$Cancelou,base$predito)

base$predito <- as.factor(ifelse(base$probabilidade>0.3, 1, 0))
cro(base$Cancelou,base$predito)

#Maximizar acurácia
library(cutpointr)
ponto <- cutpointr(base, probabilidade, Cancelou,
                   method = maximize_metric, metric = accuracy)
summary(ponto) 

#Minimizar a diferença entre especificidade e sensibilidade
ponto <- cutpointr(base, probabilidade, Cancelou,
                   method = minimize_metric, metric = abs_d_sens_spec)
summary(ponto) 

#KS e ROC
ks_stat(actuals=base$Cancelou, predictedScores=base$probabilidade)
plotROC(actuals=base$Cancelou, predictedScores=base$probabilidade)

