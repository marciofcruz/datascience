#Diretório
setwd("C:\\Users\\Caio Felipe\\Documents\\LabData FIA\\Aulas Setembro\\20210928 - Estudo de Caso Pt 2")

options(scipen=999) #Retirar a notação científica

#Bibliotecas
library(dplyr)
library(readxl)
library(expss)
library(Information) 
library(arules)
library(smbinning)
library(HH)
library(InformationValue) 
library(scorecard)

#Importar a base de dados
cartao <- read_excel("Inadimplencia_Cartao.xlsx", sheet="BASE DE DADOS")

#############################################
#Análise exploratória univariada
#############################################

#Variável resposta
prop.table(table(cartao$INADIMPLENTE))

#Variáveis quantitativas
summary(cartao)

#Idade negativa e 999
quantile(cartao$IDADE, c(.01, .05)) #percentis - nao há muitos casos
quantile(cartao$IDADE, c(.90, .95)) #percentis - nao há muitos casos
valor <- median(cartao$IDADE) #atribuir, no campo "valor", a mediana da idade
cartao$IDADE[cartao$IDADE<0] <- valor #substitui a idade negativa pela mediana
cartao$IDADE[cartao$IDADE>900] <- valor #substitui a idade maior que 900 pela mediana
summary(cartao$IDADE)
boxplot(cartao$IDADE)

#Variáveis qualitativas
cartao$SEXO <- as.character(cartao$SEXO)
cartao$SEXO[cartao$SEXO == "1"] <- "M"
cartao$SEXO[cartao$SEXO == "2"] <- "F"
table(cartao$SEXO) 

table(cartao$ESCOLARIDADE, useNA = "always")
#Muitos missings, exluir - a variavel pode nao ser de preenchimento obrigatorio

cartao$ESTADOCIVIL <- as.character(cartao$ESTADOCIVIL)
cartao$ESTADOCIVIL[cartao$ESTADOCIVIL == "1"] <- "Casado"
cartao$ESTADOCIVIL[cartao$ESTADOCIVIL == "2"] <- "Solteiro"

#############################################
#Análise exploratória bivariada
#############################################

#Variáveis qualitativas
cro_rpct(cartao$SEXO, cartao$INADIMPLENTE)
cro_rpct(cartao$ESCOLARIDADE, cartao$INADIMPLENTE)
cro_rpct(cartao$ESTADOCIVIL, cartao$INADIMPLENTE)

#Variáveis quantitativas
boxplot(cartao$VALOR_FATURA ~ cartao$INADIMPLENTE)
boxplot(cartao$IDADE ~ cartao$INADIMPLENTE)
boxplot(cartao$VALOR_FATURA_ANTERIOR ~ cartao$INADIMPLENTE)

#Cria o campo percentual de uso, que é valor da fatura/limite solicitado
cartao$perc = cartao$VALOR_FATURA_ANTERIOR/cartao$VALOR_FATURA  
summary(cartao$perc) #Estatísticas descritivas
boxplot(cartao$perc ~ cartao$INADIMPLENTE)

#############################################
#IV
#############################################
IV <- create_infotables(data = cartao[,c(2,3,5,6,7,10,11)], y = "INADIMPLENTE")
IV$Summary

#####################################################
#Categorize as 2 variáveis quantitativas mais fortes, de 2 formas: 
#em quartis e de forma otimizada. 
#Qual apresentou melhores resultados?
######################################################

cartao<-as.data.frame(cartao)

#1. 
#a. quartis
cartao$VALOR_FATURA_ANTERIOR_quartis<-discretize(cartao$VALOR_FATURA_ANTERIOR, method="frequency", breaks=4)
#b. otimizada
VALOR_FATURA_ANTERIOR_otim <- smbinning(df=cartao,y="INADIMPLENTE",x="VALOR_FATURA",p=0.05) 
cartao<-smbinning.gen(cartao,VALOR_FATURA_ANTERIOR_otim, chrname="VALOR_FATURA_ANTERIOR_otim" )
#IV
IV <- create_infotables(data = cartaoc(2,3,5,6,7,10,11), y = "INADIMPLENTE")
IV$Summary

#2. perc
#a. quartis
cartao$perc_quartis<-discretize(cartao$perc, method="frequency", breaks=4)
#b. otimizada
perc_otim <-smbinning(df=cartao,y="INADIMPLENTE",x="perc",p=0.05) 
cartao<-smbinning.gen(cartao,perc_otim, chrname="perc_otim" )
#IV
IV <- create_infotables(data = cartao, y = "INADIMPLENTE")
IV$Summary

#####################################################
#Divida a base em treino e teste e 
#####################################################
set.seed(123)
amostra = sort(sample(nrow(cartao), nrow(cartao)*.7))
treino<-cartao[amostra,]
teste<-cartao[-amostra,]

#####################################################
#obtenha o modelo de regressão logística utilizando o método Backward , 
#com 95% de confiança.
#####################################################

modelo <- glm(INADIMPLENTE ~ VALOR_FATURA_ANTERIOR_otim+perc+
                IDADE+SEXO,
              family=binomial(link='logit'),
              data=treino)
summary(modelo) 

#####################################################
#Multicolinearidade
#####################################################
vif(modelo)

#####################################################
#KS e AUC na base de treino e teste
#####################################################

#Calcular os preditos
treino$probabilidade = predict(modelo,treino, type = "response")
teste$probabilidade = predict(modelo,teste, type = "response")

ks_stat(actuals=treino$INADIMPLENTE, predictedScores=treino$probabilidade)
plotROC(actuals=treino$INADIMPLENTE, predictedScores=treino$probabilidade)

ks_stat(actuals=teste$INADIMPLENTE, predictedScores=teste$probabilidade)
plotROC(actuals=teste$INADIMPLENTE, predictedScores=teste$probabilidade)

#####################################################
#distribuição do score, por decis
#####################################################
treino$probb_faixas <-
  discretize(treino$probabilidade, method="frequency", 
             breaks=10)

cro_rpct(treino$probb_faixas,treino$INADIMPLENTE)

