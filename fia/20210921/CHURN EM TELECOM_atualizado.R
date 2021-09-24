wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())

options(scipen=999)
options(width = 1024)

#Bibliotecas
library(readxl)
library(Information)
library(InformationValue)
library(HH)
library(dgof)
library(descr)
library(arules)

#Importacao das bases de treino e teste
setwd('dados/20210921 - Estudo de Caso')
telco<-read_excel("dados/CHURN EM TELECOM.xlsx", sheet="BASE_DADOS")
#View(telco)

#Criando a variavel resposta como 1 ou 0.
telco$Churn<-ifelse(telco$Churn=="Yes",1,0)
#View(telco)

#Separacao nas bases de treino e teste
#Separar a amostra em treino e teste
set.seed(123) # para travar a amostra
sample.size <- floor(0.70 * nrow(telco))
train.index <- sample(seq_len(nrow(telco)), size = sample.size)
train <- telco[train.index, ]
#View(train)
#dim(train)
test <- telco[- train.index, ]
#View(test)
#dim(test)

#Percentual de churn na base de treino
prop.table(table(train$Churn))

#An?lise bivariada das variaveis


train$Tempo_conta_cl <- discretize(train$Tempo_conta, method = "frequency", breaks = 5)			
train$Fatura_Media_cl <- discretize(as.numeric(train$Fatura_Media), method = "frequency", breaks = 5)	
View(train)

#Mais ? frente voltaremos para falar sobre o problema de utilizar o "discretize".

CrossTable(train$Sexo, train$Churn, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)	
CrossTable(train$Idoso, train$Churn, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)	
CrossTable(train$Dependentes, train$Churn, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(train$Internet, train$Churn, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(train$Streaming_TV, train$Churn, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)	
CrossTable(train$Streaming_Filmes, train$Churn, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)	
CrossTable(train$Contrato, train$Churn, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)	
CrossTable(train$Fatura_digital, train$Churn, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)	
CrossTable(train$Metodo_Pagamento, train$Churn, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)	
CrossTable(train$Tempo_conta_cl, train$Churn, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)	
CrossTable(train$Fatura_Media_cl, train$Churn, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)	

#Identificacao das variaveis mais relevantes
Inf_Value <- create_infotables(data = train, y = "Churn", ncore = 2)
print(head(Inf_Value$Summary, 10), row.names = FALSE)

#Modelo na base de treino (com algumas vars selecionadas)
modelo<-glm(Churn ~., family = binomial (link = 'logit'), data=train[,c(2,3,4,9,13,14,15)])
summary(modelo)

#Identificao de sinais de multicolinearidade
vif(modelo)

#Calculo do score na base de treino
train$score<-predict(modelo, train, type="response")
View(train)

#Indices de acuracia, sensibilidade e especificidade na base de treino
#pc de 0.5
#Avaliando indicadores de acuracia do modelo											
train$Churn_pred<-ifelse(train$score>0.5,1,0)											
CrossTable(train$Churn, train$Churn_pred, prop.t = FALSE, prop.chisq = FALSE)	
View(train)
# Tabela de desempenho: resposta observada x predita											
(tabela_desempenho <- table(train$Churn, train$Churn_pred))											
# Medidas de desempenho											
(sensibilidade <- tabela_desempenho[2, 2] / sum(tabela_desempenho[2, ]))											
(especificidade <- tabela_desempenho[1, 1] / sum(tabela_desempenho[1, ]))											
(n <- nrow(train))											
(acuracia <- sum(tabela_desempenho[1, 1] + tabela_desempenho[2, 2]) / n)	

#KS do modelo na base de treino
ks.test(train$score[train$Churn==0],
        train$score[train$Churn==1])

#KS do modelo na base de teste
test$score<-predict(modelo, test, type="response")
ks.test(test$score[test$Churn==0],
        test$score[test$Churn==1])

#AUC do modelo na base de treino
plotROC(actuals=train$Churn, predictedScores=train$score)

#AUC do modelo na base de teste
plotROC(actuals=test$Churn, predictedScores=test$score)

#Churn por faixa de score	na base de treino										
train$score_faixa<-discretize(train$score, method = "frequency", breaks = 10)											
CrossTable(train$score_faixa, train$Churn,prop.r=TRUE, prop.c=FALSE, prop.t = FALSE, prop.chisq = FALSE)											

