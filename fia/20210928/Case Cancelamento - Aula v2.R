library(readxl)
library(dplyr)

setwd('C:\\Users\\Caio Felipe\\Documents\\LabData FIA\\Aulas Setembro\\20210928 - Estudo de Caso Pt 2')
dados <- read_excel("Cancelamento de seguros v2.xlsx", sheet="BASE DE DADOS")

#Vamos começar fazendo uma análise exploratória das variáveis

str(dados)

#Variável ID
#Uma ideia é olharmos se não temos duplicatas
length(unique(dados$id))
unique(dados$id) %>% length()


#Removendo a coluna ID dado que não precisaremos dela
dados <- dados[,-1]
#dados$id <- NULL
#dados <- dados[ ,!names(dados) %in% c("id")]


 # - Análise exploratória univariada

#Variável Produto
  is.na(dados$produto) %>% sum()
  table(dados$produto)
  table(dados$produto) %>% prop.table()
  prop.table(table(dados$produto))

#Variável Pagamento
  is.na(dados$Pagamento) %>% sum()
  table(dados$Pagamento)
  table(dados$Pagamento) %>% prop.table()
  #prop.table(table(dados$Pagamento))
  
#Variável Produto
  is.na(dados$uf) %>% sum()
  dados[is.na(dados$uf),]
  table(dados$uf)
  table(dados$uf) %>% prop.table()
  #prop.table(table(dados$produto))

#Uma possibilidade é não utilizar as UFs que têm apenas 1 obseração
#Outra possibilidade é agrupar por Região
  
  #Vamos analisar o IV das variáveis (UF (Filtrada) / Região)
  
dados$regiao <-ifelse(dados$uf %in% c("SP", "RJ", "ES", "MG"), "SUDESTE",
                                      ifelse(dados$uf %in% c("RS", "SC", "PR"), "SUL", 
                                             ifelse(dados$uf %in% c("DF", "MT", "MS", "GO"), "CENTRO-OESTE", 
                                                    ifelse(dados$uf %in% c("AL", "PI", "CE", "PE", "PB", "RN", "SE", "BA", "MA"), "NORDESTE",
                                                           ifelse(dados$uf %in% c("TO", "AM", "PA", "RR", "RO", "AC", "AP"), "NORTE", "SEM INFO")))))

table(dados$regiao)
dados[dados$regiao=='SEM INFO',]

dados <- dados[!is.na(dados$uf),]
table(dados$regiao)
table(dados$uf)

#Variável Estado Civil
is.na(dados$est_civ) %>% sum()
table(dados$est_civ)
table(dados$Pagamento) %>% prop.table()

#NaiveBayes

#Variável Sexo
is.na(dados$sexo) %>% sum()
table(dados$sexo)
table(dados$sexo) %>% prop.table()

#Variável Renda
is.na(dados$renda) %>% sum()
table(dados$renda)
table(dados$renda) %>% prop.table()

dados <- dados[!dados$renda==0,]
dados <- dados[dados$renda!=0,]

#Variável Data de Nascimento
is.na(dados$dt_nasc) %>% sum()

#Calcular uma variável idade

# Fazer a data de hoje - Data de nascimento...
# Utilizar a data do último cancelamento...
# Fazer a data de extração da base de dados - Data de nascimento

library(lubridate)
str(dados)
dados$dt_nasc <- as.Date(dados$dt_nasc)

#Como pegar a data de hoje?
now()
dados$idade <- floor((as.Date(Sys.time())-dados$dt_nasc)/365.25) %>% as.numeric()

summary(dados$idade)
dados <- dados[dados$idade>0,]
summary(dados$idade)
boxplot(dados$idade)


dados$cancelamento <- as.Date(dados$cancelamento)
dados[dados$cancelamento > now() & !is.na(dados$cancelamento),]
dados$cancelou <- ifelse(is.na(dados$cancelamento),0,1)




 # - Análises exploratórias bivariadas

  attach(dados)

table(cancelou)
table(cancelou) %>% prop.table()

prop.table(table(produto,sexo),2)
prop.table(table(produto,est_civ),2)

library(descr)

CrossTable(est_civ,cancelou,prop.t = FALSE, prop.c = FALSE, prop.chisq = FALSE)
CrossTable(sexo,cancelou,prop.t = FALSE, prop.c = FALSE, prop.chisq = FALSE)
CrossTable(regiao,cancelou,prop.t = FALSE, prop.c = FALSE, prop.chisq = FALSE)
CrossTable(uf,cancelou,prop.t = FALSE, prop.c = FALSE, prop.chisq = FALSE)
CrossTable(produto,cancelou,prop.t = FALSE, prop.c = FALSE, prop.chisq = FALSE)
CrossTable(Pagamento,cancelou,prop.t = FALSE, prop.c = FALSE, prop.chisq = FALSE)
CrossTable(renda,cancelou,prop.t = FALSE, prop.c = FALSE, prop.chisq = FALSE)

options(scipen = 999)
chisq.test(Pagamento,cancelou)
chisq.test(produto,cancelou)
chisq.test(regiao,cancelou)



library(arules)
dados$idade_cat <- discretize(dados$idade,breaks=3)
CrossTable(dados$idade_cat,cancelou,prop.t = FALSE, prop.c = FALSE, prop.chisq = FALSE)

  detach(dados)

summary(dados$produto)

set.seed(0510)
amostra <- sample(1:nrow(dados),0.7*nrow(dados))
dados$id <- row.names(dados)
treino <- dados[dados$id %in% amostra,]
teste <- dados[!dados$id %in% amostra,]

dados2 <- dados

modelo <- glm(cancelou ~ sexo+
                idade_cat+
                Pagamento+
                produto+
                regiao+
                est_civ+
                renda), data=dados2, family=binomial(link='logit'))
summary(modelo)
names(dados)

dados2 <- dados

dados2[dados2$Pagamento %in% c('anual','unico'),]$Pagamento <- 'anual ou unico'
table(dados$Pagamento)
dados2[dados2$produto %in% c('seguro 1','seguro 2'),]$produto <- 'seguro 1 ou 2'
table(dados2$produto)
dados2[dados2$regiao != 'SUL',]$regiao <- 'OUTRAS'
table(dados2$regiao)
dados2[dados2$est_civ != 'Inválido',]$est_civ <- 'VÁLIDO'
table(dados2$est_civ)



library(rpart)
library(rpart.plot)


modelo2 <- rpart(cancelou ~ sexo+
                idade_cat+
                Pagamento+
                produto+
                regiao+
                est_civ+
                renda, data=treino)
rpart.plot(modelo2,4,cex=0.8)

library(CHAID)
controle <- rpart.control(minsplit = 10,cp=0.007)
modelo3 <- chaid(as.factor(cancelou) ~ as.factor(sexo)+
                   as.factor(idade_cat)+
                   as.factor(Pagamento)+
                   as.factor(produto)+
                   as.factor(regiao)+
                   as.factor(est_civ)+
                   as.factor(renda), data=treino)
plot(modelo3)

rpart.plot(modelo3,2,cex=0.8)
?rpart


library(Information)
library(InformationValue)
Inf_Value <- create_infotables(data = dados[,-c(3,8,10,13)], y = 'cancelou')
print(head(Inf_Value$Summary, 10), row.names = FALSE)

modelo4 <- glm(cancelou ~ #sexo+
                idade_cat+
                Pagamento+
                #produto+
                regiao+
                est_civ+
                as.factor(renda), data=dados2, family=binomial(link='logit'))
summary(modelo4)

modelo5<- glm(cancelou ~ #sexo+
                 idade_cat+
                 Pagamento+
                 #produto+
                 regiao+
                 #est_civ+
                 as.factor(renda), data=dados2, family=binomial(link='logit'))
summary(modelo5)

#Modelo 5 é sem est_civ pensando no problema da variável

set.seed(0510)
amostra <- sample(1:nrow(dados2),0.7*nrow(dados2))
#dados2$id <- row.names(dados2)
treino <- dados2[dados2$id %in% amostra,]
teste <- dados2[!dados2$id %in% amostra,]

modelo5<- glm(cancelou ~ #sexo+
                idade_cat+
                Pagamento+
                #produto+
                regiao+
                #est_civ+
                renda, data=treino, family=binomial(link='logit'))
summary(modelo5)

modelo6 <- rpart(cancelou ~ sexo+
                   idade_cat+
                   Pagamento+
                   produto+
                   regiao+
                   est_civ+
                   renda, data=treino, control=controle)
rpart.plot(modelo6,4,cex=0.45)

?rpart

library(CHAID)
controle <- rpart.control(minsplit = 10,cp=0.007)
modelo7 <- chaid(as.factor(cancelou) ~ as.factor(sexo)+
                   as.factor(idade_cat)+
                   as.factor(Pagamento)+
                   as.factor(produto)+
                   as.factor(regiao)+
                   as.factor(est_civ)+
                   as.factor(renda), data=treino)
plot(modelo7)


teste$predito1 <- predict(modelo5,teste,type='response')
teste$predito2 <- predict(modelo6,teste)
teste$predito3 <- predict(modelo7,teste,type='prob')[,2]

teste$class1 <- ifelse(teste$predito1>=0.5,1,0)
teste$class2 <- ifelse(teste$predito2>=0.5,1,0)
teste$class3 <- ifelse(teste$predito3>=0.5,1,0) %>% as.factor()

table(dados$cancelou) %>% prop.table()

teste$class4 <- ifelse(teste$predito1>=0.326,1,0)
teste$class5 <- ifelse(teste$predito2>=0.326,1,0)
teste$class6 <- ifelse(teste$predito3>=0.326,1,0)

library(caret)

confusionMatrix(as.factor(teste$cancelou),as.factor(teste$class1),positive='1')
confusionMatrix(as.factor(teste$cancelou),as.factor(teste$class2),positive='1')
confusionMatrix(as.factor(teste$cancelou),as.factor(teste$class3),positive='1')

confusionMatrix(as.factor(teste$cancelou),as.factor(teste$class4),positive='1')
confusionMatrix(as.factor(teste$cancelou),as.factor(teste$class5),positive='1')
confusionMatrix(as.factor(teste$cancelou),as.factor(teste$class6),positive='1')


class(teste$class3)

?chaid

summary(modelo7)




