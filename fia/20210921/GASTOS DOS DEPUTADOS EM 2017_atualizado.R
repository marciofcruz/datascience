wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())

options(scipen=999)
options(width = 1024)


#Instalando e carregando bibliotecas
#install.packages("readxl")
#install.packages("data.table")
#install.packages("summarytools")
#install.packages("naniar")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("descr")
library(readxl)
library(data.table)
library(summarytools)
library(naniar)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(descr)
library(Rcpp)
#Importando a base de dados dos gastos dos deputados em 2017

Cota<-read_excel("dados/GASTOS DOS DEPUTADOS EM 2017.xlsx", sheet="BASE_DADOS")
#dim(Cota)
#View(Cota)

#Formato dos campos
str(Cota)

#Avaliar quantidade de dados faltantes na tabela
sum(is.na(Cota))

#Avaliar analiticamente concentra??o de dados faltantes
sapply(Cota, function(x) sum(is.na(x)))

#Avaliar graficamente concentracao de dados faltantes
options(warn=-1)
vis_miss(Cota, warn_large_data = FALSE)

#Criando a variavel REGIAO na tabela
Cota$Regiao<-ifelse(Cota$UF %in% c("SP", "RJ", "ES", "MG"), "SUDESTE",
                    ifelse(Cota$UF %in% c("RS", "SC", "PR"), "SUL", 
                           ifelse(Cota$UF %in% c("DF", "MT", "MS", "GO"), "CENTRO-OESTE", 
                                  ifelse(Cota$UF %in% c("AL", "PI", "CE", "PE", "PB", "RN", "SE", "BA", "MA"), "NORDESTE",
                                         ifelse(Cota$UF %in% c("TO", "AM", "PA", "RR", "RO", "AC", "AP"), "NORTE", "SEM INFO")))))                  

#View(Cota)

#Checagem da classificacao, vamos cruzar a UF com a REGIAO
library(summarytools)
#as.data.frame(ctable(x = Cota$UF, y = Cota$Regiao))
#View(as.data.frame(ctable(x = Cota$UF, y = Cota$Regiao)))

#Selecao de despesas da REGIAO NORTE com valores superiores a R$1.000
Cota_selecao <- subset(Cota, Regiao=="NORTE" & Valor>1000)
View(Cota_selecao)

#Media inicial do valor dos documentos apresentados (base inteira)
mean(Cota$Valor)

#Metricas descritivas dos documentos apresentados
summary(Cota$Valor)

#Calculo de quantis (quartis, decis, percentis)
quantile(Cota$Valor, c(.25, .50, .75, .90)) 

#Volumes de documento por mes
table(Cota$M?s_Despesa)
round((prop.table(table(Cota$M?s_Despesa))*100),2)

#Analise do padrao de comportamento das despesas (Valor)
ggplot(data=Cota, aes(Valor)) + 
  geom_histogram(breaks=seq(0, 2000, by=50))

#Top 10 deputados que mais gastaram
options(warn=-1)
Cota_agrupada <- Cota %>%
  group_by(Parlamentar) %>%
  summarise(Despesas=sum(Valor)) %>%
  arrange(-Despesas)
#View(Cota_agrupada)
head(Cota_agrupada,10)

#Top 10 deputados que menos gastaram
options(warn=-1)
Cota_agrupada<-Cota %>%
  group_by(Parlamentar) %>%
  summarise(Despesas=sum(Valor)) %>%
  arrange(Despesas) %>%
  filter(Despesas>=0)
head(Cota_agrupada,10)

#Volume de despesas por mes no ano de 2017
options(warn=-1)
freq(Cota$M?s_Despesa, style = "rmarkdown")

#Visao grafica da evolucao do volume de despesas
#Agrupamento da base por periodo (mes)
options(warn=-1)
library(dplyr)
Cota_agrupada<-Cota %>%
  group_by(M?s_Despesa) %>%
  summarise(Valor_Despesas=sum(Valor),
            Qtde_Despesas=n(),
            Trx_Media=mean(Valor),
            Trx_DP=sd(Valor),
            Trx_CV=(Trx_DP/Trx_Media)*100) %>%
  arrange(M?s_Despesa)
Cota_agrupada<-as.data.frame(Cota_agrupada)
#View(Cota_agrupada)

#Transformar a coluna Mes_Despesa em Data e calcular o ticket m?dio
Cota_agrupada$Mes <- as.Date(ISOdate(year = 2017, month = Cota_agrupada$M?s_Despesa , day = 1))
Cota_agrupada$Ticket_Medio <- Cota_agrupada$Valor_Despesas/Cota_agrupada$Qtde_Despesas
class(Cota_agrupada$Mes)
View(Cota_agrupada)

#Visao grafica da Quantidade de despesas por mes
ggplot(Cota_agrupada) +
  geom_line(aes(x = Mes, y = Qtde_Despesas), col = "blue", size=3) + 
  expand_limits(y=c(20000, 40000)) + ggtitle("Quantidade de despesas processadas em 2017") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab ("M?s da despesa") + ylab ("Quantidade de despesas") 

#Visao grafica do Valor de despesas por mes
ggplot(Cota_agrupada) +
  geom_line(aes(x = Mes, y = Valor_Despesas), col = "green", size=3) + 
  expand_limits(y=c(14000000, 21000000)) + ggtitle("Valor de despesas processadas em 2017") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab ("M?s da despesa") + ylab ("Valor de despesas") 


#Avaliacao dos tipos de despesa identificados na base
options(warn=-1)
library(dplyr)
Tipos_Despesa <- Cota %>%
  group_by(Categoria_Despesa) %>%
  summarise(Soma_Despesas=sum(Valor),
            Qtde_Despesas=n(),
            Despesa_Media = mean(Valor),
            Despesa_DP = sd(Valor),
            Despesa_Maxima=max(Valor)) %>%
  arrange(-Soma_Despesas)
Tipos_Despesa <- as.data.frame(Tipos_Despesa)
#View(Tipos_Despesa)
Tipos_Despesa$Despesa_extrema <- Tipos_Despesa$Despesa_Media+
  3*Tipos_Despesa$Despesa_DP
#View(Tipos_Despesa)

#Identificacao de despesa extrema (outlier) na base original
Cota1 <- left_join(Cota, Tipos_Despesa,by = "Categoria_Despesa")
View(Cota1)

#Flag Despesa Extrema
Cota1$flag_DE <- ifelse(Cota1$Valor>Cota1$Despesa_extrema,1,0)
#View(Cota1)
table(Cota1$flag_DE)
prop.table(table((Cota1$flag_DE)))

#Percentual de despesa extrema por deputado
table(Cota1$Parlamentar, Cota1$flag_DE)
Quant_DE <- as.data.frame.matrix(table(Cota1$Parlamentar, Cota1$flag_DE))
#View(Quant_DE)
Quant_DE$perc_DE <- (Quant_DE$'1'/(Quant_DE$'0'+Quant_DE$'1'))*100
#View(Quant_DE)

#Concentracao das despesas
Tipos_Despesa <- Tipos_Despesa[order(Tipos_Despesa$Soma_Despesas, decreasing=TRUE), ]
Tipos_Despesa$acumulado <- cumsum(Tipos_Despesa$Soma_Despesas)
Tipos_Despesa$percacum<-round(cumsum(Tipos_Despesa$Soma_Despesas)/sum(Tipos_Despesa$Soma_Despesas),2)*100
class(Tipos_Despesa$Categoria_Despesa)
Tipos_Despesa$Categoria_Despesa <- factor(Tipos_Despesa$Categoria_Despesa, 
                                          levels=Tipos_Despesa$Categoria_Despesa)
class(Tipos_Despesa$Categoria_Despesa)
View(Tipos_Despesa)


#Diagrama de Pareto usando o ggplot2
library(ggplot2)
ggplot(Tipos_Despesa, aes(x=Tipos_Despesa$Categoria_Despesa)) +
  geom_bar(aes(y=Tipos_Despesa$Soma_Despesas), fill='blue', stat="identity") +
  geom_point(aes(y=Tipos_Despesa$acumulado), color = rgb(0, 1, 0), pch=16, size=1) +
  geom_path(aes(y=Tipos_Despesa$acumulado, group=1), colour="slateblue1", lty=3, size=0.9) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs(title = "Diagrama de Pareto", subtitle = "FIA Labdata", x = 'Categorias de despesa', y ='Despesas')


#Gastos por partido
#Avaliacao dos tipos de despesa identificados na base
options(warn=-1)
library(dplyr)
Despesas_Partido<-Cota %>%
  group_by(Partido) %>%
  summarise(Soma_Despesas=sum(Valor),
            Qtde_Despesas=n(),
            Despesa_Media = mean(Valor)) %>%
  arrange(-Soma_Despesas) %>%
  filter(is.na(Partido)==FALSE)
  Despesas_Partido<-as.data.frame(Despesas_Partido)
View(Despesas_Partido)

#Gastos por Dias da semana
Cota1$Dia_semana<-weekdays(as.Date(Cota1$Data))
View(Cota1)
prop.table(table(Cota1$Dia_semana))

