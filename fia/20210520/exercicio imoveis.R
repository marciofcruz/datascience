
# inicialização -----------------------------------------------------------
wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())


# Funções Personalizadas --------------------------------------------------
minha_distribuicao <- function(nome_variavel, inicio_caption_variavel, caption_variavel) {
  
  labels <- c()
  for (i in 1:dim(table(nome_variavel))) {
    auxiliar <- substring(rownames(table(nome_variavel))[i], inicio_caption_variavel, stri_length(rownames(table(nome_variavel))[i]))
    auxiliar2 <- paste(table(nome_variavel)[i])
    auxiliar3 <- round(prop.table(table(nome_variavel))[i]*100, 4)

    labels <- append(labels, paste(auxiliar, ': ', auxiliar2,' - ', auxiliar3, '%'))
  }
  
  pie(table(nome_variavel),
      clockwise = T,
      main =  paste("Gráfico de Pizza para Frequência de", caption_variavel),
      labels = labels)
  
}


meuhist <- function(valores, caption_variavel) {
  hist(valores,
       main = paste("Histograma", caption_variavel),
       ylab = "Frequência",
       xlab = caption_variavel,
       col = "pink",
       include.lowest = T,
       right = T)
  
}


# Instação e carregamento de packages -------------------------------------
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("moments")
library(moments)
library(dplyr)
library(readxl)
library("stringi")

dados <- readxl::read_excel("dados/Exercícios.xlsx", sheet = "Base de Dados 3")

dados.qtde <- dim(dados)[1]

# a) Faça a distribuição de frequências da variável idade. -------------------
minha_distribuicao(dados$Idade_imovel, 4, "Idade Imóvel")

# b) Faça a distribuição de frequências da variável região ----------------
table(dados$Região)
minha_distribuicao(dados$Região, 1, "Região")


# c) Tendência central distancias ao metrô --------------------------------
summary(dados$Distancia_metro_Km)
dmetro.min <- min(dados$Distancia_metro_Km)
dmetro.max <- max(dados$Distancia_metro_Km)
dmetro.mediana <- median(dados$Distancia_metro_Km)
dmetro.q1 <- quantile(dados$Distancia_metro_Km, 0.25)
dmetro.q3 <- quantile(dados$Distancia_metro_Km, 0.75)
boxplot(dados$Distancia_metro_Km)
dmetro.coeficiente_assimetria <- skewness(dados$Distancia_metro_Km)
dmetro.iiq <- dmetro.q3 - dmetro.q1
dmetro.li <- dmetro.q1 - 1.5 * dmetro.iiq
dmetro.ls <- dmetro.q3 + 1.5 * dmetro.iiq
dmetro.qtde_discrepantes_a_menor <- length(dados$Distancia_metro_Km[dados$Distancia_metro_Km < dmetro.li])
dmetro.qtde_discrepantes_a_maior <- length(dados$Distancia_metro_Km[dados$Distancia_metro_Km > dmetro.ls])
sprintf("Resp Mín: %.2f, Max: %.2f, Mediana: %.2f, Q1: %.4f, Q3: %.4f",
        dmetro.min, dmetro.max, dmetro.mediana, dmetro.q1, dmetro.q3)
hist(dados$Distancia_metro_Km)

sprintf("Existem %d imóveis na base, o coeficiente é baixo, porem assimétrico a direita.
A maioria dos imóveis está a aproximadamente %.2f km do metrô.
Existem apenas %d imóveis com valores discrepantes, no caso, abaixo de %.2f m do metro.",
dados.qtde, dmetro.mediana, dmetro.qtde_discrepantes_a_menor, dmetro.li)

# d) Tendencia central do valor do imóvel ---------------------------------
summary(dados$Mil_reais_m2)
valorimovel.min <- min(dados$Mil_reais_m2)
valorimovel.max <- max(dados$Mil_reais_m2)
valorimovel.mediana <- median(dados$Mil_reais_m2)
valorimovel.mean <- mean(dados$Mil_reais_m2)

skewness(dados$Mil_reais_m2, na.rm = T)

valorimovel.q1 <- quantile(dados$Mil_reais_m2, 0.25)
valorimovel.q3 <- quantile(dados$Mil_reais_m2, 0.75)
valorimovel.iiq <- valorimovel.q3 - valorimovel.q1
valorimovel.coeficiente_assimetria <- skewness(dados$Mil_reais_m2, na.rm = T)
valorimovel.li <- valorimovel.q1 - 1.5 * valorimovel.iiq
valorimovel.ls <- valorimovel.q3 + 1.5 * valorimovel.iiq
valorimovel.qtde_discrepantes_a_menor <- length(dados$Mil_reais_m2[dados$Mil_reais_m2 < valorimovel.li])
valorimovel.qtde_discrepantes_a_maior <- length(dados$Mil_reais_m2[dados$Mil_reais_m2 > valorimovel.ls])
boxplot(dados$Mil_reais_m2)

sprintf("Os valores dos m2 de imóveis variam entre %.2f e %.2f, sendo a mediana do valor é de %.2f.
Quanto a valores discrepantes, há o total de %d abaixo de LI e %d acima de LS.",
valorimovel.min, valorimovel.max, valorimovel.mediana, valorimovel.qtde_discrepantes_a_menor, valorimovel.qtde_discrepantes_a_maior)
