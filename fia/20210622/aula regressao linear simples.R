# inicialização -----------------------------------------------------------
wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())

# Importar Base Dados -----------------------------------------------------
library(readxl) #Avisar o R que utilizará este pacote. Deve-se avisar toda vez que abrir o R
imobiliario <- read_excel("doc/Regressão linear simples.xlsx",sheet="Imobiliario" ) #Função que lê o arquivo xls e salva em uma base de dados R 
#Verificar variáveis
names(imobiliario)

head(imobiliario)

#Análise Exploratória
summary(imobiliario)

# a) Faça o gráfico de dispersão entre as variáveis Distancia_metro_Km e Mil_reais_m2. Existe relação entre preço do imóvel e distância para o metrô? É positiva ou negativa?
plot(imobiliario$Distancia_metro_Km, imobiliario$Mil_reais_m2)
# plot(imobiliario$ Distancia_metro_Km, imobiliario$Mil_reais_m2, ylab="Preço (Mil R$/m2)", xlab="Distância (km)", col='darkturquoise', xlim = c(0,3), ylim = c(0,20))
abline(lm(data=imobiliario, Mil_reais_m2 ~ Distancia_metro_Km), col='blue')

# b) Calcule o coeficiente de correlação linear entre as duas variáveis e interprete o coeficiente. 
cor(imobiliario$ Distancia_metro_Km, imobiliario$Mil_reais_m2)

# c) Por meio do modelo de regressão linear simples, teste se existe relação linear entre as variáveis considerando 90% de confiança. 
regressao <- lm(data=imobiliario, Mil_reais_m2 ~ 0+ Distancia_metro_Km)
summary(regressao)

# d) Interprete os parâmetros do modelo e o coeficiente de determinação.

# e) Apresente a equação do modelo estimada.
x <- 1
y <- 18.8154 - 7.2166*x
print(y)

# f) Estime o valor do m2 do imóvel caso o cliente desejasse morar a 1 km do metrô.
## Em média 11.60

# g) Para este cliente que deseja morar a 1km do metrô, estimo o valor a ser pago em um apartamento de 70 
