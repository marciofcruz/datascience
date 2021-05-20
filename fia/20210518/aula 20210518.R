
# inicialização -----------------------------------------------------------


wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variÃ¡vel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
# 

rm(list = ls())

probabilidade <- function(proba) {
  chances <- 0
  
  while (runif(1) > proba) {
    chances <- chances + 1
  }
  
  return(chances)
    
}


# instalação de pacotes ---------------------------------------------------

install.packages("xlsx")


# carregamento de pacotes -------------------------------------------------

library(xlsx)


# funçÕes -----------------------------------------------------------------

func_salario <- function(horas, pnh) {
  
  
  if (horas >= 100) {
    auxiliar <- (100 * pnh) + ((horas-100) * (pnh*0.9))
    
  }
  else
  {
    auxiliar <- horas * pnh
  }
  
  return(auxiliar)
}

minha_raiz <- function(numeros) {
  resp <- numeric(length(numeros))
  
  for(i in seq_along(numeros)) {
    resp[i] <- sqrt(numeros[i])
  }
  
  return (resp)
}


# playground --------------------------------------------------------------


# print(func_salario(103, 10))

x <- 1:1000

system.time(loop <- minha_raiz(x)) 
system.time(vetor <- sqrt(x))

seq(1, 5, by = 0.5)

teste <- rep(1:2, times=3)
teste
table(teste)

x <- 10:50
x[-c(2,5)]

x[x %in% c(1,2,3,4,40)]

matriz <- matrix(1:6, nrow = 3, ncol = 4)
matriz

matriz %*% t(matriz)


lista <- list(x = 1:5, y = c("a", "b", "c"))

lista["y"]

df <- data.frame(x = 1:3, y = c("a", "b", "c"))

x[x >= 20] <- 0
x


# cartao de credito -------------------------------------------------------

library(xlsx)
dados <- read.csv("dados/base_gastos_cartao.csv")
dados

names(dados)

unique(dados$Segmento)

typeof(dados)
# Segmento C
dados[dados$Segmento == "C",]

segmento <- dados[dados$Segmento == "A",] 

summary(segmento)


# graficos ----------------------------------------------------------------


x <- rnorm(100)
y <- x*3 + rnorm(100, 0, 2.8)

par(mfrow = c(1,2))
plot(x, y, pch=16, main = "Gráfico de dispersão de X vx Y",
     xlab = "X",
     ylab = "Y")
hist(x, main = "Histograma de X",
     xlab = "X",
     ylab = "Frequencia")
