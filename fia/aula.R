setwd("D:/projetos/github.com/datascience/fia/20210518/")
getwd()

# rm(list=ls())

proba <- 0.2
chances <- 0
while(runif(1) > proba) {
  chances <- chances + 1
}
chances


# horas trabalhadas -------------------------------------------------------

getSalario <- function(valorHora, horas) {
  return(valorHora*horas)
}

getSalario(100,10)

for (i in 1:10) {
  j <- i + 10
  print(j)
  
  rm(j)
}


x = 1:1000000


minha_raiz <- function(numeros) {
  resp <- numeric(length(numeros))
  for(i in seq_along(numeros)) {
    resp[i] <- sqrt(numeros[i])
  }
  return(resp)
}

system.time(loop <- minha_raiz(x))

system.time(sqrt(x))
