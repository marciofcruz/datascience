
# inicialização -----------------------------------------------------------
wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())

# Binomial ----------------------------------------------------------------
n <- 1500 # Elementos de estudo
p <- 0.7
k <- 8 # Experimentos para obter sucesso

probabilidade.exata <- dbinom(x = k, size = n, prob = p)
probabilidade.menorouigual <- pbinom(q = k, size = n, prob = p, lower.tail = T) 
probabilidade.maior <- 1 -probabilidade.menorouigual  
probabilidade.maiorouigual <- probabilidade.maior + probabilidade.exata
probabilidade.menor <- probabilidade.menorouigual - probabilidade.exata

media = 30 * probabilidade.exata

sprintf("P(X = %d) é %.4f", k, probabilidade.exata)
sprintf("P(X <= %d) é %.4f", k, probabilidade.menorouigual)
sprintf("P(X > %d) é %.4f", k, probabilidade.maior)
sprintf("P(X >= %d) é %.4f", k, probabilidade.maiorouigual)
sprintf("P(X < %d) é %.4f", k, probabilidade.menor)


