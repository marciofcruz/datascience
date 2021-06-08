
# inicialização -----------------------------------------------------------
wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())

# Construindo uma tabela padronizada
Z <- seq(0, 3.99, by=0.01)
probabilidade <- pnorm(Z)
tabela_normal_padronizada <- matrix(probabilidade, ncol=10, byrow = T)
colnames(tabela_normal_padronizada) <- format(seq(0.00, 0.09, by=0.01))
rownames(tabela_normal_padronizada) <- format(seq(0.00, 3.90, by=0.10), digits =2, nsmall = 2)
tabela_normal_padronizada

# Calculo para 1 variável 
media_populacional <- 20
desvio_padrao_populacional <- 5
x <- 18

z <- (x- media_populacional) / desvio_padrao_populacional

probabilidade <- pnorm(z)

round((1-probabilidade)*60)

sprintf("P(X < %.2f) = %.4f", x, probabilidade)
sprintf("P(X > %.2f) = %.4f", x, 1-probabilidade)


# Calculo para intervalo
media_populacional <- 20
desvio_padrao_populacional <- 5
x_inicial <- 15
x_final <- 28

z_inferior <- (x_inicial - media_populacional) / desvio_padrao_populacional
z_superior <- (x_final - media_populacional) / desvio_padrao_populacional

probabilidade <- pnorm(z_superior) - pnorm(z_inferior)
sprintf("P(%.2f < X < %.2f) = %.4f", x_inicial, x_final, probabilidade)
