
# inicialização -----------------------------------------------------------


wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variÃ¡vel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
# 

rm(list = ls())

library(xlsx)
# Gastos do cartao em reais
# Idade
# Renda
# Pagamento de Impostos
# Segmento


# 1 Objetivo a distribuição de cada variável
# 2 Verificar a relação entre as variáveis


dados <- read.csv("dados/base_gastos_cartao.csv")

table(dados$Segmento)
