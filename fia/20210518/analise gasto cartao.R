
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

quantile(dados$Gastos_Cartao,
         probs = c(0.01, 0.05, 0.1, 0.9, 0.95, 0.99))

vars <- names(dados)[1:4]
for(i in vars) {
  
  par(mfrow=c(2,1))
  
  hist(dados[,i], breaks = 20,
       main=paste("Histograma - ", i),
       )
  
  boxplot(dados[,i])
  
}

vars <- names(dados)[1:4]
cor(dados[, vars])

install.packages("car")
library(car)

scatterplotMatrix(Gastos_Cartao + Idade _ Renda  , data = dados, smooth = F,
                  reg.line = F)


 