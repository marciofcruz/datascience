
# inicialização -----------------------------------------------------------
wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())


# Instalação de packages --------------------------------------------------
# install.packages("readxl")
# install.packages("dplyr")

library(dplyr)
library(readxl)
dados <- readxl::read_excel("dados/Exercícios.xlsx", sheet = "Base de Dados 2")

# a) Quantos clientes a base de dados possui? Quantos são mulheres? E --------
clientes.qtde <- length(unique(dados$ID))
clientes.feminino <-  table(dados$Sexo)["Feminino"]
clientes.prop_feminino <- prop.table(table(dados$Sexo))["Feminino"]
sprintf("a) Resp: A base de dados possui %d clientes. %d são mulheres. De forma relativa, há %.4f mulheres.", 
        clientes.qtde, clientes.feminino, clientes.prop_feminino)


# b) Dados de tempo de relacionamento
relacionamento.media <- mean(dados$`Tempo_relacionamento (anos)`)
relacionamento.mediana <- median(dados$`Tempo_relacionamento (anos)`)
relacionamento.min <- min(dados$`Tempo_relacionamento (anos)`)
relacionamento.max <- max(dados$`Tempo_relacionamento (anos)`)
relacionamento.q1 <- quantile(dados$`Tempo_relacionamento (anos)`, 0.25)
relacionamento.q3 <- quantile(dados$`Tempo_relacionamento (anos)`, 0.75)
sprintf("Resp: Média: %.4f, Mediana: %.4f, Mínimo: %d, Máximo: %d, Q1: %.4f, Q4: %.4f",
        relacionamento.media, relacionamento.mediana, relacionamento.min, relacionamento.max, relacionamento.q1, relacionamento.q3)

# c) Com base na distribuição de frequências do tempo de relacionamento, qual a proporção de clientes que ainda não completaram 1 ano de relacionamento? 
relacionamento.prop0anos <- length(dados$`Tempo_relacionamento (anos)`[dados$`Tempo_relacionamento (anos)`==0]) / 
                            clientes.qtde
sprintf("Resp: Proporção de clientes com menos de 1 ano é : %.4f, portanto %.2f por cento da base.",
        relacionamento.prop0anos, relacionamento.prop0anos*100)

# d) Qual a proporção de clientes que possuem 10 anos de relacionamento?
relacionamento.prop10anos <- length(dados$`Tempo_relacionamento (anos)`[dados$`Tempo_relacionamento (anos)`==10]) / 
  clientes.qtde
sprintf("Resp: Proporção de clientes com 10 anos é : %.4f, portanto %.2f por cento da base.",
        relacionamento.prop10anos, relacionamento.prop10anos*100)

# e) Qual o % de clientes que têm 1 produto? E 2 produtos? Utilize a variável Num_de_Produtos.
produto.propqtde1 <- length(dados$Num_de_Produtos[dados$Num_de_Produtos == 1]) / clientes.qtde
produto.propqtde2 <- length(dados$Num_de_Produtos[dados$Num_de_Produtos == 2]) / clientes.qtde
sprintf("Resp: Proporção de clientes que tem 1 produto é: %.4f ", produto.propqtde1)
sprintf("Resp: Proporção de clientes que tem 2 produtos é: %.4f ", produto.propqtde2)

# f) Qual o total de clientes que já cancelaram os produtos? E q --------
clientes.qtde_ja_cancelou <- dim(dados[dados$Cancelou == 1,])[1]
clientes.qtde_nao_cancelou <- dim(dados[dados$Cancelou == 0,])[1]
clientes.prop_ja_cancelou <- clientes.qtde_ja_cancelou / clientes.qtde
clientes.prop_nao_cancelou <- clientes.qtde_nao_cancelou / clientes.qtde

sprintf("Resp: O total de clientes que já cancelaram é: %d", clientes.qtde_ja_cancelou)
sprintf("Resp: O total de clientes que não  cancelaram é: %d", clientes.qtde_nao_cancelou)
sprintf("Resp: Frequencia Relativa dos que já  cancelaram: %.4f", clientes.prop_ja_cancelou)
sprintf("Resp: Frequencia Relativa dos que não cancelaram: %.4f", clientes.prop_nao_cancelou)

