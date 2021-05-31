
# inicialização -----------------------------------------------------------
wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())


# Funções Personalizadas --------------------------------------------------

meuboxplot <- function(valores, caption_variavel) {
  boxplot(valores,
          main = paste("Boxplot ", caption_variavel),
          xlab = caption_variavel,
          col = "pink",
          horizontal = T,
          varwidth = T,
          staplewex = T,
          border = "blue")
          
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

# Instalação de packages --------------------------------------------------
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("moments")

library(moments)
library(dplyr)
library(readxl)
dados <- readxl::read_excel("dados/Exercícios.xlsx", sheet = "Base de Dados 1")

# a) Qual a idade média dos clientes presentes no banco de dados? ---------
idade.media <- mean(dados$Idade, na.rm = T)  
sprintf("Resp: A idade média dos clientes é de %.2f anos.", idade.media)


# b) Qual o valor do mínimo, máximo, mediana, Q1 e Q3 da variável  --------
idade.min <- min(dados$Idade)
idade.max <- max(dados$Idade)
idade.mediana <- median(dados$Idade, na.rm = T)
idade.q1 <- quantile(dados$Idade, 0.25)
idade.q3 <- quantile(dados$Idade, 0.75)
meuhist(dados$Idade, "Idades")
sprintf("Resp: Idade Mínima: %d, idade máxima: %d, Mediana: %.2f, Q1: %.2f, Q3: %.2f", idade.min, idade.max,  idade.mediana,  idade.q1, idade.q3)
sprintf("A interpração é que a faixa etária se estende de jovens adultos até velhice extrema (90 anos ou mais), porém, a média e a mediana demonstram que a maioria dos clientes são idosos na faixa dos 65 anos.")
        

# c) Existem clientes com idades discrepantes? Analise o boxplot. ---------
idade.iiq <- idade.q3-idade.q1
idade.li <- idade.q1 - 1.5 * idade.iiq
idade.ls <- idade.q3 + 1.5 * idade.iiq
idade.coeficiente_assimetria <- skewness(dados$Idade)
idade.discrepantes_a_menor <- length(dados$Idade[dados$Idade < idade.li])
idade.discrepantes_a_maior <- length(dados$Idade[dados$Idade > idade.ls])
meuboxplot(dados$Idade, "Idades")
sprintf("Resp: Visualmente não vemos idades fora do limite inferior ou superior, fato que foi comprovado fazendo a totalização das quantidades que é igual a %d",
        idade.discrepantes_a_maior + idade.discrepantes_a_menor)


# d) Existem clientes que possuem rendimento total discrepante.. --------
rendimento.valores <- dados$`Rendimento Total`
rendimento.q1 <- quantile(rendimento.valores, 0.25)
rendimento.q3 <- quantile(rendimento.valores, 0.75)
rendimento.iiq <- rendimento.q3 - rendimento.q1 
rendimento.li <- rendimento.q1 - 1.5 * rendimento.iiq
rendimento.ls <- rendimento.q3 + 1.5 * rendimento.iiq
rendimento.discrepantes_a_menor <- length(dados$`Rendimento Total`[dados$`Rendimento Total` < rendimento.li])
rendimento.discrepantes_a_maior <- length(dados$`Rendimento Total`[dados$`Rendimento Total` > rendimento.ls])
meuboxplot(rendimento.valores, "Rendimentos")
rm(rendimento.valores)
sprintf("Resp: Existem %d clientes que possuem rendimentos discrepantes na base analisada.", rendimento.discrepantes_a_menor + rendimento.discrepantes_a_maior)


# e) A partir de qual valor o rendimento é considerado discrepante? -------
sprintf("Resp: Rendimentos com valor acima de %.2f são considerados discrepantes.", rendimento.ls)

# f) A variável Rendimento Total pode ser considerada   -------------------
rendimento.coeficiente_assimetria <- skewness(dados$`Rendimento Total`)
meuhist(dados$`Rendimento Total`, "Rendimento Total")
sprintf("A variável Rendimento Total possui coeficiente de assimétria igual a %.4f, portanto, assimétrica a direita.", rendimento.coeficiente_assimetria)
summary(dados$`Rendimento Total`)

# g) Existem clientes que possuem salário discrepante ----------------------
salario.q1 <- quantile(dados$Salário, 0.25)
salario.q3 <- quantile(dados$Salário, 0.75)
salario.iiq <- salario.q3 - salario.q1
salario.li <- rendimento.q1 - 1.5 * rendimento.iiq
salario.ls <- rendimento.q3 + 1.5 * rendimento.iiq
salario.discrepantes_a_menor <- length(dados$Salário[dados$Salário < salario.li])
salario.discrepantes_a_maior <- length(dados$Salário[dados$Salário > salario.ls])
salario.coeficiente_assimetria <- moments::skewness(dados$Salário)
# dados[dados$Salário > salario.ls,]
meuhist(dados$Salário, "Salários")
meuboxplot(dados$Salário, "Salários")
sprintf("Resp: De acordo com o boxplot e analisando os valores há %d clientes com salário discrepante.", salario.discrepantes_a_menor + salario.discrepantes_a_maior)

# h) A partir de qual valor o salário é considerado discrepante?

  

# h) A partir de qual valor o salário é considerado discrepante? ----------
sprintf("Resp: Salários com valor acima de %.2f são considerados discrepantes.", salario.ls)

# i) A variável salário pode ser considerada simétrica? -------------------
sprintf("A variável Salário possui coeficiente de assimetria igual a %.4f, portanto assimétrica a direita.", salario.coeficiente_assimetria)

# j) Existem clientes que possuem limite de cheque especial        --------
cheque.q1 <- quantile(dados$`Limite do Cheque Especial`, 0.25)
cheque.q3 <- quantile(dados$`Limite do Cheque Especial`, 0.75)
summary(dados$`Limite do Cheque Especial`)
cheque.iiq <- cheque.q3 - cheque.q1
cheque.ls <- cheque.q3 + 1.5 * cheque.iiq
cheque.discrepantes_a_maior <- length(dados$`Limite do Cheque Especial`[dados$`Limite do Cheque Especial` > cheque.ls])
meuboxplot(dados$`Limite do Cheque Especial`, 'Limite Cheque Especial')
sprintf("Resp: De acordo com o boxplot e analisando os valores há %d clientes com cheque especial discrepante.", cheque.discrepantes_a_maior)

# k) A partir de qual valor o limite de cheque especial é considerado discrepante?
sprintf("Resp: Limites a partir de %.2f são considerados discrepantes.", cheque.ls)

# l) A variável limite de cheque especial pode ser considerada sim --------
cheque.coeficiente_assimetria <- skewness(dados$`Limite do Cheque Especial`)
cheque.media <- mean(dados$`Limite do Cheque Especial`)
cheque.mediana <- median(dados$`Limite do Cheque Especial`)
sprintf("Resp: A variável possui coeficiente de assimetria de %.4f, o que confirma que é assimétrica a direita.",
        cheque.coeficiente_assimetria)

  
