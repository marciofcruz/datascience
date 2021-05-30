
# inicialização -----------------------------------------------------------
wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variÃ¡vel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())


# Instalação de packages --------------------------------------------------
# install.packages("readxl")
# install.packages("dplyr")

dados <- readxl::read_excel("dados/Exercícios.xlsx", sheet = "Base de Dados 1")

# Resolução ---------------------------------------------------------------

#a) Qual a idade média dos clientes presentes no banco de dados? 
idade_media <- mean(dados$Idade, na.rm = T)  

#b) Qual o valor do mínimo, máximo, mediana, Q1 e Q3 da variável idade? Interprete os valores. 
summary(dados$Idade)
q1_idade <- quantile(dados$Idade, 0.25)
q3_idade <- quantile(dados$Idade, 0.75)
iic_idade <- q3_idade-q1_idade
li_idade <- q1_idade - 1.5 * iic_idade
ls_idade <- q3_idade + 1.5 * iic_idade
min_idade <- min(dados$Idade)
max_idade <- max(dados$Idade)

#c) Existem clientes com idades discrepantes? Analise o boxplot.


# (a) Qual a idade média dos clientes presentes no banco de dados? 
q1_idade <- quantile(dados$Idade, 0.25)
q3_idade <- quantile(dados$Idade, 0.75)
iic_idade <- q3_idade-q1_idade
li_idade <- q1_idade - 1.5 * iic_idade
ls_idade <- q3_idade + 1.5 * iic_idade
qtde_discrepante_menor_idade <- length(dados$Idade[dados$Idade < li_idade])
qtde_discrepante_maior_idade <- length(dados$Idade[dados$Idade > ls_idade])
boxplot(dados$Idade,
        main = "Boxplot Idade",
        Ylab = "Idades",
        col = "pink",
        horizontal = T,
        varwidth = TRUE,
        staplewex = T,
        border = "blue"
        )


# c) Existem clientes com idades discrepantes? Analise o boxplot.
sprintf("Quantidade de dados discrepantes de idade: %d", qtde_discrepante_menor_idade + qtde_discrepante_menor_idade)
sprintf("Pois, não idades menores que o LI %.2f e LS %.2f", li_idade, ls_idade)


# d) Existem clientes que possuem rendimento total discrepante em relação aos demais clientes? Analise o boxplot. 
