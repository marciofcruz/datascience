
# inicialização -----------------------------------------------------------
wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())


# Carregar bibliotecas ----------------------------------------------------
library(readr)
# install.packages("tidyverse")
# install.packages("magrittr")

# install.packages("tidyr")

library(dplyr)
library(tidyverse)
library(magrittr)
library(tidyr)


#aluguel <- read.csv("https://raw.githubusercontent.com/alura-cursos/Analise-de-Dados-com-R/master/aluguel.csv", sep = ";")
#write.csv(aluguel, "aluguel.csv")

aluguel <- read.csv("aluguel.csv")
aluguel

summary(aluguel)

str(aluguel)


dim(aluguel[is.na(aluguel$IPTU),])

aluguel_t <- unique(aluguel)
head(aluguel_t)
rm(aluguel)

aluguel_t

# Selecionando os campos --------------------------------------------------
dim(aluguel_t)
selec1 <- select(aluguel_t, ï..Tipo, Bairro, Area, Valor)
selec1
glimpse(selec1)


# Retirando campos --------------------------------------------------------
select2 <- select(aluguel_t, -IPTU)
select2

# Usando seleção contendo alguns caracteres -------------------------------

# Filtragem ---------------------------------------------------------------

names(aluguel_t)

unique(selec1$ï..Tipo)

head(aluguel_t)
arrange(selec1, desc(Valor))


imoveis <- mutate(selec1, indicador = (Area/Valor) * 100)
imoveis




# Agrupar e Sumarizar -----------------------------------------------------

aluguel_t %>% 
  group_by("ï..Tipo") 


aluguel_t %>% 
  group_by("Bairro")  %>% 
  summarise(valorMedio = mean(Valor, na.rm = T))


summary(aluguel_t)


# Relacionamentos ---------------------------------------------------------
cargo = data.frame(c("Andre", "Marcos", "Fernando", "Julia"),
                   c("Analista", "Analista", "Coordenador", "Gerente"))
colnames(cargo) = c("nome", "cargo")

local <- data.frame(c("Fabio", "Marcos", "Fernanda", "Julia"),
                    c("Comercial", "CRM", "TI", "RH"))
colnames(local) <- c("nome", "local")


merge(x = cargo, y = local, by = "nome", all.x = T)
