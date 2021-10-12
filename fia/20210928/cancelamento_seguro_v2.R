wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())

options(scipen=999)
options(width = 1024)

library(readxl)
library(lubridate)

# Carregar Dados ----------------------------------------------------------

dados <- read_excel("dados/Cancelamento de seguros v2.xlsx", sheet = "BASE DE DADOS")

str(dados)

length(unique(dados$id))

dados2 <- dados[,-c("id")]

dados$id <- NULL
unique(dados$id) %>% length()


# Analise Univariada ------------------------------------------------------
head(dados)

table(dados$produto) %>% prop.table()
table(dados$Pagamento) %>% prop.table()
table(dados$uf) %>% prop.table()

table(dados$est_civ) %>% prop.table()
table(dados$sexo) %>% prop.table()      


dados[dados$uf == 'CE',]
?difftime


dados$regiao <-ifelse(dados$uf %in% c("SP", "RJ", "ES", "MG"), "SUDESTE",
                    ifelse(dados$uf %in% c("RS", "SC", "PR"), "SUL", 
                           ifelse(dados$uf %in% c("DF", "MT", "MS", "GO"), "CENTRO-OESTE", 
                                  ifelse(dados$uf %in% c("AL", "PI", "CE", "PE", "PB", "RN", "SE", "BA", "MA"), "NORDESTE",
                                         ifelse(dados$uf %in% c("TO", "AM", "PA", "RR", "RO", "AC", "AP"), "NORTE", "SEM INFO")))))  

str(dados)

dados[dados$regiao == "SEM INFO",]

dados$dtnasc <- as.Date(dados$dt_nasc)

lubridate::now()-dados$dt_nasc

dados$idade <- difftime(lubridate::now(), dados$dtnasc, units = "days") / 365.25 %>% as.integer()
str(dados$idade)


summary(dados$idade)         
