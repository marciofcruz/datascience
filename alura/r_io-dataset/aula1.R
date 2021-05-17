setwd("D:/projetos/github.com/datascience/alura/r_io-dataset")
getwd()


# install.packages("jsonlite")
# install.packages("RJSONIO")
# install.packages("data.table")
library(jsonlite)
library(RJSONIO)
library(data.table)

reclamacao_string <- paste(readLines('dados/reclamacao_2009.json'), collapse='')
reclamacao_2009_ant <- fromJSON(reclamacao_string)

reclamacao_2009 <- RJSONIO::fromJSON('dados/reclamacao_2009.json', encodming="utf-8")

dados_2009 <- rbindlist(l = reclamacao_2009, fill=T)

head(dados_2009)

rm(reclamacao_string, reclamacao_2009_ant, reclamacao_2009)

table(dados_2009$UF)

dados_2009_limpos <- dados_2009[dados_2009$UF != "N/D",]

sub_json <- jsonlite::toJSON(dados_2009_limpos)

write(sub_json, 'dados/reclamacao_2009_limpos.json')

sub_json <- RJSONIO::toJSON(dados_2009_limpos)
write(sub_json, 'dados/reclamacao_2009_limpos.json')

system.time(
reclamacao_2009_ant <-jsonlite::fromJSON(paste(readLines('dados/reclamacao_2009.json'), collapse=''))
)

system.time(reclamacao_2009 <- RJSONIO::fromJSON('dados/reclamacao_2009.json', encodming="utf-8"))

system.time(jsonlite <- jsonlite::toJSON(dados_2009_limpos))

sub_json <- RJSONIO::toJSON(dados_2009_limpos)
sub_json
