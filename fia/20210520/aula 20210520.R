
# inicialização -----------------------------------------------------------


wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variÃ¡vel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
# 

rm(list = ls())


StartDate <- as.Date("2020/2/28")
StopDate <- as.Date("2020/3/1")
StopDate - StartDate
