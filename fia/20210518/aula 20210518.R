wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretório definido na variável de ambiente RWD não foi encontrado",wd, sep="="))
} 

setwd(wd)
print(paste("Diretório atual de trabalho:", getwd()))

rm(wd)

