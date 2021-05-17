setwd("D:/projetos/github.com/datascience/alura/r_io-dataset")
getwd()

install.packages("xlsx")

library(xlsx)

# excel <- read.xlsx(file="dados/reclamacao_2010_2011.xlsx", sheetIndex=1)
dados_2010  <- read.xlsx2(file="dados/reclamacao_2010_2011.xlsx", sheetIndex=1)
dados_2011  <- read.xlsx2(file="dados/reclamacao_2010_2011.xlsx", sheetIndex="2011", stringsAsFactors = F)

