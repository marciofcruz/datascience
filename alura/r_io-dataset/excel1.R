setwd("D:/projetos/github.com/datascience/alura/r_io-dataset")
getwd()

install.packages("xlsx")

library(xlsx)

# excel <- read.xlsx(file="dados/reclamacao_2010_2011.xlsx", sheetIndex=1)
dados_2010  <- read.xlsx2(file="dados/reclamacao_2010_2011.xlsx", sheetIndex=1)
dados_2011  <- read.xlsx2(file="dados/reclamacao_2010_2011.xlsx", sheetIndex="2011", stringsAsFactors = F)

dados_2010_limpos <- dados_2010[,-17]
colnames(dados_2010_limpos)

write.xlsx(x = dados_2010_limpos, file='dados_reclamacao_2010_2011_limpo.xlsx', row.names = FALSE,
           sheetName='2010')

dados_2011_limpos <- dados_2011[,-17]
write.xlsx(x = dados_2011_limpos, file='dados_reclamacao_2010_2011_limpo.xlsx', row.names =F,
           sheetName='2011')

rm(dados_2010, dados_2011, excel)
