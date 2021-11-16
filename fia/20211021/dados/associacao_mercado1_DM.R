# Exercício Cesto de Compras 
#Este código é referente a aplicação de Market Basket Analysis para a base Mercado1

setwd('C:\\Users\\Caio Felipe\\Documents\\LabData FIA\\Aulas Outubro')

#install.packages('arules') # Instalando o Pacote arules
library(arules) # Carregando o pacote arules
library(arulesViz)

base = read.csv('Mercado1.csv', header = FALSE) # lendo arquivo csv de transações
base = read.transactions('mercado1.csv', sep = ',', rm.duplicates = TRUE) # tranasformando o aruivo em transações
summary(base) # Resumo da base


itemFrequencyPlot(base, topN = 7)# Gráfico de Frequência por produto

#Gráfico alternativo que destaca cada produto por uma cor diferente
library("RColorBrewer")
arules::itemFrequencyPlot(base,
                          topN=6,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency (Relative)")

regras = apriori(data = base, parameter = list(support = 0.3, confidence = 0.8)) # geração de regras com algoritmo apriori
inspect(sort(regras, by = 'lift')) # ordenação das Regras de Acordo com o Lift
plot(regras, method = "graph",  engine = "htmlwidget") # Gráfico Interativo de Regras
