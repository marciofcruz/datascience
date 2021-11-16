# Exercício Cesto de Compras 
#Este código é referente a aplicação de Market Basket Analysis para a base Mercado2

setwd('C:\\Users\\Caio Felipe\\Documents\\LabData FIA\\Aulas Outubro')

# Carregando Pacotes necessários para este Exercício
library(arules)
library("RColorBrewer")


base = read.transactions('Mercado2.csv', sep = ',', rm.duplicates = TRUE) # Carregando a base de Dados Mercado2
summary(base) # Resumo da Tabela Transacional

#Gráfico de Frequência por Produto ( TOP20 produtos)
arules::itemFrequencyPlot(base,
                          topN=20,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency (Relative)")

regras = apriori(data = base, parameter = list(support = 0.003, confidence = 0.2)) # Aplicação do Algoritmo Apriori
inspect(sort(regras, by = 'lift')[1:20]) # Selecionando as 20 regras de acordo com o Lift
plot(regras, method = "graph",  engine = "htmlwidget") # Gráfico Interativo de Regras
