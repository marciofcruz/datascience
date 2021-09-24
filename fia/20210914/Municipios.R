wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

rm(list = ls())

options(scipen=999)
options(width = 1024)


#Instalacao de pacotes (base realizar 1 vez)
# install.packages("cluster")
# install.packages("factoextra")
# install.packages("gridExtra")

#**********************************************************
#Leitura da Base 
library(readxl)
municipios <- read_excel("dados/Municipios.xlsx", sheet = 'Base de Dados')
nrow(municipios)
ncol(municipios)
#**********************************************************

#**********************************************************
# Faca uma analise exploratoria da base de dados 
# (obtenha as medidas de posicao e dispersao). 
summary(municipios[,-1]) #Min, Q1, Q2, Q3 e Max
apply(municipios[,-1] , 2 , sd) #Desvio Padrao

#Considerando o histograma das variaveis hab e pib, as distribuicoes sao simetricas?
hist(municipios$hab, main="Habitantes")  
hist(municipios$pib,main="PIB") 

#Considerando as variaveis pop15 e pop60, qual possui a maior variabilidade?
sd(municipios$pop15)/mean(municipios$pop15)*100
sd(municipios$pop60)/mean(municipios$pop60)*100

#Existe outlier nas variaveis nas variaveis pop15 e pop60?
par(mfrow=c(1,2))
boxplot(municipios$pop15, col="paleturquoise", main="pop15")
boxplot(municipios$pop60, col="darkturquoise", main="pop60")

#**********************************************************
#Padronize as variaveis.
municipios_z<-scale(municipios[,-1])
head(municipios_z)

#Calcule a matriz de distancias euclidianas 
distancia <- dist(municipios_z, method="euclidean") #Calculo das distancias euclidianas
distancia

#Faca a analise de agrupamento com as variaveis padronizadas
#usando os 2 metodos apresentados, escolha um dos metodos 
#e justifique a quantidade de grupos apos a analise do Dendrograma.

#Matriz de graficos de dimensao 1 linhas x 2 colunas
par(mfrow=c(1,2))

clust_single <- hclust(distancia, method="single") 
plot(clust_single, main="Metodo Single", hang=-1, labels=FALSE) #hang=-1 para deixar todos iniciando do zero

clust_complete <- hclust(distancia, method="complete")
plot(clust_complete, main="Metodo Complete", hang=-1, labels=FALSE)

#Utilize o metodo K-Medias 
#Qual numero de grupos e melhor?
set.seed(12345) # Ao mudar essa semente, o agrupamento muda

library(cluster)    
library(factoextra) 
library(gridExtra)

#Metodo K-means, 2 grupos
dados.k2 <- kmeans(municipios_z, centers = 2, nstart = 25 , iter.max = 100)
#kmeans e o codigo para criacao do algoritimo kmeans
#centers e o numero de grupos, ou seja, numero de centroides
#nstart e o numero de tentativas de centroides iniciais
#iter.max e o numero maximo de interacoes simuladas para obter o melhor controide, ou seja, melhor agrupamento

#Visualizar os clusters
fviz_cluster(dados.k2, data = municipios_z, main = "Cluster K2")

# numero de observacoes em cada grupo
table(dados.k2$cluster)

#Agora rodar de 3 a 5 grupos e visualizar qual a melhor divisao
dados.k3 <- kmeans(municipios_z, centers = 3, nstart = 25, iter.max = 100)
dados.k4 <- kmeans(municipios_z, centers = 4, nstart = 25, iter.max = 100)
dados.k5 <- kmeans(municipios_z, centers = 5, nstart = 25, iter.max = 100)

#Graficos
G1 <- fviz_cluster(dados.k2, geom = "point", data = municipios_z) + ggtitle("k = 2")
G2 <- fviz_cluster(dados.k3, geom = "point", data = municipios_z) + ggtitle("k = 3")
G3 <- fviz_cluster(dados.k4, geom = "point", data = municipios_z) + ggtitle("k = 4")
G4 <- fviz_cluster(dados.k5, geom = "point", data = municipios_z) + ggtitle("k = 5")

#Criar uma matriz com 4 graficos
grid.arrange(G1, G2, G3, G4, nrow = 2)

#Agrupar cluster e base - foi considerado 3 grupos
municipios<- data.frame(municipios, dados.k3$cluster)
head(municipios)

# numero de observacoes em cada grupo
table(dados.k3$cluster)

#Analise as caracteristicas de cada grupo.
#Distribuicao das variaveis por cluster
par(mfrow=c(2,4)) #coloca os graficos lado a lado
boxplot(municipios$pop15 ~ municipios$dados.k3.cluster, col="paleturquoise", main="% com ate 15 anos")
boxplot(municipios$pop60 ~ municipios$dados.k3.cluster, col="darkturquoise", main="% com 60 anos ou mais")
boxplot(municipios$hab ~ municipios$dados.k3.cluster, col="paleturquoise", main="Densidade demografica")
boxplot(municipios$area ~ municipios$dados.k3.cluster, col="darkturquoise", main="Area")
boxplot(municipios$taxa ~ municipios$dados.k3.cluster, col="darkturquoise", main="Taxa de natalidade")
boxplot(municipios$esgoto ~ municipios$dados.k3.cluster, col="paleturquoise", main="% saneamento basico")
boxplot(municipios$emprego ~ municipios$dados.k3.cluster, col="darkturquoise", main="% habitantes empregados")
boxplot(municipios$pib ~ municipios$dados.k3.cluster, col="paleturquoise", main="PIB per capita")


#Analise as caracteristicas de cada grupo.
#Atribui a cada pais o cluster a qual ele pertence pela variavel cluster
municipios$cluster <- as.factor(cutree(clust_complete, k=4))

#Tamanho dos Clusters
table(municipios$cluster)

#Faz BoxPlot para cada variavel e compara por cluster
#Distribuicao das variaveis por cluster
par(mfrow=c(2,4)) #coloca os graficos lado a lado
boxplot(municipios$pop15 ~ municipios$cluster, col="paleturquoise", main="% com ate 15 anos")
boxplot(municipios$pop60 ~ municipios$cluster, col="darkturquoise", main="% com 60 anos ou mais")
boxplot(municipios$hab ~ municipios$cluster, col="paleturquoise", main="Densidade demografica")
boxplot(municipios$area ~ municipios$cluster, col="darkturquoise", main="Area")
boxplot(municipios$taxa ~ municipios$cluster, col="darkturquoise", main="Taxa de natalidade")
boxplot(municipios$esgoto ~ municipios$cluster, col="paleturquoise", main="% saneamento basico")
boxplot(municipios$emprego ~ municipios$cluster, col="darkturquoise", main="% habitantes empregados")
boxplot(municipios$pib ~ municipios$cluster, col="paleturquoise", main="PIB per capita")



