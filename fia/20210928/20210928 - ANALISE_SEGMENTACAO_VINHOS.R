#Instalando e carregando bibliotecas
install.packages("cluster")
install.packages("flexclust")
install.packages("NbClust")
library(flexclust)
library(NbClust)
library(cluster)
library(readxl)
library(ggplot2)
library(tidyverse)
library(factoextra)

#Importando a base de dados
wines <- read_excel("C:\\Users\\Caio Felipe\\Documents\\LabData FIA\\Aulas Setembro\\20210928 - Estudo de Caso Pt 2\\ANALISE_SEGMENTACAO_VINHOS.xlsx", sheet = "BASE_DADOS")
#View(wines)
#str(wines)

#Padronizando a base de dados
wine_standard <- scale(wines)
View(wine_standard)

# Histograma para cada atributo
wines %>%
  gather(Attributes, value, 1:13) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Wines Attributes - Histograms") +
  theme_bw()

# # Grafico de densidade para cada atributo
# wines %>%
#   gather(Attributes, value, 1:13) %>%
#   ggplot(aes(x=value, fill=Attributes)) +
#   geom_density(colour="black", alpha=0.5, show.legend=FALSE) +
#   facet_wrap(~Attributes, scales="free_x") +
#   labs(x="Values", y="Density",
#        title="Graficos de densidade para os atributos do vinho") +
#   theme_bw()

# Boxplot para cada atributo
wines %>%
  gather(Attributes, values, c(1:4, 6:12)) %>%
  ggplot(aes(x=reorder(Attributes, values, FUN=median), y=values, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Boxplots para os atributos do vinho") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  ylim(0, 35) +
  coord_flip()

#Clusterizando a base de dados
library(cluster)
kmeans_wine <- kmeans(wine_standard, 3)
attributes(kmeans_wine)
kmeans_wine$cluster
kmeans_wine$size
clusplot(wine_standard, kmeans_wine$cluster,
         main = "Representacao dos clusters",
         color = TRUE, shade = TRUE,
         labels = 2, lines = 0
)

#Concordancia dos clusters
concordancia <- table(wines$type, kmeans_wine$cluster)
concordancia
randIndex(concordancia)


#Apendice (sugestao da quantidade de clusters)
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(wine_standard)

set.seed(1234)
nc <- NbClust(wine_standard, min.nc=2, max.nc=15, method="kmeans")

table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

######################################################
