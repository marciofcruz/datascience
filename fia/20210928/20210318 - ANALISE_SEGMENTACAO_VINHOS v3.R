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
wines <- read_excel("C:\\Users\\Caio Felipe\\Documents\\LabData FIA\\20210318 - Aula - Estudo de casos\\ANALISE_SEGMENTACAO_VINHOS.xlsx", sheet = "BASE_DADOS")
#View(wines)
str(wines)

table(wines$type) %>% prop.table()*100

#Padronizando a base de dados
wine_standard <- scale(wines)
View(wine_standard)

# Histograma para cada atributo
hist(wines$alcohol)

wines %>%
  gather(Attributes, value, 1:13) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Wines Attributes - Histograms") +
  theme_bw()

# Grafico de densidade para cada atributo
wines %>%
  gather(Attributes, value, 1:13) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_density(colour="black", alpha=0.5, show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Graficos de densidade para os atributos do vinho") +
  theme_bw()

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
#library(cluster)
set.seed(123)
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
set.seed(1234)

sil <- silhouette(kmeans_wine$cluster, dist(wine_standard))
fviz_silhouette(sil)

boxplot()
######################################################

#Quer mensurar um valor (um número).. Geralmente trabalhamos com um modelo de Regressão Linear Múltipla.

mcor <- cor(wines) %>% as.data.frame()
head(wines)

#phenols e flavanoids | phenols e diluition | diluittion e flavanoids

modelo1 <- lm(data=wines,
                alcohol ~ 
                  malic+ash+alcalinity+magnesium+
                +proantho+color+hue+diluition+proline)
summary(modelo1)

modelo2 <- lm(data=wines,
              alcohol ~ 
                malic+ash+alcalinity+
                +proantho+color+hue+diluition+proline)
summary(modelo2)

modelo3 <- lm(data=wines,
              alcohol ~ 
                malic+alcalinity+
                +proantho+color+hue+diluition+proline)
summary(modelo3)

modelo4 <- lm(data=wines,
              alcohol ~ 
                malic+alcalinity+
                +proantho+color+diluition+proline)
summary(modelo4)


#MODELO 5 TEM TODAS AS VARIÁVEIS SENDO SIGNIFICATIVAS
modelo5 <- lm(data=wines,
              alcohol ~ 
                malic + alcalinity+
                color+diluition+proline)
summary(modelo5)

modelo6 <- lm(data=wines,
              alcohol ~
                malic +
                alcalinity+
                +color+proline)
summary(modelo6)

modelo7 <- lm(data=wines,
              alcohol ~ 
                alcalinity+
                +color+proline)
summary(modelo7)

modelo8 <- lm(data=wines,
              alcohol ~ 
                color+proline)
summary(modelo8)

modelo9 <- lm(data=wines,
              alcohol ~ 
                color)
summary(modelo9)

wines$predito <- predict(modelo5,data=wines)
wines$predito2 <- predict(modelo8,data=wines)

filtro <- wines[,c('alcohol','predito','predito2')]
filtro$diff<- filtro$preditofiltro$alcohol
filtro$diff2 <- filtro$predito2-filtro$alcohol

plot(filtro$diff)
plot(filtro$diff2)
hist(filtro$diff)
hist(filtro$diff2)

shapiro.test(filtro$diff)
shapiro.test(filtro$diff2)

#Shapiro-wilk é um teste que avalia se um determinado conjunto de dados tem distribuição normal
#H0: Os dados têm distribuição normal.
#H1: Os dados não têm distribuição normal.