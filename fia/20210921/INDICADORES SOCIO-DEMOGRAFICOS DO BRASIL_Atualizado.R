wd <- Sys.getenv("RWD")

if (!file.exists(wd)) {
  stop(paste("O diretorio definido na variavel de ambiente RWD nao foi encontrado",wd, sep="="))
} 

setwd(wd)
getwd()

setwd("C:/Projetos/github.com/datascience/fia/20210921")

rm(list = ls())

options(scipen=999)
options(width = 1024)



#Instalar e Carregar bibliotecas
# install.packages("devtools")

# devtools::install_github("rpradosiqueira/brazilmaps")
#install.packages("geobr")
#install.packages("leaflet")
#install.packages("lwgeom")
#install.packages("sf")
#install.packages("viridis")
#install.packages("maptools")
#install.packages("vctrs")
library(Rcpp)
library(readxl)
library(tidyverse)
library(brazilmaps)
library(geobr)
library(leaflet)
library(lwgeom)
library(viridis)
library(sf)
library(maptools)
library(ggplot2)
library(vctrs)


#Importacao da base de dados
ind <- read_excel("dados/INDICADORES SOCIO-DEMOGRAFICOS DO BRASIL.xlsx", sheet="BASE_DADOS")
View(ind)

#Filtrar cidades com mais de 50mil habitantes
ind_over50mil <- subset(ind, IBGE_RES_POP>50000)
#View(ind_over50mil)

#Ordenar as cidades de acordo com o IDHM
ind_over50mil <- ind_over50mil[order(ind_over50mil$IDHM, decreasing=TRUE), ]
ind_over50mil_sel<-ind_over50mil[,c(1,2,11)]
#View(ind_over50mil_sel)
head(ind_over50mil_sel,10)

#Calcular o indicador assinantes de TV paga a cada 100mil habitantes
ind$TV_CABO_per100mil <- round(((ind$TV_CABO/ind$IBGE_RES_POP)*100000),0)
#View(ind)

#Ordenar as cidades de acordo com o indice de TV paga a cada 100mil habitantes
ind_tvpaga100mil <- subset(ind[,c(1,2,3,17,24)],IBGE_RES_POP > 50000)
ind_tvpaga100mil <- ind_tvpaga100mil[order(ind_tvpaga100mil$TV_CABO_per100mil, decreasing=TRUE), ]
#View(ind_tvpaga100mil)
head(ind_tvpaga100mil,10)

#Calcular o indicador motos a cada 100mil habitantes
ind$motos_100mil <- round(((ind$MOTOS/ind$IBGE_RES_POP)*100000),0)
#View(ind)

#Ordenar as cidades de acordo com o indice de motos a cada 100mil habitantes
ind_moto100mil<-subset(ind[,c(1,2,3,22,25)],IBGE_RES_POP>50000 & !is.na(motos_100mil))
#View(ind_moto100mil)
ind_moto100mil <- ind_moto100mil[order(ind_moto100mil$motos_100mil, decreasing=TRUE), ]
#View(ind_moto100mil)
head(ind_moto100mil,10)

#Calcular o indicador carros a cada 100mil habitantes
ind$carros_100mil<-round(((ind$CARROS/ind$IBGE_RES_POP)*100000),0)
#View(ind)

#Ordenar as cidades de acordo com o indice de carros a cada 100mil habitantes
ind_carros100mil<-subset(ind[,c(1,2,3,21,26)],IBGE_RES_POP>50000 & !is.na(carros_100mil))
#View(ind_carros100mil)
ind_carros100mil <- ind_carros100mil[order(ind_carros100mil$carros_100mil, decreasing=TRUE), ]
#View(ind_carros100mil)
head(ind_carros100mil,10)

#Correlacao entre IDHM e carros por 100mil habitantes
ind_IDHM_carros100mil<-subset(ind[,c(1,2,3,11,12,13,14,26)])
View(ind_IDHM_carros100mil)
#sum(is.na(ind_IDHM_carros100mil))
sapply(ind_IDHM_carros100mil, function(x) sum(is.na(x)))
ind_IDHM_carros100mil<-ind_IDHM_carros100mil %>% drop_na()
#View(ind_IDHM_carros100mil)
round(cor(ind_IDHM_carros100mil[,c(3,4,5,6,7,8)]),2)

#Criar a variavel regiao na base
#Criando a variavel REGIAO na tabela
ind$Regiao<-ifelse(ind$ESTADO %in% c("SP", "RJ", "ES", "MG"), "SUDESTE",
                    ifelse(ind$ESTADO %in% c("RS", "SC", "PR"), "SUL", 
                           ifelse(ind$ESTADO %in% c("DF", "MT", "MS", "GO"), "CENTRO-OESTE", 
                                  ifelse(ind$ESTADO %in% c("AL", "PI", "CE", "PE", "PB", "RN", "SE", "BA", "MA"), "NORDESTE",
                                         ifelse(ind$ESTADO %in% c("TO", "AM", "PA", "RR", "RO", "AC", "AP"), "NORTE", "SEM INFO")))))                  
#View(ind)
ind_GDPpercapita<-ind[,c(1,2,3,20,27)]
#View(ind_GDPpercapita)

#Ordenar por GDP per capita
ind_GDPpercapita <- ind_GDPpercapita[order(ind_GDPpercapita$PIB_CAPITA, decreasing=TRUE), ]
sum(is.na(ind_GDPpercapita))
ind_GDPpercapita<-ind_GDPpercapita %>% drop_na()
#View(ind_GDPpercapita)
ind_GDPpercapita$ranking<-rank(-ind_GDPpercapita$PIB_CAPITA)
View(ind_GDPpercapita)

#Obter os top 100 com maiores GDP per capita e fazer a distribuicao por regiao
ind_GDPpercapita100<-subset(ind_GDPpercapita,ranking<=100)
#View(ind_GDPpercapita100)
prop.table(table(ind_GDPpercapita100$Regiao))


#Obter os top 100 com menores GDP per capita e fazer a distribuicao por regiao
ind_GDPpercapita_100piores<-subset(ind_GDPpercapita,ranking>=5465)
#View(ind_GDPpercapita_100piores)
prop.table(table(ind_GDPpercapita_100piores$Regiao))

#Desenhar um mapa do Brasil com relacao a IDHM e assinantes de TV paga por 100mil habitantes
#Agrupar a base de indicadores por estado
ind$TV_CABO_100mil<-(ind$TV_CABO/ind$IBGE_RES_POP)*100000
#View(ind)
ind<- ind %>% drop_na()
#View(ind)

ind_estado<-ind %>%
  group_by(ESTADO) %>%
  summarise(IDHM_estado=mean(IDHM),
            TV_CABO_100mil_estado=mean(TV_CABO_100mil)) %>%
            arrange(ESTADO)
View(ind_estado)

#Mapa do Brasil em preto e branco
theme_set(theme_bw())
mapa <- brazilmaps::get_brmap("State")
mapa <- read_state(showProgress = FALSE)
class(mapa)
View(mapa)
mapa$ESTADO<-mapa$abbrev_state
View(mapa)

ggplot(mapa)+ 
  geom_sf()

#IDHM Medio por estado
mapa %>% 
  left_join(ind_estado, by = "ESTADO") %>% 
  ggplot(aes(fill = IDHM_estado), color = "black") +
  geom_sf() + 
  scale_fill_viridis(name = "IDHM Medio por estado", direction = -1)

#Media de assinantes de TVpaga a cada 100mil habitantes, por estado
mapa %>% 
  left_join(ind_estado, by = "ESTADO") %>% 
  ggplot(aes(fill = TV_CABO_100mil_estado), color = "black") +
  geom_sf() + 
  scale_fill_viridis(name = "TVpaga a cada 100mil hab", direction = -1)

#Forma alternativa de mostrar
coord_pontos <- mapa %>% 
  left_join(ind_estado, by = "ESTADO") %>% 
  st_centroid()

ggplot(mapa)+ 
  geom_sf() + 
  geom_sf(data = coord_pontos, aes(size = IDHM_estado), col = "blue", alpha = .65,
          show.legend = "point") + 
  scale_size_continuous(name = "IDHM Medio por Estado")

#Concentracao de riqueza usando GDP
GDP<-ind[,c(1,2,3,19)]
#View(GDP)
GDP<- GDP %>% drop_na()
#View(GDP)
GDP <- GDP[order(GDP$PIB, decreasing=TRUE), ]
GDP$acumulado <- cumsum(GDP$PIB)
GDP$rankingPIB<-rank(-GDP$PIB)
GDP$percacum_PIB<-round(((cumsum(GDP$PIB)/sum(GDP$PIB))*100),2)
GDP$perc_cidades<-round(((GDP$rankingPIB/5565)*100),2)
GDP$CIDADE <- as.factor(GDP$CIDADE)
class(GDP$CIDADE)
View(GDP)

#Diagrama de Pareto usando o ggplot2 (exemplo de quando ele pode ser inadequado)

#GDP <- GDP[1:100,]
ggplot(GDP, aes(x=GDP$rankingPIB)) +
  geom_bar(aes(y=GDP$PIB), fill='blue', stat="identity") +
  geom_point(aes(y=GDP$acumulado), color = rgb(0, 1, 0), pch=16, size=1) +
  geom_path(aes(y=GDP$acumulado, group=1), colour="slateblue1", lty=3, size=0.9) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs(title = "Diagrama de Pareto para concentra??o de PIB por cidade", subtitle = "FIA Labdata", x = 'Cidade', y ='PIB')





