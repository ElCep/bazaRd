#Script de traitement des données de l'inventaire viticole de 1957
#numérisation réaliser pendant un stage au CAAPG (Savoie)
#Auteur : E.DELAY (Laboratoire GEOLAB, université de Limoges)
# date  27 septembre 2014

#### chargement necessaire ####
##librairies
require(ggplot2)
require(rgdal)
require(plyr)

##fonctions
source("script&R/fonc_multiplot.R")

#### directory ####
setwd("~/github/bazaRd/encepagement_CAAPG/")

#### read data ####
##csv
savoie<-read.csv("data_inventaire_1957/inventaire_1957_Savoie.csv",header = T)
sumSavoie<-sum(savoie$Total)
hsavoie<-read.csv("data_inventaire_1957/inventaire_1957_HauteSavoie.csv",header = T)

#transformation factor pour la jointure
savoie$Num_com<-as.factor(savoie$Num_com)
hsavoie$Num_com<-as.factor(hsavoie$Num_com)

#change nom colonne
colnames(savoie)[2]<-"INSEE_COM"
colnames(hsavoie)[2]<-"INSEE_COM"

##donées geo
shp_savoie<-readOGR(dsn="./GEOFLA_dep_73_74/",layer = "savoie")

#### jointure data #####
# On ne peut pos joindre les deux df car il n'ont pas le même nombre de colone'
shp_savoie@data<-join(shp_savoie@data,savoie,by='INSEE_COM')


#remove NA by 0
savoie[is.na(savoie)]<-0
