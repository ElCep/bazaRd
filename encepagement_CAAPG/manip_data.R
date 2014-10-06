#Script de traitement des données de l'inventaire viticole de 1957
#numérisation réaliser pendant un stage au CAAPG (Savoie)
#Auteur : E.DELAY (Laboratoire GEOLAB, université de Limoges)
# date  27 septembre 2014

#### chargement necessaire ####
##librairies
require(ggplot2)
require(rgdal)
require(plyr)
require(classInt)

##fonctions
source("fonc_multiplot.R")

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
row.names(savoie)<-NULL
colnames(hsavoie)[2]<-"INSEE_COM"


##donées geo
communesFr<-readOGR(dsn="./communes_geofla/",layer = "communes")
sel<-communesFr@data$CODE_DEPT=="73"

spSavoie<-communesFr[sel,]
plot(spSavoie)
#### jointure data #####
# On ne peut pos joindre les deux df car il n'ont pas le même nombre de colone'
spSavoie@data$INSEE_COM<-as.character(spSavoie@data$INSEE_COM)
savoie$INSEE_COM<-as.character(savoie$INSEE_COM)


#transforme shp to df for ggplot
spSavoie@data$id<-rownames(spSavoie@data)
spSavoie.point<-fortify(spSavoie,region='id')
spSavoie.df<-join(spSavoie.point,spSavoie@data, by="id")
spSavoie.df<-join(spSavoie.df, savoie, by='INSEE_COM', type = 'left')

nameIndex<-names(spSavoie.df)
for (i in 27:length(spSavoie.df[1,])){
  print(i)
  space<-spSavoie.df[,1:7]
  variety<-spSavoie.df[,i]
  variety<-as.numeric(as.character(variety))
  variety[is.na(variety)]<-0
  nb_viti_country <- sum(variety > 0)
  if(nb_viti_country > 10){
    ##classification de jenks sur variety
    classif<-classIntervals(variety,n=4,style="kmeans")
    #pal1 <- c("wheat1", "red3") #definition des extrémités de la palette de couleurs
    #plot(classif,pal=pal1) #plot des effectifs cumulés
    space$class<-as.factor(findCols(classif))
    if (max(as.numeric(as.character(space$class)))>3){
      brks<-round(classif$brks,digits=2) ##definition des breaks
      
      ##préparation de la legende
      # Create labels from break values
      intLabels <- matrix(1:(length(brks)-1))
      for(j in 1:length(intLabels )){intLabels [j] <- paste(as.character(brks[j]),"-",as.character(brks[j+1]))}
      
      #carto avec GGplot2
      png(paste("map_savoie_1957/",nameIndex[i],"_k.png",sep=""), width=900,height=550)
      cepMapping<-ggplot()+
        geom_polygon(data=space,aes(long,lat,group=group,fill=class))+
        geom_path(data=space, aes(x=long,y=lat,group=group),color="grey30")+
        theme_bw()+
        scale_fill_brewer(labels = intLabels)+
        labs(fill = "Intervales \n kmeans \n sur les surfaces \n (ha)", 
             title=paste("Classification du cépage ",nameIndex[i] ," \n par communes en 1957",sep=""), 
             x="longitude", y="latitude")+
        annotate("text", label = "Sources: IGN geoflat & inventaire national des cépages 1957", x = 950000, y = 6440000, size = 5)+
        annotate("text", label = "Réalisation : E. Delay", x = 920000, y = 6445000, size = 5)+
      coord_equal()
      print(cepMapping)
      dev.off()
    }
  }
}

#remove NA by 0
savoie[is.na(savoie)]<-0
