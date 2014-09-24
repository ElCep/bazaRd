#Script de traitement des données de l'encycolpedie des cépages d'adelaide
# http://www.adelaide.edu.au/wine-econ/databases/winegrapes/
#Auteur : E.DELAY (Laboratoire GEOLAB, université de Limoges)
# date  21 janvier 2014 

#librairies
require("rgdal") # requires sp, will use proj.4 if installed
require("rgeos")
require("ggplot2")
require("plyr")
require("classInt")#pour faire des classifications univariée

rm(list=ls())

setwd("~/github/bazaRd/adelaide_wine/")
data2000<-read.csv("data/csv/national_2000.csv",head=TRUE,sep=",",skip=1)

data2000<-t(data2000)
#head(data2000)
data2000<-as.data.frame(data2000)
#transformer les colonnes en numérique


#rename col with cepages
colNames<-data2000[1,]
colNames<-data.frame(lapply(colNames,as.character), stringsAsFactors=FALSE)
names(data2000)[1:length(colNames)]<-colNames
data2000<-data2000[-1,] #cancel a blank line
data2000<-data.frame(as.character(rownames(data2000)),data2000) #to make a real colonne with row.name colonne
names(data2000)[1]<-"place"
names(data2000)[2]<-"iso3"



#transformer les colonnes en numérique
for(i in 3:length(data2000[1,])){
  #   data2000[i,]<-as.numeric(levels(data2000[i,])[data2000[i,]])
  data2000[,i]<-as.numeric(as.character(data2000[,i]))
}

#remove NA by 0
data2000[is.na(data2000)]<-0

#faire un df avec les valeurs mondiales
codeIso<-data2000$iso3
surfaces_viti_world<-NULL
for(i in 1:length(data2000[,1])){
  tps<-sum(data2000[i,3:length(data2000[1,])])
  surfaces_viti_world<-c(surfaces_viti_world,tps)
}
prop_viti_world<-surfaces_viti_world/sum(surfaces_viti_world)*100
Ws<-as.data.frame(cbind(as.character(codeIso),surfaces_viti_world,prop_viti_world))
colnames(Ws)<-c("iso3","surfaces_viti_world","prop_viti_world")


#transformer en proportion les valeurs de cépages ni/ntot *100
for(i in 3:length(data2000[i,])){
  data2000[,i]<-data2000[,i]/sum(data2000[,i])*100
}

geolocep<-join(data2000,Ws,by="iso3")


##transformer les colonnes qui sont en factor en numeric
for(i in 3:length(geolocep[1,])){
  geolocep[,i]<-as.numeric(as.character(geolocep[,i]))
}


##readSHP
##Prepare word shp pour récupérer la projection
word<-readOGR(dsn="data/world/ne_110m_admin_0_countries.shp", layer="ne_110m_admin_0_countries")


word@data$iso3<-word@data$gu_a3
word@data<-join(word@data,geolocep,by="iso3" )

#transforme shp to df for ggplot
word@data$id<-rownames(word@data)
word.point<-fortify(word,region="id")
word.df<-join(word.point,word@data, by="id")


##Tenttative de generaliser la creation de carte pour chaque cépage
nameIndex<-names(word.df)
for (i in 19:length(data2000[1,])){
  print(i)
  space<-word.df[,1:18]
  variety<-word.df[,i]
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
    png(paste("map2000/",nameIndex[i],"_k.png",sep=""), width=900,height=550)
    cepMapping<-ggplot()+
      geom_polygon(data=space,aes(long,lat,group=group,fill=class))+
      geom_path(data=word.df, aes(x=long,y=lat,group=group),color="grey30")+
      theme_bw()+
      scale_fill_brewer(labels = intLabels)+
      labs(fill = "Intervales \n kmeans \n en % de surface \n mondiale", 
           title=paste("Proportion des surfaces du cépage ",nameIndex[i] ," \n par pays (2000)",sep=""))
    coord_equal()
    print(cepMapping)
    dev.off()
  }
 }
}

