##script pour parser les pages du site http://www.si-vitifrance.com/ pour les coopératives
##ATTENTION 
##On peut modifier la date de l'étude diachronique en modifiant dans l'URL 
##http://www.si-vitifrance.com/docs/cvi/cvi10/ par http://www.si-vitifrance.com/docs/cvi/cvi13/ pour passer de 2010 à 2013
##attention à la phase d'enregistrement



library(rgdal) ##manipumation de données spatial avec gdal
library(XML)
library(RCurl)
library(stringr) ##manipulation des chaines de charactères


rm(list=ls())
setwd("~/github/bazaRd/coop_viti/")

#####################################################################
## ICI on peut definir la date entre 07 et 13
annee<-10
#####################################################################
communes<-readOGR(dsn = "./geofla/",layer="commune_s")
##c'est le champs code_dep qu'il faut utiliser pour scrapper les url
nam_col<-t(read.csv("name_col.csv",sep = ",",header = F))
nam_col<-nam_col[1,]
nam_col<-nam_col[-1]

code_insee<-as.character(unique(communes@data$CODE_DEPT))

##construction des URL

for(h in 1: length(code_insee)){
  doc<-NULL
  url<-paste("http://www.si-vitifrance.com/docs/cvi/cvi",annee,"/cartes_inter/c_vin01_coop_com",code_insee[h],"/embfiles/th0.xml",sep="")
  verif<-sapply(url, url.exists)
  if (verif == TRUE){
    doc = htmlTreeParse(url, useInternalNodes = T)
    nam<-paste("dep",code_insee[h],sep="")
    ##on va recontruire les table
    table<-NULL
    for(i in 0:8){
      myNode<-paste("//f",i,sep="")
      tps <- xpathSApply(doc, myNode, fun=xmlValue)
      table<-cbind(table, tps)
    }
    colnames(table)<-nam_col
    table<-as.data.frame(table)
    table<-table[-1,] ##pour conserver sous forme de tableau les table qui n'ont qu'une ligne
    code<-rep(code_insee[h],length(table[,1]))
    table<-cbind(table,code)
#     table[,1]<-str_sub(table[,1],start=5)
    
    assign(nam,table)
  }
 
}

ordre.var<-grep("dep",ls())
list.var<-ls()
df.coop<-NULL
for(o in ordre.var){
  var.tps<-get(list.var[o])
  df.coop<-rbind(df.coop,var.tps)
}

#df.coop est donc le data.frmae exploitable
write.csv(df.coop,paste("volume_coop_commune",annee,".csv",sep=""),row.names=F)
