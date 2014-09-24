##Script pour préparer des tables rapport marius
library(rgdal)
library(plyr)
library(ggplot2)
library(xtable)

rm(list=ls())
setwd("~/github/bazaRd/coop_viti/data/")
source("../fonc_multiplot.R")

##lecture des données
departements<-readOGR(dsn = "../geofla",layer="departements_s")
coop13<-read.csv("volume_coop_commune13.csv",sep=",",header=T)

particulier13<-read.csv("volume_caves_particulieres_commune13.csv",sep=",",header=T)

#tableaux condensé
###Coop


volume_Coop13_dep<-ddply(coop13,.(code), 
                         summarise,
                         sum_volumes_hl = sum(Qté.tot.livrée.à.la.cave.coop..hl.))
##particulier
                 sum_volumes_hl = sum(Qté.tot.réc.par.les.caves.particulières..hl.))

volume_particulier13_dep<-ddply(particulier13,.(code), 
                                summarise,
                                sum_volumes_hl = sum(Qté.tot.réc.par.les.caves.particulières..hl.))


df.aggregation<-join(volume_Coop13_dep,volume_particulier13_dep,by="code",type="left")
colnames(df.aggregation)<-c("CODE_DEPT","volumes_coop_2013_hl","volumes_part_2013_hl")

df.aggregation[is.na(df.aggregation)] <- 0 #convertir les NA en zero

##Les proportion de la vendange qui sont transformer par la coop
df.aggregation$pct_coop_13<-df.aggregation$volumes_coop_2013_hl/(df.aggregation$volumes_coop_2013_hl+df.aggregation$volumes_part_2013_hl)*100

##les proportion de vendange transformé en cave particulière
df.aggregation$pct_parti_13<-df.aggregation$volumes_part_2013_hl/(df.aggregation$volumes_coop_2013_hl+df.aggregation$volumes_part_2013_hl)*100

##ce que représente chaque type de production par rapport a la production viticole nationnal
df.aggregation$volume_2013_dep_vs_nat<-(df.aggregation$volumes_part_2013_hl+df.aggregation$volumes_coop_2013_hl)/(sum(df.aggregation$volumes_coop_2013_hl)+sum(df.aggregation$volumes_part_2013_hl))*100

tps<-subset(departements@data, select = c("CODE_DEPT","NOM_DEPT"))

df.aggregation<-join(df.aggregation,tps,by="CODE_DEPT",type="left")
rm(tps)

df.table<-subset(df.aggregation,select = c("CODE_DEPT","NOM_DEPT","pct_coop_13","pct_parti_13","volume_2013_dep_vs_nat"))
colnames(df.table)<-c("code","nom","coopérative","particulier","vol_departemental")
print(xtable(df.table, caption = "table récapitulative des volumes produits par la 
             filière viticole par département", label="tab:recap_filiere")
      , include.rownames=FALSE)

##### Travail sur les IGP et non IGP ####

##Dans les coopératives
##sommes des vin rouge rosé en blan AOP
coop13$sumAOP<-rowSums(coop13[,4:5])
coop13$sumIGP<-rowSums(coop13[,6:7])
coop13$sumSans<-rowSums(coop13[,8:9])

volume_AOP_Coop13_dep<-ddply(coop13,.(code), 
                             summarise,
                             coop_AOP_volumes_hl = sum(sumAOP))

volume_IGP_Coop13_dep<-ddply(coop13,.(code), 
                         summarise,
                         coop_IGP_volumes_hl = sum(sumIGP))

volume_sans_Coop13_dep<-ddply(coop13,.(code), 
                             summarise,
                             coop_Sans_volumes_hl = sum(sumSans))
aggregAOPcoop<-join(volume_AOP_Coop13_dep, volume_IGP_Coop13_dep, by="code")
aggregAOPcoop<-join(aggregAOPcoop, volume_sans_Coop13_dep, by="code")

##Dans les caves particulières
particulier13$sumAOP<-rowSums(particulier13[,3:4])
particulier13$sumIGP<-rowSums(particulier13[,5:6])
particulier13$sumSans<-rowSums(particulier13[,7:8])

volume_AOP_particulier13_dep<-ddply(particulier13,.(code), 
                                    summarise,
                                    part_AOP_volumes_hl = sum(sumAOP))

volume_IGP_particulier13_dep<-ddply(particulier13,.(code), 
                                summarise,
                                part_IGP_volumes_hl = sum(sumIGP))

volume_nonIGP_particulier13_dep<-ddply(particulier13,.(code), 
                                summarise,
                                part_Sans_volumes_hl = sum(sumSans))
aggregAOPpart<-join(volume_AOP_particulier13_dep, volume_IGP_particulier13_dep, by="code")
aggregAOPpart<-join(aggregAOPpart, volume_nonIGP_particulier13_dep, by="code")

##joindre les 2 df
df.aggregeAOPIGP<-join(aggregAOPcoop, aggregAOPpart, by="code",type='right')
df.aggregeAOPIGP$code<-as.numeric(df.aggregeAOPIGP$code)
df.aggregeAOPIGP[is.na(df.aggregeAOPIGP)] <- 0 #convertir les NA en zero

##calcul des pct AOC etc
df.pct<-df.aggregeAOPIGP$code
for(i in 2:7){
  tps<-df.aggregeAOPIGP[i]/sum(colSums(df.aggregeAOPIGP[-1]))*100
  df.pct<-cbind(df.pct, tps)
}
colnames(df.pct)<-c("CODE_DEPT", "pct_coop_AOP","pct_coop_IGP","pct_coop_Sans",
                    "pct_part_AOP","pct_part_IGP","pct_part_Sans")

tps<-subset(departements@data, select = c("CODE_DEPT","NOM_DEPT"))

df.pct<-join(df.pct,tps,by="CODE_DEPT",type="left")
rm(tps)

df.pct_coop<-subset(df.pct,select = c("CODE_DEPT","NOM_DEPT","pct_coop_AOP","pct_coop_IGP","pct_coop_Sans"))
colnames(df.pct_coop)<-c("Code","Nom","AOP","IGP","Sans")

df.pct_part<-subset(df.pct,select = c("CODE_DEPT","NOM_DEPT","pct_part_AOP","pct_part_IGP","pct_part_Sans"))
colnames(df.pct_part)<-c("Code","Nom","AOP","IGP","Sans")

print(xtable(df.pct_coop, caption = "table récapitulative des proportions (%) de volumes produits
             en coopérative dans chaque catégorie de produit par rapport à la production nationale", label="tab:recap_coop_AOP_IGP_SANS")
      , include.rownames=FALSE)
print(xtable(df.pct_part, caption = "table récapitulative des proportions (%) de volumes produits
             en cave particulière dans chaque catégorie de produit par rapport à la production nationale", label="tab:recap_part_AOP_IGP_SANS")
      , include.rownames=FALSE)

##jointure sur les données spatial
departements@data<-join(departements@data,df.aggregation,by="CODE_DEPT",type = "left")

##ggplot2##
##transformation d'un spData.frame en dataFrame
departements@data$id<-rownames(departements@data)
dep.pts<-fortify(departements,region="id")
dep.df<-join(dep.pts, departements@data, by="id")
