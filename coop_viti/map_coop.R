##script qui va permettre de croiser les données scrapées sur le site http://www.si-vitifrance.com
##avec les deux autres script scrap_coop et scrap_caves_particulieres à l'échelle communales
##pour en faire des cartes

library(rgdal)
library(plyr)
library(ggplot2)

rm(list=ls())
setwd("~/github/bazaRd/coop_viti/data/")
source("../fonc_multiplot.R")

##lecture des données
departements<-readOGR(dsn = "../geofla",layer="departements_s")
coop10<-read.csv("volume_coop_commune10.csv",sep=",",header=T)
coop13<-read.csv("volume_coop_commune13.csv",sep=",",header=T)

particulier10<-read.csv("volume_caves_particulieres_commune10.csv",sep=",",header=T)
particulier13<-read.csv("volume_caves_particulieres_commune13.csv",sep=",",header=T)

#tableaux condensé
###Coop
volume_Coop10_dep<-ddply(coop10,.(code), 
                     summarise,
                     sum_volumes_hl = sum(Qté.tot.livrée.à.la.cave.coop..hl.))

volume_Coop13_dep<-ddply(coop13,.(code), 
                         summarise,
                         sum_volumes_hl = sum(Qté.tot.livrée.à.la.cave.coop..hl.))
##particulier
volume_particulier10_dep<-ddply(particulier10,.(code), 
                       summarise,
                       sum_volumes_hl = sum(Qté.tot.réc.par.les.caves.particulières..hl.))

volume_particulier13_dep<-ddply(particulier13,.(code), 
                                summarise,
                                sum_volumes_hl = sum(Qté.tot.réc.par.les.caves.particulières..hl.))

df.aggregation.tps1<-join(volume_Coop10_dep,volume_Coop13_dep,by="code",type = "right")
colnames(df.aggregation.tps1)<-c("code","volumes_coop_2010_hl","volumes_coop_2013_hl")
df.aggregation.tps2<-join(volume_particulier10_dep,volume_particulier13_dep,by='code',type='right')
colnames(df.aggregation.tps2)<-c("code","volumes_particulier_2010_hl","volumes_particulier_2013_hl")
df.aggregation<-join(df.aggregation.tps1,df.aggregation.tps2,by="code",type="right")
colnames(df.aggregation)<-c("CODE_DEPT","volumes_coop_2010_hl","volumes_coop_2013_hl","volumes_particulier_2010_hl","volumes_particulier_2013_hl")

df.aggregation[is.na(df.aggregation)] <- 0 #convertir les NA en zero
##Les proportion de la vendange qui sont transformer par la coop
df.aggregation$pct_coop_13<-df.aggregation$volumes_coop_2013_hl/(df.aggregation$volumes_coop_2013_hl+df.aggregation$volumes_particulier_2013_hl)*100
df.aggregation$pct_coop_10<-df.aggregation$volumes_coop_2010_hl/(df.aggregation$volumes_coop_2010_hl+df.aggregation$volumes_particulier_2010_hl)*100

##Les augmentation et réduction de volume entre 2010 et 2013
df.aggregation$variation_coop_1013<-df.aggregation$volumes_coop_2013_hl-df.aggregation$volumes_coop_2010_hl
df.aggregation$variation_particulier_1013<-df.aggregation$volumes_particulier_2013_hl-df.aggregation$volumes_particulier_2010_hl

departements@data<-join(departements@data,df.aggregation,by="CODE_DEPT",type = "left")

##ggplot2##
##transformation d'un spData.frame en dataFrame
departements@data$id<-rownames(departements@data)
dep.pts<-fortify(departements,region="id")
dep.df<-join(dep.pts, departements@data, by="id")



map_coop_2013<-ggplot(data=dep.df,aes(x=long,y=lat,group=group,fill=pct_coop_13))+
  geom_polygon()+
  scale_fill_gradient(high="red",low="#CECECE",name="% du volume \n produit")+
  labs(title="Proportion de la production viticole (en hl)\n produite en coopértive en 2013")+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  annotate("text", label = "Sources: IGN geoflat & FranceAgriMer", x = 370000, y = 6050000, size = 5)+
  annotate("text", label = "Réalisation : E. Delay", x = 240000, y = 6010000, size = 5)+
  coord_equal()
print(map_coop_2013)

map_coop_2010<-ggplot(data=dep.df,aes(x=long,y=lat,group=group,fill=pct_coop_10))+
  geom_polygon()+
  scale_fill_gradient(high="red",low="#CECECE",name="% du volume \n produit")+
  labs(title="Proportion de la production viticole (en hl)\n produite en coopértive en 2010")+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  annotate("text", label = "Sources: IGN geoflat & FranceAgriMer", x = 370000, y = 6050000, size = 5)+
  annotate("text", label = "Réalisation : E. Delay", x = 240000, y = 6010000, size = 5)+
  coord_equal()
print(map_coop_2010)

ggsave("../img/pct_coop_departements2013.png",map_coop_2013,width = 8,height = 7,dpi = 150)
ggsave("../img/pct_coop_departements2010.png",map_coop_2010,width = 8,height = 7,dpi = 150)

png("../img/ggplot_coop_2010_203.png", height = 700, width = 1400)
  multiplot(map_coop_2010, map_coop_2013, cols=2)
dev.off()

##map de la variation entre 2010 et 2013
map_variation_coop<-ggplot(data=dep.df,aes(x=long,y=lat,group=group,fill=variation_coop_1013))+
  geom_polygon()+
  scale_fill_gradient(name="Volume produit \nen Hl")+
  labs(title="Évolution des volumes produits \nen coopérative entre 2010 et 2013")+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  annotate("text", label = "Sources: IGN geoflat & FranceAgriMer", x = 370000, y = 6050000, size = 5)+
  annotate("text", label = "Réalisation : E. Delay", x = 240000, y = 6010000, size = 5)+
  coord_equal()
print(map_variation_coop)

map_variation_particulier<-ggplot(data=dep.df,aes(x=long,y=lat,group=group,fill=variation_particulier_1013))+
  geom_polygon()+
  scale_fill_gradient(name="Volume produit \nen Hl")+
  labs(title="Évolution des volumes produits \nen caves particulières entre 2010 et 2013")+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  annotate("text", label = "Sources: IGN geoflat & FranceAgriMer", x = 370000, y = 6050000, size = 5)+
  annotate("text", label = "Réalisation : E. Delay", x = 240000, y = 6010000, size = 5)+
  coord_equal()
print(map_variation_particulier)

ggsave("../img/varaition_coop_2010_2013.png",map_variation_coop,width = 8,height = 7,dpi = 150)
ggsave("../img/varaition_c_particulieres_2010_2013.png",map_variation_particulier,width = 8,height = 7,dpi = 150)

png("../img/ggplot_variation_2010_203.png", height = 700, width = 1400)
multiplot(map_variation_coop, map_variation_particulier, cols=2)
dev.off()
