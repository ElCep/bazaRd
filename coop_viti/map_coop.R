##script qui va permettre de croiser les données scrapées sur le site http://www.si-vitifrance.com
##avec les deux autres script scrap_coop et scrap_caves_particulieres à l'échelle communales
##pour en faire des cartes

library(rgdal)
library(plyr)
library(ggplot2)

rm(list=ls())
setwd("~/Téléchargements/coop_viti/")

##lecture des données
departements<-readOGR(dsn = "/home/delaye/Téléchargements/coop_viti/geofla",layer="departements_s")
coop<-read.csv("volume_coop_commune.csv",sep=",",header=T)
particulier<-read.csv("volume_caves_particulieres_commune.csv",sep=",",header=T)


#tableaux condensé
volume_Coop_dep<-ddply(coop,.(code), 
                     summarise,
                     sum_volumes_hl = sum(Qté.tot.livrée.à.la.cave.coop..hl.))

volume_particulier_dep<-ddply(particulier,.(code), 
                       summarise,
                       sum_volumes_hl = sum(Qté.tot.réc.par.les.caves.particulières..hl.))

df.aggregation<-join(volume_Coop_dep,volume_particulier_dep,by="code",type = "right")
colnames(df.aggregation)<-c("CODE_DEPT","volumes_coop_hl","volumes_particuliers_hl")
df.aggregation[is.na(df.aggregation)] <- 0 #convertir les NA en zero
df.aggregation$pct_coop_dep<-df.aggregation$volumes_coop_hl/(df.aggregation$volumes_coop_hl+df.aggregation$volumes_particuliers_hl)*100

departements@data<-join(departements@data,df.aggregation,by="CODE_DEPT",type = "left")

##ggplot2##
##transformation d'un spData.frame en dataFrame
departements@data$id<-rownames(departements@data)
dep.pts<-fortify(departements,region="id")
dep.df<-join(dep.pts, departements@data, by="id")



map_coop_dep<-ggplot(data=dep.df,aes(x=long,y=lat,group=group,fill=pct_coop_dep))+
  geom_polygon()+
  scale_fill_gradient(high="red",low="#CECECE",name="% du volume \n produit")+
  labs(title="Proportion de la production viticole (en hl)\n produite en coopértive en 2013")+
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  coord_equal()
print(map_coop_dep)

ggsave("pct_coop_departements.png",map_coop_dep,width = 8,height = 7,dpi = 150)
