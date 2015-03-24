##script qui va permettre de croiser les données scrapées sur le site http://www.si-vitifrance.com
## pour évalier la corelation qui peut exister entre les volumes en coop et indiv

library(rgdal)
library(plyr)
library(ggplot2)

rm(list=ls())
setwd("~/github/bazaRd/coop_viti/data/")

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

##les proportion de vendange transformé en cave particulière
df.aggregation$pct_parti_13<-df.aggregation$volumes_particulier_2013_hl/(df.aggregation$volumes_coop_2013_hl+df.aggregation$volumes_particulier_2013_hl)*100
df.aggregation$pct_parti_10<-df.aggregation$volumes_particulier_2010_hl/(df.aggregation$volumes_coop_2010_hl+df.aggregation$volumes_particulier_2010_hl)*100

##Les augmentation et réduction de volume entre 2010 et 2013
df.aggregation$variation_coop_1013<-df.aggregation$volumes_coop_2013_hl-df.aggregation$volumes_coop_2010_hl
df.aggregation$variation_particulier_1013<-df.aggregation$volumes_particulier_2013_hl-df.aggregation$volumes_particulier_2010_hl

##ce que représente chaque type de production par rapport a la production viticole nationnal
df.aggregation$volume_viticole_2013<-(df.aggregation$volumes_particulier_2013_hl+df.aggregation$volumes_coop_2013_hl)/(sum(df.aggregation$volumes_coop_2013_hl)+sum(df.aggregation$volumes_particulier_2013_hl))*100

smalldep<-subset(departements@data,select=c("CODE_DEPT", "NOM_DEPT"))
df.aggregation<-join(smalldep, df.aggregation,by="CODE_DEPT", type="right")

## teste de correlation ####
cor.test(df.aggregation$pct_coop_13, df.aggregation$pct_parti_13, method = "pearson")
model<-lm(df.aggregation$volumes_coop_2013_hl~df.aggregation$volumes_particulier_2013_hl)

plot(df.aggregation$volumes_coop_2013_hl, df.aggregation$volumes_particulier_2013_hl, 
     xlab="volume en coop", ylab= "volume en cave")
text(df.aggregation$volumes_coop_2013_hl, df.aggregation$volumes_particulier_2013_hl, 
     labels=df.aggregation$CODE_DEPT, pos=1)

##si on suprime les dep 16 17 33
sel<-df.aggregation$CODE_DEPT %in% c(16,17,33)
cor.test(df.aggregation[!sel,]$volumes_coop_2013_hl, df.aggregation[!sel,]$volumes_particulier_2013_hl, method = "pearson")
model<-lm(df.aggregation[!sel,]$volumes_coop_2013_hl~df.aggregation[!sel,]$volumes_particulier_2013_hl)
summary(model)

plot(df.aggregation$volumes_coop_2013_hl, df.aggregation$volumes_particulier_2013_hl, 
     xlab="volume en coopérative", ylab= "volume en cave individuelle",xlim=c(0, 1000000))
text(df.aggregation$volumes_coop_2013_hl, df.aggregation$volumes_particulier_2013_hl, 
     labels=df.aggregation$CODE_DEPT, pos=3)
abline(model)


reg_lin<-ggplot(data=df.aggregation, aes(x=volumes_coop_2013_hl, y=volumes_particulier_2013_hl))+
  geom_point()+
  geom_text(aes(label=tolower(NOM_DEPT)),hjust=0, vjust=1)+
  stat_smooth(data=df.aggregation[!sel,], aes(x=volumes_coop_2013_hl, y=volumes_particulier_2013_hl),
              method=lm)+
  labs(x="volumes traités en coopérative",y="volume traités en cave particulière")
reg_lin
ggsave("../img/regression.png",reg_lin,dpi = 120, height = 7, width = 15)

reg_lin_zoom<-ggplot(data=df.aggregation, aes(x=volumes_coop_2013_hl, y=volumes_particulier_2013_hl))+
  geom_point()+
  geom_text(aes(label=tolower(NOM_DEPT)),hjust=0, vjust=1)+
  stat_smooth(data=df.aggregation[!sel,], aes(x=volumes_coop_2013_hl, y=volumes_particulier_2013_hl),
              method=lm)+
  labs(x="volumes traités en coopérative",y="volume traités en cave particulière")+
  scale_x_continuous(limits = c(0, 1000000))+
  scale_y_continuous(limits = c(0, 1000000))
reg_lin_zoom
