## Et si je jouais avec Google flu trend
## Source : http://www.google.org/flutrends/about/
## creer un gif avec  : 
##                      convert -delay 10 -loop 0 *.png animaion.gif

library(rgdal)
library(plyr)
library(RColorBrewer)
library(classInt)
library(maptools)

rm(list = ls())

setwd("~/")

data.df <- read.table("https://www.google.org/flutrends/about/data/flu/data.txt", skip = 11, header = TRUE, sep = ",")
world.shp <- readOGR(dsn = "./github/bazaRd/adelaide_wine/data/world/", layer = "ne_110m_admin_0_countries")
world.shp@data$name <- as.character(world.shp@data$name)

for(i in 1:length(data.df[,1])){
  tps.d <- data.df[i,]
  
  pays.v <- names(tps.d)
  pays.v <- pays.v[-1]
  
  date.i <- as.Date(as.character(tps.d[1,1]), format = "%Y-%m-%d")
  flu <- t(tps.d[,-1])
  row.names(flu) <- NULL
  
  
  wformat <- as.data.frame(cbind(pays.v, flu))
  colnames(wformat) <- c("name",paste(date.i))
  wformat[,2] <- as.numeric(as.character(wformat[,2]))
  
  world.shp@data <- join(world.shp@data, wformat, by="name")
  
  
  colors <- brewer.pal(5, "YlOrRd") #set breaks for the 9 colors
  brks<-classIntervals(world.shp@data[,length(world.shp@data)], n=5, style="quantile")
  #brks<- brks$brks #plot the map
  world.shp@data$colors <- findColours(brks, pal = colors)
  png(paste("github/bazaRd/flu-ggTrend/img/",date.i,".png", sep = ""), width = 580, height = 480, units = "px")
    plot(world.shp, col=world.shp@data$colors, axes=T, main = paste(date.i))
    
    #add a legend
    brks<- brks$brks #plot the map
    legend(x=-150, y=-50, legend=leglabs(round(brks)), fill=colors, bty="n",x.intersp = .5, y.intersp = .5)
  dev.off()
}
