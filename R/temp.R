library("geosphere")
library(sp)
require(sp)
library(dplyr)
load("ten.Rda")

# source("../../DistanceGen.R") - have the functions ready

genDist<-function(raw){
  raw<-raw[order(raw$time),]
  coordinates(raw)<- ~ long+lat
  proj4string(raw) <- CRS("+proj=longlat +datum=WGS84")
  class(raw@data$time)=c('POSIXt','POSIXct')
  for (i in 2:(nrow(raw))){
    sort(raw@data$time)
    raw@data$dists[1]<-0
    raw@data$dists[i]<-spDistsN1(pts= raw[i-1,], pt= raw[i,], longlat=TRUE)
    raw@data$t.delta[1]<-0
    raw@data$t.delta[i]<-difftime(raw@data$time[i],raw@data$time[i-1],units="hours")
    raw@data$sum.dists[1]<-0
    raw@data$sum.dists[i]<- raw@data$sum.dists[i-1]+raw@data$dists[i]
    raw@data$sum.t[1]<-0
    raw@data$sum.t[i]<- raw@data$sum.t[i-1]+raw@data$t.delta[i]
    raw@data$vel[1] <- NA
    raw@data$vel[i] <-raw@data$dists[i]/raw@data$t.delta[i]
    raw@data$s.vel[1] <- NA
    raw@data$s.vel[i] <-raw@data$sum.dists[i]/raw@data$sum.t[i]
  }
  raw
}

p.6218<-genDist(person6218)

p.5925<-genDist(person5925)
p.5927<-genDist(person5927)
p.5928<-genDist(person5928)
p.5938<-genDist(person5938)
p.5939<-genDist(person5939)
p.5940<-genDist(person5940)


plot(p.6075@data$sum.t, p.6075@data$velo, 
     col="coral", lwd = 0.02, pch=19, type="l",xlab="week",ylab="Velocity (km/h)",
     ylim=c(0,5),xlim=c(0,50), main= "cumulative")


spd = 0

avgSpeed <- function(person){
  for (j in 2:nrow(p.6075@data)){
    p.6075@data$velo[1] <- 0
    p.6075@data$velo[j] <- p.6075@data$dists[j]/p.6075@data$t.delta[j]
    p.6075@data$sum.t[j]<- p.6075@data$sum.t[j]/168
    df<-p.6075@data
    for (k in 1: 52){
      spd[k] <-(subset(df, df$sum.dists>(k-1) & df$sum.t<k))$velo
    }
  }
}



lines((p.5968@data$sum.t)/168, p.5968@data$vel, col="cyan", lwd = 2)
lines((p.6020@data$sum.t)/168, p.6020@data$vel, col="green", lwd = 2)
lines((p.6058@data$sum.t)/168, p.6058@data$vel, col="gray", lwd = 2)
lines((p.6066@data$sum.t)/168, p.6066@data$vel, col="red", lwd = 2)
lines((p.6075@data$sum.t)/168, p.6075@data$vel, col="darkolivegreen", lwd = 2)
lines((p.6106@data$sum.t)/168, p.6106@data$vel, col="darkorange", lwd = 2)
lines((p.6176@data$sum.t)/168, p.6176@data$vel, col="darkorchid", lwd = 2)
lines((p.6180@data$sum.t)/168, p.6180@data$vel, col="burlywood", lwd = 2)
lines((p.6194@data$sum.t)/168, p.6194@data$vel, col="coral", lwd = 2)
par(xpd=FALSE)
legend(20, -3 ,c("M,6", "F,6", "F,2","M,3","M,2","F,4","F,3","M,4",
              "F,2","M,2"), 
       col = c("pink","cyan","green","gray","red","darkolivegreen",
               "darkorange","darkorchid","burlywood","coral"), 
       lwd = c(2,2,2,2,2,2,2,2,2,2))
