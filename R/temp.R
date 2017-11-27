library("geosphere")
library(sp)
require(sp)
library(dplyr)
load("byperson.Rda")
setwd("~/Data/CDC")
# source("../../DistanceGen.R") - have the functions ready

# Paste the function here
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
    raw@data$vel[i] <-raw@data$sum.dists[i]/raw@data$sum.t[i]
  }
  raw
}



p <- lapply(s,genDist)
