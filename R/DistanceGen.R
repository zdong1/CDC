##########################################################################################################
# CDC Distance Generator 
# Zhihang Dong
# Version 1.4
# 11/26/2017
##########################################################################################################
# If you don't have this:
# install.packages("gmapsdistance")
library("gmapsdistance")
library("geosphere")
library(sp)
require(sp)
# Sample data: 3600 observations of a person in CDC (very small partition of a partition...)
# load("d1.Rdata") -- See Github repository
# If the use of gmaps function get solved
# get.api.key()
# trial1<-gmapsdistance(origin="46.5255+6.65218", destination = "46.5240+6.61600",mode="walking")
# trial1
# trial1$Distance
# trim the dataset
##########################################################################################################
# TODO: Solve - Google API returned an error: You have exceeded your daily request quota for this API.
# pair<-paste(d0$lon[1],"+",d0$lat[1],sep="") # sep="" removes the white space
# attach(d0)
# for (i in 1:(nrow(d0)-1)){
#   d0$origin[i]<-paste(d0$lon[i],"+",d0$lat[i],sep="")
#   d0$destination[i]<-paste(d0$lon[i+1],"+",d0$lat[i+1],sep="")
#   d0$dist[i]<-gmapsdistance(origin=d0$origin[i], destination=d0$destination[i], mode="walking")$Distance
# }
###########################################################################################################
# Crow flies Distance Version - Less Ideal, but mathematically fine...
###########################################################################################################
# Prepare the Data
d1_one<-read.csv(file="person_5479.csv",header=FALSE, sep=",")
d1<-d1_one[c(1,4:6)]
colnames(d1)<-c("db_key","time","lat","lon")
# sort the data set by time
d1<-d1[order(d1$time),]     # or attach(d1); d1<-d1[order(time),]


## Main Functions
genDist<-function(raw){
  coordinates(raw)<- ~ lon+lat
  proj4string(raw) <- CRS("+proj=longlat +datum=WGS84")
  class(raw@data$time)=c('POSIXt','POSIXct')
  for (i in 2:(nrow(raw))){
    raw@data$dists[1]<-0
    raw@data$dists[i]<-spDistsN1(pts= raw[i-1,], pt= raw[i,], longlat=TRUE)
    raw@data$t.delta[1]<-0
    raw@data$t.delta[i]<-difftime(raw@data$time[i],raw@data$time[i-1],units="hours")
    raw@data$sum.dists[1]<-0
    raw@data$sum.dists[i]<- raw@data$sum.dists[i-1]+raw@data$dists[i]
    raw@data$sum.t[1]<-0
    raw@data$sum.t[i]<- raw@data$sum.t[i-1]+raw@data$t.delta[i]
    raw@data$vel[i] <-raw@data$sum.dists[i]/raw@data$sum.t[i]
  }
  raw
}
# Generate this New Dataset
d1.new<-genDist(raw=d1)
#############################################
# replicate this method on another person
#############################################
# TODO: use a function to read files
# DONE 
#############################################
# Examine the Dataset
summary(d1.new@data$sum.dists)

