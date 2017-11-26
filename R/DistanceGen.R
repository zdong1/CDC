# CDC Distance Generator 
# If you don't have this:
# install.packages("gmapsdistance")
library("gmapsdistance")
library("geosphere")
library(sp)
require(sp)




# Sample data: 10900 observations of a person in CDC (very small partition of a partition...)
# load("d0_one.Rdata") -- See Github repository

get.api.key()
d0_one<-read.csv(file="person_5578.csv",header=FALSE, sep=",")
# trial1<-gmapsdistance(origin="46.5255+6.65218", destination = "46.5240+6.61600",mode="walking")
# trial1
# trial1$Distance
# trim the dataset
d0<-d0_one[c(1,4:6)]
colnames(d0)<-c("db_key","time","lat","lon")
# sort the data set by time
d0<-d0[order(d0$time),]     # or attach(d0); d0<-do[order(time)]

coordinates(d0)<- ~ lon+lat
proj4string(d0) <- CRS("+proj=longlat +datum=WGS84")

# TODO: Solve - Google API returned an error: You have exceeded your daily request quota for this API.
# pair<-paste(d0$lon[1],"+",d0$lat[1],sep="") # sep="" removes the white space

# attach(d0)
# for (i in 1:(nrow(d0)-1)){
#   d0$origin[i]<-paste(d0$lon[i],"+",d0$lat[i],sep="")
#   d0$destination[i]<-paste(d0$lon[i+1],"+",d0$lat[i+1],sep="")
#   d0$dist[i]<-gmapsdistance(origin=d0$origin[i], destination=d0$destination[i], mode="walking")$Distance
# }



d1_one<-read.csv(file="person_5479.csv",header=FALSE, sep=",")
d1<-d1_one[c(1,4:6)]
colnames(d1)<-c("db_key","time","lat","lon")
d1<-d1[order(d1$time),]     # or attach(d1); d1<-d1[order(time),]


## No more bugs
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



d1.new<-genDist(raw=d1)
#############################################
# replicate this method on another person
#############################################
# TODO: use a function to read files
# DONE 
#############################################

summary(d1.new@data$sum.dists)

