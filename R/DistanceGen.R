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
coordinates(d1)<- ~ lon+lat
proj4string(d1) <- CRS("+proj=longlat +datum=WGS84")
class(d1@data$time)=c('POSIXt','POSIXct')


## HAVE BUGSSS!!!!!!!!!!!
input<-d1
genDist<-function(input){
  for (i in 1:(nrow(input)-1)){
    input@data$dists[nrow(input)]<-0
    input@data$dists[i]<-spDistsN1(pts= input[i,], pt= input[i+1,], longlat=TRUE)
    input@data$t.delta[nrow(input)]<-0
    input@data$t.delta[i]<-difftime(input@data$time[i+1],input@data$time[i],units="hours")
    input@data$sum.dists[1]<-input@data$dists[1]
    input@data$sum.dists[i+1]<-input@data$sum.dists[i]+input@data$dists[i+1]
    input@data$sum.t[1]<-input@data$t.delta[1]
    input@data$sum.t[i+1]<-input@data$sum.t[i]+ input@data$t.delta[i+1]
    input@data$vel[i] <-input@data$sum.dists[i]/input@data$sum.t[i]
  }
}
d1.new<-genDist(input=d1)
#############################################
# replicate this method on another person
#############################################
# TODO: use std.in to read a file, and provide output from std.out
#############################################


plot(d1@data, d1$vel, type="l", lty = 1, lwd=2, col="darkorange",
     xlab="Cumulative Time (h)", ylab="Velocity (m/h)")
lines(log(d1$sum.t), log(d1$vel), lty = 1, lwd=2, col="pink")
legend(9,2.07,c("Person 1 - M", "Person 2- F"),
       lty=c(1,1), lwd=c(2.5,2.5),col=c("darkorange","pink")) 

###### Verify ######
for (i in 1:(nrow(d1)-1)){
  d1@data$dists[nrow(d1)]<-0
  d1@data$dists[i]<-spDistsN1(pts= d1[i,], pt= d1[i+1,], longlat=TRUE)
  d1@data$t.delta[nrow(d1)]<-0
  d1@data$t.delta[i]<-difftime(d1@data$time[i+1],d1@data$time[i],units="hours")
  d1@data$sum.dists[i+1]<-d1@data$sum.dists[i]+d1@data$dists[i+1]
}

summary(d1@data$sum.dists)

d1@data$sum.dists[1]<-d1@data$dists[1]
d1@data$sum.t[1]<-d1@data$t.delta[1]
d1@data$sum.t[i+1]<-d1@data$sum.t[i]+d1@data$t.delta[i+1]
d1@data$vel[i] <-d1@data$sum.dists[i]/d1@data$sum.t[i]