# CDC Distance Generator 
# If you don't have this:
# install.packages("gmapsdistance")
library("gmapsdistance")
library("geosphere")
# Sample data: 3700 observations of a person in CDC (very small partition of a partition...)
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

# TODO: Solve - Google API returned an error: You have exceeded your daily request quota for this API.
# pair<-paste(d0$lon[1],"+",d0$lat[1],sep="") # sep="" removes the white space

# attach(d0)
# for (i in 1:(nrow(d0)-1)){
#   d0$origin[i]<-paste(d0$lon[i],"+",d0$lat[i],sep="")
#   d0$destination[i]<-paste(d0$lon[i+1],"+",d0$lat[i+1],sep="")
#   d0$dist[i]<-gmapsdistance(origin=d0$origin[i], destination=d0$destination[i], mode="walking")$Distance
# }

# Alternative: use geosphere package
for (i in 1:(nrow(d0)-1)){
  d0$dist[i]<-distHaversine(c(d0$lon[i],d0$lat[i]), c(d0$lon[i+1],d0$lat[i+1]))
  d0$t.delta[i]<-d0$time[i+1]-d0$time[i]
  d0$sum.dist[1]<-d0$dist[1]
  d0$sum.dist[i+1]<-d0$sum.dist[i]+d0$dist[i+1]
  d0$sum.t[1]<-d0$t.delta[1]
  d0$sum.t[i+1]<-d0$sum.t[i]+d0$t.delta[i+1]
  d0$vel[i] <-d0$sum.dist[i]/d0$sum.t[i]
}


#############################################
# replicate this method on another person
#############################################
# TODO: use std.in to read a file, and provide output from std.out
#############################################
d1_one<-read.csv(file="person_5479.csv",header=FALSE, sep=",")
d1<-d1_one[c(1,4:6)]
colnames(d1)<-c("db_key","time","lat","lon")
d1<-d1[order(d1$time),]     # or attach(d1); d1<-d1[order(time),]
for (i in 1:(nrow(d1)-1)){
  d1$dist[i]<-distHaversine(c(d1$lon[i],d1$lat[i]), c(d1$lon[i+1],d1$lat[i+1]))
  d1$t.delta[i]<-d1$time[i+1]-d1$time[i]
  d1$sum.dist[1]<-d1$dist[1]
  d1$sum.dist[i+1]<-d1$sum.dist[i]+d1$dist[i+1]
  d1$sum.t[1]<-d1$t.delta[1]
  d1$sum.t[i+1]<-d1$sum.t[i]+d1$t.delta[i+1]
  d1$vel[i] <-d1$sum.dist[i]/d1$sum.t[i]
}

plot(log(d0$sum.t), log(d0$vel), type="l", lty = 1, lwd=2, col="darkorange",
     xlab="Logged Cumulative time (s)", xlim=c(2,16), ylab="Logged Velocity (m/s)",
     ylim=c(-2,2))
lines(log(d1$sum.t), log(d1$vel), lty = 1, lwd=2, col="pink")
legend(9,2.07,c("Person 1 - M", "Person 2- F"),
       lty=c(1,1), lwd=c(2.5,2.5),col=c("darkorange","pink")) 

