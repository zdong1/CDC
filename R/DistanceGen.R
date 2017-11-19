# CDC Distance Generator 
# If you don't have this:
# install.packages("gmapsdistance")
library("gmapsdistance")
library("geosphere")
# Sample data: 3700 observations of a person in CDC (very small partition of a partition...)
# load("d0_one.Rdata") -- See Github repository

get.api.key()

trial1<-gmapsdistance(origin="46.5255+6.65218", destination = "46.5240+6.61600",mode="walking")
trial1
trial1$Distance
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
  d0$sum.t[i+1]<-d0$sum.t[i]+d0$t.delta[i]
}

plot(d0$sum.t,d0$sum.dist,cex=0.4, pch=19, col="pink",
     xlab="Cumulative time",ylab="Cumulative Distance")
lo <- loess(d0$sum.t~d0$sum.dist)
lines(lo, col='blue', lwd=1) # this looks weird...
