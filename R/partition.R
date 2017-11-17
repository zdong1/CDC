# All CDC Files are tab-delimited...
###################################
# Load the Required Package 
###################################
library("dplyr")
library("sp")
library("ggplot2")

# Partition into small files, and filter out redundant information

demog<-read.csv(file="./MDC/Data/RawData/demographics.csv",header=FALSE,sep="\t")
gps<-read.csv(file="./MDC/Data/RawData/gps.csv",header=FALSE,sep="\t")
# Test Everything is right...
head(gps, n=10)
# Rename the column...
colnames(gps.s)<-c("db_key","time","long","lat","deltat")
head(gps.s, n=10)
# Save this into RData, the size shrinked to 1G --> 150M 
save(gps.s,file="./gps.s.Rda")
quit()
