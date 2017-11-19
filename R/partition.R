###################################
# Load the Required Package 
# Actually the packages below won't be used lol...
###################################
library("dplyr")
library("sp")
library("ggplot2")

# Partition into small files, and filter out redundant information
# Note all of the CDC data are tab-delimited

demog<-read.csv(file="./MDC/Data/RawData/demographics.csv",header=FALSE,sep="\t")
gps<-read.csv(file="./MDC/Data/RawData/gps.csv",header=FALSE,sep="\t")
# Test Everything is right...
head(gps, n=10)
# Rename the column...
colnames(gps.s)<-c("db_key","time","long","lat","deltat")
head(gps.s, n=10)
# Save this into RData, the size shrinked to 1G --> 150M 
save(gps.s,file="./MDCRes/gps.s.Rda")
quit()
