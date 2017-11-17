# To load the 8G data, need 24G CPU..., see --mem-per-cpu
srun --pty --time=1200 --mem-per-cpu=10240 --partition=largemem /bin/bash
module load R
R
# All CDC Files are tab-delimited...
> demog<-read.csv(file="./MDC/Data/RawData/demographics.csv",header=FALSE,sep="\t")
> gps<-read.csv(file="./MDC/Data/RawData/gps.csv",header=FALSE,sep="\t")
# Test Everything is right...
> head(gps, n=10)
# Rename the column...
> colnames(gps.s)<-c("db_key","time","long","lat","deltat")
> head(gps.s, n=10)

