# To load the 8G data, need 24G CPU...
srun --pty --time=1200 --mem-per-cpu=10240 --partition=largemem /bin/bash
module load R
R
> demog<-read.csv(file="./MDC/Data/RawData/demographics.csv",header=FALSE,sep=",")
> gps<-read.csv(file="./MDC/Data/RawData/gps.csv",header=FALSE,sep="\t")
