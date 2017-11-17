# To load the 8G data, need 24G CPU..., see --mem-per-cpu
srun --pty --time=1200 --mem-per-cpu=10240 --partition=largemem /bin/bash
module load R
R
# All CDC Files are tab-delimited...
###################################
# Load the Required Package 
###################################

#!/bin/bash
#SBATCH --job-name cdcpartition      # Set a name for your job. This is especially useful if you have multiple jobs queued.
#SBATCH --partition largemem     # Slurm partition to use
#SBATCH --ntasks 1          # Number of tasks to run. By default, one CPU core will be allocated per task
#SBATCH --time 0-20:00        # Wall time limit in D-HH:MM
#SBATCH --mem-per-cpu= 10240     # Memory limit for each tasks (in MB) (Note this is NOT enough for records.csv)
#SBATCH -o myscript_%j.out    # File to which STDOUT will be written
#SBATCH -e myscript_%j.err    # File to which STDERR will be written
#SBATCH --mail-type=ALL       # Type of email notification- BEGIN,END,FAIL,ALL
#SBATCH --mail-user=zdong@uw.edu # Email to which notifications will be sent

# Examples...
> library("dplyr")
> library("sp")
> library("ggplot2")

# Partition into small files, and filter out redundant information

> demog<-read.csv(file="./MDC/Data/RawData/demographics.csv",header=FALSE,sep="\t")
> gps<-read.csv(file="./MDC/Data/RawData/gps.csv",header=FALSE,sep="\t")
# Test Everything is right...
> head(gps, n=10)
# Rename the column...
> colnames(gps.s)<-c("db_key","time","long","lat","deltat")
> head(gps.s, n=10)

