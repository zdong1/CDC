library(Rmpi)
library(snow)

# Initialize SNOW using MPI communication. The first line will get the number of
# MPI processes the scheduler assigned to us. Everything else is standard SNOW
np <- mpi.universe.size() - 1
cluster <- makeMPIcluster(np)

load("final.RData")

shrink <- function(df){
  class(df$time)=c('POSIXt','POSIXct')
  for (j in 1: nrow(df)){
    if(sum(df$week==df$week[j]) > 100){
      df$valid[j] = 1
    } else{
      df$valid[j] = 0
    }
  }
  df = df[!df$valid == 0,]
  df$vel[1] = 0
  df$t.delta[1] = 0
  df$dists[1] = 0
  df$t.delta[1]<-0
  for (i in 2:nrow(df)){
    df$t.delta[i]<-difftime(df$time[i],df$time[i-1],units="hours")
  }
  df$t.delta[df$t.delta>48] <- 0
  df$dists[df$t.delta>48] <- 0
  for (i in 2:nrow(df)){
    df$sum.dists[i]<- df$sum.dists[i-1]+df$dists[i]
    df$sum.t[i]<- df$sum.t[i-1]+df$t.delta[i]
  }
  df$vel<- df$dists/df$t.delta
  df$s.vel<-df$sum.dists/df$sum.t
  df
}

df.new67<-shrink(df67)
df.new68<-shrink(df68)
df.new69<-shrink(df69)
df.new70<-shrink(df70)
df.new71<-shrink(df71)

save(df.new67,df.new68,df.new69,df.new70,df.new71,
     file = "temp.RData")


