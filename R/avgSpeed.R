avgSpeed <- function(pid){
    pid@data$velo[1] <- 0
    pid@data$velo[j] <- pid@data$dists[j]/pid@data$t.delta[j]
    pid@data$sum.t[j]<- pid@data$sum.t[j]/168
    df<-pid@data
    for (k in 1: 52){
      if (nrow(subset(df,df$sum.t>(k-1) & df$sum.t<k))==0){
        spd[k] = NA
      } else{
        spd[k] <-(subset(df, df$sum.t>(k-1) & df$sum.t<k))$velo 
      }
    }
}