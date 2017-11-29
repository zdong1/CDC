avgSpeed<-function(pid){
    df<-pid@data
    for(j in 2: nrow(df)){
    df$velo[1]<- 0
    df$velo[j]<- df$dists[j]/df$t.delta[j]
    }
    spd = NULL
    for (k in 1: 52){
      if (nrow(subset(df,(df$sum.t)/168>(k-1) & (df$sum.t)/168<k))==0){
        spd[k] = NA
      } else{
        spd[k] <-mean((subset(df, (df$sum.t)/168>(k-1) & (df$sum.t)/168<k))$velo, na.rm=TRUE) 
      }
    }
    spd
}
