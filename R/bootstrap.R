library(boot)
library(data.table)

# This function generates a new week of a person
newweek<-function(daf){
  # Give each weekname an assigned number, use the shrink version of dataset
  daf[which(daf$wkdy=="Monday"),]$wkdy = 1
  daf[which(daf$wkdy=="Tuesday"),]$wkdy = 2
  daf[which(daf$wkdy=="Wednesday"),]$wkdy = 3
  daf[which(daf$wkdy=="Thursday"),]$wkdy = 4
  daf[which(daf$wkdy=="Friday"),]$wkdy = 5
  daf[which(daf$wkdy=="Saturday"),]$wkdy = 6
  daf[which(daf$wkdy=="Sunday"),]$wkdy = 7
  temp_df<-daf[c(1:3,6:9,12:13)]
  i = 1
  day.w<-NULL
  while (i < 8){
    day.p <- temp_df[which(temp_df$wkdy==i),] # permutate a Mon/Tues/Weds...
    day<-day.p[sample(nrow(day.p),1,replace = TRUE),]
    j = 1
    # Given a day, sample with replacement of a particular move until 24 hours are filled
    while (j > 0){
      day.s<-day.p[sample(nrow(day.p),1,replace = TRUE),]
      day<- rbind(day.s,day)
      if(sum(day$t.delta,na.rm=TRUE) < 24){
        j = j+1
      } else {
        j = 0
      }
    }
    if (i > 1){
      day.w<-rbind(day.w,day)
    } else{
      day.w<-day
    }
    i = i+1
  }
  day.w
}


weeks <- replicate(20,newweek(df.new15),simplify=FALSE)
weeks <- rbindlist(weeks)


# Calculate the Cumulative Speed Over Time
bsCalc <-function(df){
  df$t.delta[1] = 0
  df$dists[1] = 0
  df$t.delta[1] <- 0
  df$sum.dists <- 0
  df$sum.t <- 0
  df$s.vel <- 0
  for (i in 2:nrow(df)){
    df$sum.dists[i]<- df$sum.dists[i-1]+df$dists[i]
    df$sum.t[i]<- df$sum.t[i-1]+df$t.delta[i]
  }
  df$s.vel<-df$sum.dists/df$sum.t
  df
}
df.bt105<-bsCalc(weeks)

# Get the Plot
plot(df.bt105$sum.t,df.bt105$s.vel, type="l", ylim=c(0,10))
# TODO: Replicate this to hours

