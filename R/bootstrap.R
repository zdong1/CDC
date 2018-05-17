# ===========================================================================
# Bootstrapping New Weeks
# @Author: Zhihang Dong
# Part 1: Load necessary packages
# ===========================================================================
library(boot)
library(data.table)
library(ggplot2)
library(ggmap)
# ============================================================================
# @Func[numeric_weekday]: turn string weekday (e.g. Monday) to numeric(e.g. 1)
# @Func[newweek]: bootstrap and generate a newweek (you need to replicate it)
# @Func[bsCalc]: recalculate a bootstrapped week
# ============================================================================

numeric_weekday<-function(daf){
  # @throw: many small data has 0 for a particular DOW, give them a skip when so. 
  if (nrow(daf[which(daf$wkdy=="Monday"),])>0){
    daf[which(daf$wkdy=="Monday"),]$wkdy = 1
  }
  if (nrow(daf[which(daf$wkdy=="Tuesday"),])>0){
    daf[which(daf$wkdy=="Tuesday"),]$wkdy = 2 
  }
  if (nrow(daf[which(daf$wkdy=="Wednesday"),])>0){
    daf[which(daf$wkdy=="Wednesday"),]$wkdy = 3
  }
  if (nrow(daf[which(daf$wkdy=="Thursday"),])>0){
    daf[which(daf$wkdy=="Thursday"),]$wkdy = 4
  }
  if (nrow(daf[which(daf$wkdy=="Friday"),])>0){
    daf[which(daf$wkdy=="Friday"),]$wkdy = 5
  }
  if (nrow(daf[which(daf$wkdy=="Saturday"),])>0){
    daf[which(daf$wkdy=="Saturday"),]$wkdy = 6
  }
  if (nrow(daf[which(daf$wkdy=="Sunday"),])>0){
    daf[which(daf$wkdy=="Sunday"),]$wkdy = 7 
  }
  daf
}

# mini is the minimum gap time you want to set up for your bootstrap simul.
# maxi is the maximum gap time. Raise mini for a faster algorithm, decrease maxi for
# higher accuracy
newweek<-function(temp_df){
  temp_df<-temp_df[which(temp_df$t.delta>0.02),]
  temp_df<-temp_df[which(temp_df$t.delta<3),]
  # Give each weekname an assigned number, use the shrink version of dataset
  i = 1
  day.w<-NULL
  while (i < 8){
    if (nrow(temp_df[which(temp_df$wkdy==i),]) < 20){
      i = i+1
    } else{
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
  }
  day.w
}

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
# ============================================================================
# Examples
# First, you must enumerate weekdays
# wk<-newweek(daf3)
# Then, use function 2 to generate a new week of a person 20 times
# weeks <- replicate(20,newweek(daf3),simplify=FALSE)
# new.week <- rbindlist(weeks)
# Finally, calculate the Cumulative Speed Over Time for a newly generated week
# bt104<-bsCalc(weeks_p30)


# Get the Plot, this is pretty raw at this point.
# plot(bt104$sum.t,bt104$s.vel, type="l",xlab="Cumulative Time",ylab="Cumulative Velocity",
#     lwd=1.2,col="brown",main="Bootstrapped Person Speed Plot")
# TODO: Replicate this to hours
# TODO: Refine the plot (5/16)


# ============================================================
# raw workspace
# ============================================================

lct<- NULL
gender<-NULL
age<-NULL
id<-NULL
df.lct<-data.frame(id,age,gender,lct)
colnames(df.lct)<-c("id","gender","age","lct")

get.lct<-function(df){
  id<-df$personid[1]
  lct<-df$sum.t[max(which(df$s.vel>rev(df$s.vel)[1]*1.1|df$s.vel<rev(df$s.vel)[1]*0.9))]
  gender<-df$gen[1]
  age<-df$age[1]
  newrow<-c(id,gender,age,lct)
  rbind(df.lct,newrow)
}

df.lct<-get.lct(bt182)
