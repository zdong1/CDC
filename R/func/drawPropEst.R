##########################################################################################################
# CDC Proportional Estimator Drawing
# Zhihang Dong
# Version 0.5
# 3/6/2019
##########################################################################################################
library(dplyr)

rankProp <- function(dat){
  dat$long = round(dat$long*2, digit = 1)/2
  dat$lat = round(dat$lat*2, digit = 1)/2
  proptab<-dat %>%
    group_by(long, lat) %>%
    summarise(a_sum=sum(t.delta))
  proptab$prop_per = round((100*proptab$a_sum/sum(proptab$a_sum)),digits = 1)
  proptab = proptab[-c(3)]
  proptab = proptab[order(-proptab$prop_per),]
}



getFinalLvSet <- function(proptab, levelSet){
  for (k in 1:nrow(proptab)){
    topdat = head(proptab, k)
    if (sum(topdat$prop_per)>levelSet){
      break
    }
  }
  topdat = topdat[-c(3)]
  topdat
}


# topdat = getFinalLvSet(proptab, 20)

propLvSet <- function(dat, lvSet){
  proptab = rankProp(dat)
  topdat = getFinalLvSet(proptab, levelSet = lvSet)
  for (i in max(dat$week):1){
    newdat = dat[which(dat$week <= i), ]
    newptab = rankProp(newdat)
    newtopdat = getFinalLvSet(newptab,levelSet = lvSet)
    if (i == 1){
      return(i)
      break
    }
    if (identical(topdat,newtopdat) == FALSE){
      return(i)
      break
    }
  }
}

resulted = propLvSet(dat,20)
resulted

drawLevelSets <- function(dat, incre){
  finalResults = c()
  bins = c()
  for (j in seq(10, 100, incre)){
    results = propLvSet(dat, j)
    finalResults = append(finalResults, results)
    bins = append(bins, j)
  }
  if(dat$gen[1] == 1){
    cala = "darkgreen"
  } else if(dat$gen[1] == 2){
    cala = "deeppink"
  } else{
    cala = "orange"
  }
  plot(finalResults~bins, bty = "l", type ="b", pch=19, cex = 0.5, col=cala, 
       ylim=c(0,max(dat$week)*1.3), xlim=c(0,100), ylab="Estimator Results", xlab="LevelSets")
  abline(h=max(dat$week), col="red")
  text(x=85, y=max(dat$week + 3), labels="Max Length", col="blue")
}


# Experimental Section
# dat = person_6072
# drawLevelSets(dat,10)
