library(dplyr)

dat = person_5991

rankProp <- function(dat){
  dat$long = round(dat$long*2, digit = 1)/2
  dat$lat = round(dat$lat*2, digit = 1)/2
  proptab<-dat %>%
    group_by(long, lat) %>%
    summarise(a_sum=sum(t.delta))
  proptab$rank <- NA
  order.a_sum <- order(proptab$a_sum)
  proptab$rank[order.a_sum] <- 1:nrow(proptab)
  proptab<-proptab[-c(3)]
  proptab
}

proptab = rankProp(dat)

propLvSet<-function(proptab, lvSet, roE = 0){
  finalptab<-proptab[which(proptab$rank<round(max(proptab$rank)*lvSet,digit=0)),]
  for (i in 1:max(dat$week)){
    newdat <- dat[which(dat$week < i), ]
    newptab<- rankProp(newdat)
    newptab<-newptab[which(newptab$rank<round(max(proptab$rank)*lvSet,digit=0)),]
    if(nrow(anti_join(newptab,finalptab)) <= roE){
      print(i)
      break
    }
  }
}

