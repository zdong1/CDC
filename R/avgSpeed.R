#================================================================================
# Seasonality and Velocity Summary Plot
# Author: Zhihang Dong
# Last Update: 12/11/2017
# Run Function 1, then Run Func 2 or 3, or both.
# Func 2 is a summary plot of velocity
# Func 3 is a seasonality plot of velocity (weekday vs weekend)
#================================================================================
# Load Libraries and Data
load("twelve.Rda")
library(ggplot2)

# Func 1: This function extract data into flat dataset
setUpWeek <- function(person){
  test = person@data
  test$wkdy = weekdays(as.Date(test$time))
  test$week = ceiling(test$sum.t/168)
  test
}

# Load Data
df = setUpWeek(p.5939)

# Func 2: This draws the velocity, both weekly and cumulatively
drawTrends <- function(flat){
  v1 = NULL
  week = NULL
  v2 = NULL
  gen_mark = NULL
  if (flat$gen[1] == 1) {
    gen_mark = "Male"
  } else if (flat$gen[1] == 2) {
    gen_mark = "Female"
  } else
    gen_mark = "Unknown"
  for (i in 1: max(flat$week)){
    week[i] = i
    if(nrow(flat[which(flat$week==i),]) > 20){
      v1[i] = (rev(flat[which(flat$week==i),10])[1] - flat[which(flat$week==i),10][1])/
        (rev(flat[which(flat$week==i),11])[1] - flat[which(flat$week==i),11][1])
    }  else{
      v1[i] = NA
    }
    v2[i] = (rev(flat[which(flat$week<i+1),10])[1])/(rev(flat[which(flat$week==i+1),11])[1])
  }
  sp = data.frame(week,v1,v2)
  mu = rev(flat$s.vel)[1]
  # bd = 1.96*sd(sp$v1,na.rm=TRUE)/sqrt(nrow(sp)) #band necessary?
  f  = ggplot(sp,aes(week,v1))
  f  + geom_point(col="deepskyblue", cex=2, pch=18)+
    geom_smooth(model=loess,col="firebrick",lwd= 0.6)+
    geom_line(data=sp,aes(week,v2),col="darkgreen",lwd=0.8)+
    geom_line(data=flat,aes(week,s.vel),col="darkgoldenrod",lwd=0.3)+
    ylim(low=0, high =10)+
    geom_hline(yintercept=rev(flat$s.vel)[1], color = "red", linetype="dashed")+
    #geom_hline(yintercept=(rev(flat$s.vel)[1]+bd),linetype="dashed",color = "blue")+
    #geom_hline(yintercept=(rev(flat$s.vel)[1]-bd),linetype="dashed",color = "blue")+
    labs(y="Speed (km/h)",title=paste("Speed Plot:", 
                                      gen_mark, ", Age Group", flat$age, ", ID:", 
                                      flat$personid,  sep=" "))
}
drawTrends(df)

### Func 3: Weekday Seasonality Plot (A bit less rough, update 12/11)
WeekPlot <-function(daf){
  days1 = days2 = days3 = days4 = days5 = days6 = days7 = NULL
  monday = daf[which(daf$wkdy=="Monday"),]
  tues = daf[which(daf$wkdy=="Tuesday"),]
  weds = daf[which(daf$wkdy=="Wednesday"),]
  thurs = daf[which(daf$wkdy=="Thursday"),]
  friday = daf[which(daf$wkdy=="Friday"),]
  sats = daf[which(daf$wkdy=="Saturday"),]
  sunday = daf[which(daf$wkdy=="Sunday"),]
  for (i in 1:max(daf$week)){
    days1[i]<-(rev(monday[which(monday$week==i),10])[1] - monday[which(monday$week==i),10][1])/
      (rev(monday[which(monday$week==i),11])[1] - monday[which(monday$week==i),11][1])
    days2[i]<-(rev(tues[which(tues$week==i),10])[1] - tues[which(tues$week==i),10][1])/
      (rev(tues[which(tues$week==i),11])[1] - tues[which(tues$week==i),11][1])
    days3[i]<-(rev(weds[which(weds$week==i),10])[1] - weds[which(weds$week==i),10][1])/
      (rev(weds[which(weds$week==i),11])[1] - weds[which(weds$week==i),11][1])
    days4[i]<-(rev(thurs[which(thurs$week==i),10])[1] - thurs[which(thurs$week==i),10][1])/
      (rev(thurs[which(thurs$week==i),11])[1] - thurs[which(thurs$week==i),11][1])
    days5[i]<-(rev(friday[which(friday$week==i),10])[1] - friday[which(friday$week==i),10][1])/
      (rev(friday[which(friday$week==i),11])[1] - friday[which(friday$week==i),11][1])
    days6[i]<-(rev(sats[which(sats$week==i),10])[1] - sats[which(sats$week==i),10][1])/
      (rev(sats[which(sats$week==i),11])[1] - sats[which(sats$week==i),11][1])
    days7[i]<-(rev(sunday[which(sunday$week==i),10])[1] - sunday[which(sunday$week==i),10][1])/
      (rev(sunday[which(sunday$week==i),11])[1] - sunday[which(sunday$week==i),11][1])
  }
  
  plot(days1,pch=19,cex=0.7,col="gray",ylim=c(0,30),xlab="Week",ylab="Speed (km/h)",xlim=c(0,max(daf$week)))
  points(days2,pch=19,cex=0.7, col="gray",ylim=c(0,20))
  points(days3,pch=19,cex=0.7,col="gray",ylim=c(0,20))
  points(days4,pch=19,cex=0.7,col="gray",ylim=c(0,20))
  points(days5,pch=19,cex=0.7,col="darkturquoise",ylim=c(0,20))
  points(days6,pch=19,cex=0.7,col="red",ylim=c(0,20))
  points(days7,pch=19,cex=0.7,col="orange",ylim=c(0,20))
  abline(h=8,lty=2)
}
WeekPlot(df)