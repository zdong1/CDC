load("twelve.Rda")
library(ggplot2)
# This function extract data into flat dataset
setUpWeek <- function(person){
  test = person@data
  test$wkdy = weekdays(as.Date(test$time))
  test$week = ceiling(test$sum.t/168)
  monday = test[which(test$wkdy=='Monday'),]
  tues = test[which(test$wkdy=='Tuesday'),]
  weds = test[which(test$wkdy=='Wednesday'),]
  thurs = test[which(test$wkdy=='Thursday'),]
  friday = test[which(test$wkdy=='Friday'),]
  sats = test[which(test$wkdy=='Saturday'),]
  sunday = test[which(test$wkdy=='Sunday'),]
  test
}

df = setUpWeek(p.5943)

# This draws the function
drawTrends <- function(flat){
  v1 = NULL
  week = NULL
  v2 = NULL
  for (i in 1: max(df$week)){
    week[i] = i
    if(nrow(df[which(df$week==i),]) > 20){
      v1[i] = (rev(df[which(df$week==i),10])[1] - df[which(df$week==i),10][1])/
        (rev(df[which(df$week==i),11])[1] - df[which(df$week==i),11][1])
    }  else{
      v1[i] = NA
    }
    v2[i] = (rev(df[which(df$week<i+1),10])[1])/(rev(df[which(df$week==i+1),11])[1])
  }
  sp = data.frame(week,v1,v2)
  mu = rev(flat$s.vel)[1]
  bd = 1.96*sd(flat$vel,na.rm=TRUE)/sqrt(nrow(flat))
  f  = ggplot(sp,aes(week,v1))
  f  + geom_point(col="deepskyblue")+
    geom_smooth(model=loess,col="coral")+
    geom_line(data=sp,aes(week,v2),col="darkgreen",lwd=1.2)+ 
    ylim(low=0, high =10)+
    geom_hline(yintercept=rev(df$s.vel)[1],color = "red")+
    geom_hline(yintercept=(rev(df$s.vel)[1]+bd),linetype="dashed",color = "blue")+
    geom_hline(yintercept=(rev(df$s.vel)[1]-bd),linetype="dashed",color = "blue")+
    labs(y="Speed (km/h)",title="Week-by-Week and Cumulative Speed")
}

drawTrends(df)







### Weekday Seasonality Plot (Very rough)
days1 <- NULL
days2 <- NULL
days3 <- NULL
days4 <- NULL
days5 <- NULL
days6 <- NULL
days7 <- NULL

for (i in 1:108){
  days1[i]<-(rev(monday[which(monday$week==i),10])[1] - monday[which(monday$week==i),10][1])/
    (rev(monday[which(monday$week==i),11])[1] - monday[which(monday$week==i),11][1])
}

for (i in 1:108){
  days2[i]<-(rev(tues[which(tues$week==i),10])[1] - tues[which(tues$week==i),10][1])/
    (rev(tues[which(tues$week==i),11])[1] - tues[which(tues$week==i),11][1])
}

for (i in 1:108){
  days3[i]<-(rev(weds[which(weds$week==i),10])[1] - weds[which(weds$week==i),10][1])/
    (rev(weds[which(weds$week==i),11])[1] - weds[which(weds$week==i),11][1])
}

for (i in 1:108){
  days4[i]<-(rev(thurs[which(thurs$week==i),10])[1] - thurs[which(thurs$week==i),10][1])/
    (rev(thurs[which(thurs$week==i),11])[1] - thurs[which(thurs$week==i),11][1])
}

for (i in 1:108){
  days5[i]<-(rev(friday[which(friday$week==i),10])[1] - friday[which(friday$week==i),10][1])/
    (rev(friday[which(friday$week==i),11])[1] - friday[which(friday$week==i),11][1])
}

for (i in 1:108){
  days6[i]<-(rev(sats[which(sats$week==i),10])[1] - sats[which(sats$week==i),10][1])/
    (rev(sats[which(sats$week==i),11])[1] - sats[which(sats$week==i),11][1])
}

for (i in 1:108){
  days7[i]<-(rev(thurs[which(thurs$week==i),10])[1] - thurs[which(thurs$week==i),10][1])/
    (rev(thurs[which(thurs$week==i),11])[1] - thurs[which(thurs$week==i),11][1])
}

plot(days1,pch=19,cex=1.4,col="cyan",ylim=c(0,20),xlab="Week",ylab="Speed (km/h)")
points(days2,pch=19,cex=1.4, col="green",ylim=c(0,20))
points(days3,pch=19,cex=1.4,col="pink",ylim=c(0,20))
points(days4,pch=19,cex=1.4,col="yellow",ylim=c(0,20))
points(days5,pch=19,cex=1.4,col="burlywood",ylim=c(0,20))
points(days6,pch=19,cex=1.4,col="red",ylim=c(0,20))
points(days7,pch=19,cex=1.4,col="orange",ylim=c(0,20))
abline(h=8,lty=2)
plot(days6,type="p",pch=19,ylim=c(0,2))