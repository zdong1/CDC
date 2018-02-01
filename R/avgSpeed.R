#================================================================================
# Seasonality and Velocity Summary Plot
# Author: Zhihang Dong
# Last Update: 12/11/2017
# Run Function 1, then Run Func 2 or 3, or both.
# Func 2 is a summary plot of velocity
# Func 3 is a seasonality plot of velocity (weekday vs weekend)
#================================================================================
# Load Libraries and Data
load("sup.Rda")

library(ggplot2)

# Func 1: This function extract data into flat dataset
# length(lis)
dflist<-ls(pattern="p.*")[1:182]

setUpWeek <- function(person){
  test = person@data
  test$wkdy = weekdays(as.Date(test$time))
  test$week = ceiling(test$sum.t/168)
  test$s.vel = test$sum.dists/test$sum.t
  test
}

for (i in 1:length(dflist)){
  name.df=NULL
  name.df[i]=dflist[i]
  df<-setUpWeek(person=get(name.df[i]))
  df$mark[df$wkdy=="Sunday"|df$wkdy=="Saturday"]<- "Weekend"
  df$mark[df$wkdy=="Monday"|df$wkdy=="Tuesday"|df$wkdy=="Wednesday" |df$wkdy=="Thursday"|df$wkdy=="Friday"] <- "Weekday"
  assign(paste0("df",i),df)
}





#########################################################
# return to functions
#########################################################


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


df.new100<-shrink(df100)
df.new160<-shrink(df160)

# Func 2: This draws the velocity, both weekly and cumulatively
drawTrends <- function(df){
  gen_mark = NULL
  if (is.na(df$gen[1])) {
    gen_mark = "Unknown"
  } else if (df$gen[1] == 2) {
    gen_mark = "Female"
  } else
    gen_mark = "Male"
  week = NULL
  v1 = NULL
  v2 = NULL
  for (i in 1: max(df$week)){
    week[i] = i
    v1[i] = (rev(df[which(df$week==i),10])[1] - df[which(df$week==i),10][1])/
      (rev(df[which(df$week==i),11])[1] - df[which(df$week==i),11][1])
    v2[i] = (rev(df[which(df$week<i+1),10])[1])/(rev(df[which(df$week==i+1),11])[1])
  }
  sp = data.frame(week,v1,v2)
  mu = rev(df$s.vel)[1]
  # bd = 1.96*sd(sp$v1,na.rm=TRUE)/sqrt(nrow(sp)) #band necessary?
  f  = ggplot(sp,aes(week,v1))
  f  + geom_point(col="deepskyblue", cex=2, pch=18)+
    geom_smooth(model=loess,col="firebrick",lwd= 0.6)+
    geom_line(data=sp,aes(week,v2),col="darkgreen",lwd=0.8)+
    ylim(low=0, high =8)+
    geom_hline(yintercept=rev(df$s.vel)[1], color = "red", linetype="dashed")+
    #geom_hline(yintercept=(rev(df$s.vel)[1]+bd),linetype="dashed",color = "blue")+
    #geom_hline(yintercept=(rev(df$s.vel)[1]-bd),linetype="dashed",color = "blue")+
    labs(y="Speed (km/h)",title=paste("Speed Plot:", 
                                      gen_mark, ", Age Group", df$age, ", ID:", 
                                      df$personid,  sep=" "))
}


drawTrends(df.new102)



# Func 4: Generate Frequencies

getFreq <- function(df){
  a<-as.data.frame(table(df$week,df$mark))
  a$id=df$personid[1]
  a$gen=df$gen[1]
  a$age=df$age[1]
  a
}

idlist<-ls(pattern="df*")[2:183]
for (i in 1:length(idlist)){
  temp.df=NULL
  temp.df[i]=idlist[i]
  freq<-getFreq(df=get(temp.df[i]))
  assign(paste0("freq",i),freq)
}

## Finally, let's just combine them together 

freqlist<-ls(pattern="freq*")[2:183]
freq<-get(freqlist[1])
for (i in 2:length(freqlist)){
  freq<-rbind(freq,get(freqlist[i]))
  freq
}


w<-density(0.28*freq$Freq)
plot(w)
d<-density(0.4*(freq[which(freq$Var2=='Weekday'),]$Freq))
k<-density(freq[which(freq$Var2=='Weekend'),]$Freq)
plot(k,col="red",main="Observations per Week")
lines(w,col="blue")
lines(d)
legend("topright", inset=.05, title="Distribution",
       c("Weekend Only","Overall","Weekday"), col=c("red","blue","black"),
       horiz=FALSE,lty=1)

## Barplots of Distributions
library("scales")
pts=c(-1,0,20,50,100,500,2000,15000)
xx<-barplot(table(cut(freq$Freq,pts)),
            names=c("0","(0,20]","(20,50]","(50,100]","(100,500]","(500,2000]","2000+"),
            xlab="count",ylab="Frequency", ylim=c(0,7000),main="Number of Observations Per Week")
## Add text at top of bars
text(x = xx, y = t$Freq, label = t$perc, pos = 3, cex = 0.8, col = "red")



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
