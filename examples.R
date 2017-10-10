### A demo for "DR.R"
### The GPS dataset is from UCI repository:
###     https://archive.ics.uci.edu/ml/datasets/GPS+Trajectories
### Please download the dataset using the above link.
source("DR.R")

D0 =read.csv("gwlan_linked.csv")
D1 = D0[,3:4]

plot(D1)
# some outliers

### set range and basic processing
x0_lim = c(46.5,46.6)
y0_lim = c(6.5,6.7)

w = which(D1[,1]>x0_lim[1]&D1[,1]<x0_lim[2]&D1[,2]>
            y0_lim[1]&D1[,2]<y0_lim[2])
D1 = D1[w,]

plot(D1,col=44, lwd=0.1, pch =16, xlab="Longitude", ylab="Latitude")
# looks much better

### start analysis
h0 = 0.001
# smoothing bandwidth


D_DR = DR(data = D1,h = h0)
names(D_DR)


### density ranking contour
colP = colorRampPalette(c("white","tan","brown"))
image(D_DR$x_grid,D_DR$y_grid,matrix(D_DR$gr_alpha, nrow=201), 
      col=colP(100), xlab="Longitude", ylab="Latitude")

### mass-volume curve
plot(x=rev(D_DR$clevel),y=log(D_DR$Mcurve,base = 10), 
     type="l",lwd=3, ylab="log volume",  xlab=expression(gamma), 
     main="Mass-Volume Curve")

### Betti number curve
plot(x=rev(D_DR$clevel),y=D_DR$Bcurve, type="l",lwd=3,
     main="Betti Number Curve", ylab="# of connceted component",
     xlab=expression(gamma))

### Persistence curve
plot(D_DR$Pcurve, pch=20, lwd=2, 
     main="Persistence Curve",xlim=c(0,1))

