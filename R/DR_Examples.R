 source("DR.R")
df <- read.table("p5924.txt", header=TRUE, 
                     sep="\t")
df2 <- read.table("p5925.txt", header=TRUE, 
                 sep="\t")
df3 <- read.table("p5927.txt", header=TRUE, 
                 sep="\t")
df4 <- read.table("p5928.txt", header=TRUE, 
                 sep="\t")
df5 <- read.table("p5942.txt", header=TRUE, 
                  sep="\t")

D1 = df[1:30000,5:6]
D2 = df2[1:30000,5:6]
D3 = df3[1:30000,5:6]
D4 = df4[1:30000,5:6]
D5 = df5[1:30000,5:6]
# some outliers

### set range and basic processing
x0_lim = c(6.0,8.0)
y0_lim = c(46.0,47.0)

w = which(D1[,1]>x0_lim[1]&D1[,1]<x0_lim[2]&D1[,2]>
            y0_lim[1]&D1[,2]<y0_lim[2])
w2 = which(D2[,1]>x0_lim[1]&D2[,1]<x0_lim[2]&D2[,2]>
            y0_lim[1]&D2[,2]<y0_lim[2])
w3 = which(D3[,1]>x0_lim[1]&D3[,1]<x0_lim[2]&D3[,2]>
            y0_lim[1]&D3[,2]<y0_lim[2])
w4 = which(D4[,1]>x0_lim[1]&D4[,1]<x0_lim[2]&D4[,2]>
            y0_lim[1]&D4[,2]<y0_lim[2])
w5 = which(D5[,1]>x0_lim[1]&D5[,1]<x0_lim[2]&D5[,2]>
            y0_lim[1]&D5[,2]<y0_lim[2])

D1 = D1[w,]
D2 = D2[w2,]
D3 = D3[w3,]
D4 = D4[w4,]
D5 = D5[w5,]
plot(D1,pch=19,cex=0.4)
# looks much better

### start analysis
h0 = 0.1
# smoothing bandwidth


D_DR = DR(data = D1,h = h0)
D_DR2 = DR(data = D2,h = h0)
D_DR3 = DR(data = D3,h = h0)
D_DR4 = DR(data = D4,h = h0)
D_DR5 = DR(data = D5,h = h0)
names(D_DR)


### density ranking contour
colP = colorRampPalette(c("white","tan","brown"))
image(D_DR$x_grid,D_DR$y_grid,matrix(D_DR$gr_alpha, nrow=201), 
      col=colP(100), xlab="Longitude", ylab="Latitude")

### mass-volume curve
plot(x=rev(D_DR$clevel),y=log(D_DR$Mcurve,base = 10), ylim= c(-5,0),
     type="l",lwd=2, ylab="log volume",  xlab=expression(gamma), 
     main="Mass-Volume Curve", col = "pink")
lines(x=rev(D_DR2$clevel),y=log(D_DR2$Mcurve,base = 10), 
     type="l",lwd=2, ylab="log volume",  xlab=expression(gamma), 
     main="Mass-Volume Curve", col = "deepskyblue")
lines(x=rev(D_DR3$clevel),y=log(D_DR3$Mcurve,base = 10), 
     type="l",lwd=2, ylab="log volume",  xlab=expression(gamma), 
     main="Mass-Volume Curve", col = "gold")
lines(x=rev(D_DR4$clevel),y=log(D_DR4$Mcurve,base = 10), 
     type="l",lwd=2, ylab="log volume",  xlab=expression(gamma), 
     main="Mass-Volume Curve", col = "darkorange")
lines(x=rev(D_DR5$clevel),y=log(D_DR5$Mcurve,base = 10), 
     type="l",lwd=2, ylab="log volume",  xlab=expression(gamma), 
     main="Mass-Volume Curve", col = "chartreuse")

### Betti number curve
plot(x=rev(D_DR$clevel),y=D_DR$Bcurve, type="l",lwd=2,
     main="Betti Number Curve", ylab="# of connceted component",
     xlab=expression(gamma), col="pink",ylim=c(0,4))
lines(x=rev(D_DR2$clevel),y=D_DR2$Bcurve, type="l",lwd=2,
     main="Betti Number Curve", ylab="# of connceted component",
     xlab=expression(gamma), col = "deepskyblue")
lines(x=rev(D_DR3$clevel),y=D_DR3$Bcurve, type="l",lwd=2,
     main="Betti Number Curve", ylab="# of connceted component",
     xlab=expression(gamma), col = "gold")
lines(x=rev(D_DR4$clevel),y=D_DR4$Bcurve, type="l",lwd=2,
     main="Betti Number Curve", ylab="# of connceted component",
     xlab=expression(gamma), col = "darkorange")
lines(x=rev(D_DR5$clevel),y=D_DR5$Bcurve, type="l",lwd=2,
     main="Betti Number Curve", ylab="# of connceted component",
     xlab=expression(gamma), col = "chartreuse")


### Persistence curve
plot(D_DR$Pcurve, pch=20, lwd=2, 
     main="Persistence Curve",xlim=c(0,1), col="pink")
lines(D_DR2$Pcurve, pch=20, lwd=2, 
     main="Persistence Curve",xlim=c(0,1), col="deepskyblue")
lines(D_DR3$Pcurve, pch=20, lwd=2, 
     main="Persistence Curve",xlim=c(0,1), col="gold")
lines(D_DR4$Pcurve, pch=20, lwd=2, 
     main="Persistence Curve",xlim=c(0,1), col="darkorange")
lines(D_DR5$Pcurve, pch=20, lwd=2, 
     main="Persistence Curve",xlim=c(0,1), col="chartreuse")

