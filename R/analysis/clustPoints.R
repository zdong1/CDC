load("byperson.Rda")
library(splancs)
library(spatstat)
library(sp)
library(maptools)
library(lattice)

# pppCov: Change Flat DF into normalized geoframe
pppCov<- function(pid,norm){
  p<-pid
  df<-data.frame(x=p$long,
                 y=p$lat,
                 gen=p$gen,
                 age=p$age)
  W <- owin(c(6.0,9.0),c(45.5,48.5))
  pp0<-as.ppp(df,W=W)
  pp1<-as(pp0,"SpatialPoints")
  if (norm == TRUE) {
    pp2<-elide(pp1,scale=TRUE, unitsq=TRUE)
    pp3<-as(pp2,"ppp")
    pp2
  } else {
    pp1
  }
}

# Case Study: person A, C = Male, B, D = Female
# A, B = Reference Age Group (3), C, D = Elder Age Group (4)

pa<-pppCov(person6087,norm=TRUE)
pb<-pppCov(person6083,norm=TRUE)  
pc<-pppCov(person6168,norm=TRUE)
pd<-pppCov(person5924,norm=TRUE)

# Draw Trajectories

dpp<-data.frame(rbind(coordinates(pa),
                      coordinates(pb),
                      coordinates(pc),
                      coordinates(pd)))
na<-nrow(coordinates(pa))
nb<-nrow(coordinates(pb))
nc<-nrow(coordinates(pc))
nd<-nrow(coordinates(pd))
dpp <- cbind(dpp, c(rep("Male, 2", na), rep("Female 2", nb),
                    rep("Male, 3", nc), rep("Female 3", nd)))
names(dpp) <- c("x","y","DATASET")
print(xyplot(y ~ x | DATASET, data = dpp,
             pch = 19, aspect = 1, cex = 0.5))

# G-Functions
set.seed(120109)
r <- seq(0, sqrt(2)/6, by = 0.001)
env.a <- envelope(as(pa, "ppp"),
                   fun = Gest, r = r, nrank = 2, nsim = 99)
env.b <- envelope(as(pb, "ppp"), fun = Gest,
                   r = r, nrank = 2, nsim = 99)
env.c <- envelope(as(pc, "ppp"),
                     fun = Gest, r = r, nrank = 2, nsim = 99)
env.d <- envelope(as(pd, "ppp"),
                  fun = Gest, r = r, nrank = 2, nsim = 99)
Gresults <- rbind(env.a,env.b,env.c,env.d)
Gresults <- cbind(Gresults, y = rep(c("Male 2",
                                      "Male 3", "Female 2","Female 3"), each = length(r)))
print(xyplot(obs ~ theo | y, data = Gresults,
             type = "l", xlab = "Theoretical", ylab = "Observed",
             panel = function(x, y, subscripts) {
               lpolygon(c(x, rev(x)), c(Gresults$lo[subscripts],
                                        rev(Gresults$hi[subscripts])),
                        border = "gray", col = "gray")
               llines(x, y, col = "black", lwd = 2)
             }))

# F-Functions
set.seed(3000)
fen.a <- envelope(as(pa, "ppp"),
                    fun = Fest, r = r, nrank = 2, nsim = 99)
fen.b <- envelope(as(pb, "ppp"), fun = Fest,
                    r = r, nrank = 2, nsim = 99)
fen.c <- envelope(as(pc, "ppp"),
                      fun = Fest, r = r, nrank = 2, nsim = 99)
fen.d <- envelope(as(pc, "ppp"),
                  fun = Fest, r = r, nrank = 3, nsim = 99)

Fresults <- rbind(fen.a,fen.b,fen.c,fen.d)
Fresults <- cbind(Gresults, y = rep(c("Male 2",
                                      "Male 3", "Female 2","Female 3"), each = length(r)))
print(xyplot(obs ~ theo | y, data = Fresults,
             type = "l", xlab = "Theoretical", ylab = "Observed",
             panel = function(x, y, subscripts) {
               lpolygon(c(x, rev(x)), c(Fresults$lo[subscripts],
                                        rev(Fresults$hi[subscripts])),
                        border = "gray", col = "gray")
               llines(x, y, col = "black", lwd = 2)
             }))

# Difference as of K functions

cca<-pa@coords
ccpts.a <- as.points(cca[1:10000,1], cca[1:10000,2])
ccd<-pd@coords
ccpts.d <- as.points(ccd[1:10000,1], ccd[1:10000,2])


polyx <- c(0, 0.51, 0.875, 0.62, 0.4)
polyy <- c(0.23, 0.73, 0.65, 0.13, 0.105)
poly <- matrix(c(polyx, polyy), nrow = 5,
               ncol = 2, byrow = F)

plot(ccpts.a, type = "n", xlab = "Eastings",
     ylab = "Northings")
points(ccpts.a, pch = 1, cex = 0.4, col = "blue")
points(ccpts.d, pch = 2, cex = 0.4, col = "red")
legend("topleft", legend = c("Males",
                             "Females"), col = c("blue", "red"), bty = "n",
       pch = 1:2, cex = 0.5)
polymap(poly, add = T)

d <- seq(0, 1, 0.05)
khat0 <- khat(ccpts.a, poly, d)
khat1 <- khat(ccpts.d, poly, d)
detach(package:spatstat) # Kenv.label doesn't work 
# when spatstat present
plot(d,khat1-khat0,ylim=c(-0.2,0.2), xlim=c(0,0.8),pch=19,col="blue",ylab=expression(hat(K)(d)[1]-hat(K)(d)[0]),xlab="Distance, d",cex.lab=.7,cex=.5)

lines(d,khat1-khat0)
Env10 <- Kenv.label(as.points(ccpts.a),as.points(ccpts.d),
                    poly=poly,nsim=99,d,quiet=F) # splancs func
points(d,Env10$upper,col="gray",type="l")
points(d,Env10$lower,col="gray",type="l")
abline(0,0,col="red")
