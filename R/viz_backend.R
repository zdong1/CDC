#####################################################
# Visualizer Back-End Functions
#####################################################

# Load required packages

library(sp)
library(sf)
library(rgdal)
library(raster)
library(GISTools)
library(raster)
library(spdep)
library(plyr)
library(ggplot2)
library(rsatscan)
library(smerc)
library(reshape)
library(maptools)

# Time stamp translates timestamp from raw format to POSIXt format, and calculates
# cumulative time and week.

timestamp <- function(raw){
  raw = raw[order(raw$time),]
  class(raw$time) = c('POSIXt','POSIXct')
  sort(raw$time)
  raw$sum.t[1] = 0
  raw$t.delta[1]<-0
  for (i in 2:(nrow(raw))){
    raw$t.delta[i] = difftime(raw$time[i], raw$time[i-1], units="hours")
    raw$sum.t[i] = raw$sum.t[i-1]+raw$t.delta[i]
  }
  raw$week = ceiling(raw$sum.t/168)
  raw
}

getArea <- function(){
  switz <- getData('GADM', country = 'CHE', level = 1)
  vaud <- switz[switz$NAME_1 == "Vaud",]
  vaud_m <- spTransform(vaud,CRS("+proj=utm +zone=32 +datum=WGS84"))
  vaud_m
}

# upto is an indicator of up to which week you want to look at
getPoint <- function(p, upto, lonColumn = 5, latColumn = 6){
  p <- p [which(p$week < upto + 1), ]
  xy <- p[c(lonColumn, latColumn)]
  geodf <- SpatialPointsDataFrame(coords = xy, data = xy, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0"))
  gps <- spTransform(geodf, CRS("+proj=utm +zone=32 +datum=WGS84"))
  gps
}

drawGrid <- function(xdim, ydim, xfrom = 284374.8, xto = 365936.1, 
                     yfrom = 5165759.3, yto = 5117012.8){
  # project grids and points, find projections and rasterize the entire region by 4000 unit of easting/northing
  x<- seq(from = xfrom, to = xto, by = xdim)
  y<- seq(from = yfrom, to = yto, by = -ydim)
  xy<-expand.grid(x=x, y=y)
  # Project points to spdf format
  grid.pts<-SpatialPointsDataFrame(coords = xy, data = xy, 
                                   proj4string = CRS("+proj=utm +zone=32 +datum=WGS84"))
  gridded(grid.pts) = TRUE
  grid <- as(grid.pts, "SpatialPolygons")
  
  for (i in 1:length(grid)){
    grid@polygons[[i]]@ID<-substring(grid@polygons[[i]]@ID, 2)
  }
  
  grid
}

gridCount <- function(gridFile, pointFile){
  # First get the cells with observations
  res<-over(pointFile,grid)
  tab<-table(res)
  qid<-NULL
  case<-NULL
  
  for (j in 1:length(tab)){
    qid[j]<-names(tab[j])
    case [j]<-tab[j]
  }
  # This would generate a table of cells *with observations only*
  record<-data.frame(qid,case)
  
  # Extracts Data from sp Polygon and coerce them into a dataframe
  df <- data.frame(matrix(ncol = 4, nrow = length(gridFile)))
  var <- c("id", "easting", "northing", "pop")
  colnames(df) <- var
  
  for (i in 1:length(gridFile)){
    df$id[i]<-as.numeric(gridFile@polygons[[i]]@ID)
    df$easting[i]<-gridFile@polygons[[i]]@labpt[1]
    df$northing[i]<-gridFile@polygons[[i]]@labpt[2]
    df$pop[i]<-sum(tab)/length(gridFile)
  }
  
  # merge two tables with left join on df
  final<-merge(x=df, y=record, by.x=c('id'), by.y=c('qid'), all.x=TRUE)
  final[is.na(final)] <- 0
  final
}

trimCell <- function(leastObs, gridFile, pointFile){
  gridspdf <- SpatialPolygonsDataFrame(gridFile, data=data.frame(id=row.names(gridFile), 
                                                             row.names=row.names(gridFile)))
  gridspdf@data$obs<-final$case
  testpdf<-subset(gridspdf, obs > leastObs)
  grid2 <- as(testpdf, "SpatialPolygons")
  
  for (i in 1:length(grid2)){
    grid2@polygons[[i]]@ID<-as.character(i)
  }
   grid2
}

# Must run function gridCount before running flexScan
flexScan <- function(gridFile, pointFile, nbQueen = TRUE, zeroPolicy = TRUE){
  res<-over(pointFile,gridFile)
  tab<-table(res)
  qid<-NULL
  case<-NULL
  for (j in 1:length(tab)){
    qid[j]<-names(tab[j])
    case [j]<-tab[j]
  }
  record<-data.frame(qid,case)
  
  # Extracts Data from sp Polygon and coerce them into a dataframe
  df <- data.frame(matrix(ncol = 4, nrow = length(grid2)))
  var <- c("id", "easting", "northing", "pop")
  colnames(df) <- var
  for (i in 1:length(gridFile)){
    df$id[i]<-as.numeric(gridFile@polygons[[i]]@ID)
    df$easting[i]<-gridFile@polygons[[i]]@labpt[1]
    df$northing[i]<-gridFile@polygons[[i]]@labpt[2]
    df$pop[i]<-sum(tab)/length(grid2)
  }
  
  # merge two tables with left join on df
  final<-merge(x=df, y=record, by.x=c('id'), by.y=c('qid'), all.x=TRUE)
  
  final[is.na(final)] <- 0
  
  xy = data.frame(final[2], final[3])
  # Create adjacency matrix for the polygons you created.
  nb <- poly2nb(grid2, queen = nbQueen) # DO both in queen option
  mat<-nb2mat(nb, zero.policy = zeroPolicy)
  final$exp = 0
  trueCentroids = gCentroid(gridFile,byid=TRUE)
  cent = trueCentroids@coords
  out = flex.test(coords = cent, cases = final$case, w = mat, ex = final$pop, k = 10,
                  pop = final$pop, nsim = 200, alpha = 0.005, lonlat =FALSE)
  out
}

plotGrid <- function(gridFile, pointFile){
    plot(gridFile)
    text(coordinates(gridFile), labels=sapply(slot(gridFile, "polygons"), 
                                              function(i) slot(i,"ID")), cex=0.7)
    plot(pointFile, col= 'blue', add =TRUE, cex = 0.04, pch = 1)
}



##############################################
### Try Me !
##############################################

person6106<-timestamp(person6106)
vaud_m = getArea()
gps = getPoint(person6106, upto = 8)
grid = drawGrid(4000, 4000)
final = gridCount(grid, gps)
plotGrid(gridFile = grid2, pointFile = gps)
grid2 = trimCell(10, grid, gps)
out = flexScan(grid2, gps)
{plot(grid2, col = color.clusters(out))
  plot(gps, col='pink', add =TRUE, cex = 0.05, pch = 1)}
