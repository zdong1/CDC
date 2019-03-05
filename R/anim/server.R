######################################################################
# R Shiny Server File for Mobility Visualizer
# Author: Zhihang Dong
# Date: 1/13/2019
# Version: 0.1.0.
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#######################################################################

##############################################################
# For future editors:
# House rule for this file:
# Use "<-" for functions, and use "=" for all other operations
##############################################################
# Load Required Packages
{library(shiny)
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
  library(maptools)}

# Time stamp translates timestamp from raw format to POSIXt format, and calculates
# cumulative time and week. No need to do this for formatted data.

timestamp <- function(raw){
  raw = raw[order(raw$time),]
  class(raw$time) = c('POSIXt','POSIXct')
  sort(raw$time)
  raw$sum.t[1] = 0
  raw$t.delta[1] = 0
  for (i in 2:(nrow(raw))){
    raw$t.delta[i] = difftime(raw$time[i], raw$time[i-1], units="hours")
    raw$sum.t[i] = raw$sum.t[i-1]+raw$t.delta[i]
  }
  raw$week = ceiling(raw$sum.t/168)
  raw
}

# Below are required functions

getArea <- function(){
  switz = getData('GADM', country = 'CHE', level = 1)
  vaud = switz[switz$NAME_1 == "Vaud",]
  vaud_m = spTransform(vaud,CRS("+proj=utm +zone=32 +datum=WGS84"))
  vaud_m
}

getPoint <- function(p, start, end, lonColumn = 5, latColumn = 6){
  p = p [which(p$week > start - 1), ]
  p = p [which(p$week < end + 1), ]
  xy = p[c(lonColumn, latColumn)]
  geodf = SpatialPointsDataFrame(coords = xy, data = xy, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 + towgs84=0,0,0"))
  gps = spTransform(geodf, CRS("+proj=utm +zone=32 +datum=WGS84"))
  gps
}

drawGrid <- function(xdim, ydim, xfrom = 284374.8, xto = 365936.1, 
                     yfrom = 5165759.3, yto = 5117012.8){
  # project grids and points, find projections and rasterize the entire region by 4000 unit of easting/northing
  x<- seq(from = xfrom, to = xto, by = xdim)
  y<- seq(from = yfrom, to = yto, by = -ydim)
  xy<-expand.grid(x=x, y=y)
  # Project points to spdf format
  grid.pts = SpatialPointsDataFrame(coords = xy, data = xy, 
                                    proj4string = CRS("+proj=utm +zone=32 +datum=WGS84"))
  gridded(grid.pts) = TRUE
  grid = as(grid.pts, "SpatialPolygons")
  
  for (i in 1:length(grid)){
    grid@polygons[[i]]@ID = substring(grid@polygons[[i]]@ID, 2)
  }
  
  grid
}

gridCount <- function(gridFile, pointFile){
  # First get the cells with observations
  res = over(pointFile,gridFile)
  tab = table(res)
  qid = NULL
  case = NULL
  
  for (j in 1:length(tab)){
    qid[j]<-names(tab[j])
    case [j]<-tab[j]
  }
  # This would generate a table of cells *with observations only*
  record = data.frame(qid,case)
  
  # Extracts Data from sp Polygon and coerce them into a dataframe
  df = data.frame(matrix(ncol = 4, nrow = length(gridFile)))
  var = c("id", "easting", "northing", "pop")
  colnames(df) = var
  
  for (i in 1:length(gridFile)){
    df$id[i] = as.numeric(gridFile@polygons[[i]]@ID)
    df$easting[i] = gridFile@polygons[[i]]@labpt[1]
    df$northing[i] = gridFile@polygons[[i]]@labpt[2]
    df$pop[i] = sum(tab)/length(gridFile)
  }
  
  # merge two tables with left join on df
  final = merge(x=df, y=record, by.x=c('id'), by.y=c('qid'), all.x=TRUE)
  final[is.na(final)] = 0
  final$prop = final$case/sum(final$case)
  final
}

trimCell <- function(leastObs, gridFile, pointFile, gridCountFile){
  gridspdf <- SpatialPolygonsDataFrame(gridFile, data=data.frame(id=row.names(gridFile), 
                                                                 row.names=row.names(gridFile)))
  gridspdf@data$obs = gridCountFile$case
  gridspdf = subset(gridspdf, obs > leastObs)
  grid_trimmed = as(gridspdf, "SpatialPolygons")
  
  for (i in 1:length(grid_trimmed)){
    grid_trimmed@polygons[[i]]@ID = as.character(i)
  }
  grid_trimmed
}

# Notes: Must run function gridCount before running flexScan
flexScan <- function(gridFile, pointFile, nbQueen = FALSE, zeroPolicy = TRUE){
  res = over(pointFile,gridFile)
  tab = table(res)
  qid = NULL
  case = NULL
  for (j in 1:length(tab)){
    qid[j] = names(tab[j])
    case [j] = tab[j]
  }
  record<-data.frame(qid,case)
  
  # Extracts Data from sp Polygon and coerce them into a dataframe
  df <- data.frame(matrix(ncol = 4, nrow = length(gridFile)))
  var <- c("id", "easting", "northing", "pop")
  colnames(df) <- var
  for (i in 1:length(gridFile)){
    df$id[i] = as.numeric(gridFile@polygons[[i]]@ID)
    df$easting[i] = gridFile@polygons[[i]]@labpt[1]
    df$northing[i] = gridFile@polygons[[i]]@labpt[2]
    df$pop[i] = sum(tab)/length(gridFile)
  }
  
  # merge two tables with left join on df
  final = merge(x = df, y = record, by.x = c('id'), by.y = c('qid'), all.x = TRUE)
  
  final[is.na(final)] = 0
  
  xy = data.frame(final[2], final[3])
  # Create adjacency matrix for the polygons you created.
  nb = poly2nb(gridFile, queen = nbQueen) # DO both in queen option
  mat = nb2mat(nb, zero.policy = zeroPolicy)
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

plotDuo <- function(gridFile, pointFile, scanOutput){
  res = over(pointFile,gridFile)
  tab = table(res)
  qid = NULL
  case = NULL
  for (j in 1:length(tab)){
    qid[j] = names(tab[j])
    case[j] = tab[j]
  }
  record = data.frame(qid,case)
  
  # Extracts Data from sp Polygon and coerce them into a dataframe
  df = data.frame(matrix(ncol = 4, nrow = length(gridFile)))
  var = c("id", "easting", "northing", "pop")
  colnames(df) = var
  for (i in 1:length(gridFile)){
    df$id[i] = as.numeric(gridFile@polygons[[i]]@ID)
    df$easting[i] = gridFile@polygons[[i]]@labpt[1]
    df$northing[i] = gridFile@polygons[[i]]@labpt[2]
    df$pop[i] = sum(tab)/length(gridFile)
  }
  
  # merge two tables with left join on df
  final = merge(x=df, y=record, by.x=c('id'), by.y=c('qid'), all.x=TRUE)
  
  final[is.na(final)] = 0
  
  
  quadframe = SpatialPolygonsDataFrame(gridFile,data=as.data.frame(final))
  manual.col = colorRampPalette(c("#f7f6fd","#4635d0"))
  color.match = manual.col(length(unique(quadframe@data$case)))
  lookupTable = sort(unique(quadframe@data$case))
  quadframe$color = color.match[match(quadframe@data$case, lookupTable)]
  {par(mfrow=c(1,2))
    plot(quadframe, col=quadframe$color, border="light gray", lwd=0.5, main = 'Grid-Level Count Map', axes= TRUE)
    plot(pointFile, col='pink', add =TRUE, cex = 0.05, pch = 1)
    plot(gridFile, col = color.clusters(scanOutput), border="gray", main = "Clustering Results", axes= TRUE)}
}

plotL1map <- function(gridFile, fullCount, currentCount){
  L1Count = fullCount
  for (i in 1: nrow(fullCount)){
    L1Count$prop[i] = abs(fullCount$prop[i] - currentCount$prop[i]) 
  }
  quadframe1 = SpatialPolygonsDataFrame(gridFile,data=as.data.frame(L1Count))
  manual.col = colorRampPalette(c("#FFFFD4", "#FED98E", "#FE9929","#CC4C02"))
  color.match1 = manual.col(length(unique(quadframe1@data$prop)))
  lookupTable1 = sort(unique(quadframe1@data$prop))
  quadframe1$color = color.match1[match(quadframe1@data$prop, lookupTable1)]  
  l1error = sum(L1Count$prop)
  {par(mfrow=c(1,1))()
    plot(quadframe1, col=quadframe1$color, border="light gray", lwd=0.5, 
         main = paste("L1 Error Map: L1 =", l1error, sep = " "), axes= TRUE)
  }
}

plotPropmap <- function(gridFile, pointFile1, pointFile2, gridCountFile1, gridCountFile2){
  quadframe1 <-SpatialPolygonsDataFrame(gridFile,data=as.data.frame(gridCountFile1))
  manual.col = colorRampPalette(c("#FFFFD4", "#FED98E", "#FE9929","#CC4C02"))
  color.match1 = manual.col(length(unique(quadframe1@data$prop)))
  lookupTable1 = sort(unique(quadframe1@data$prop))
  quadframe1$color = color.match1[match(quadframe1@data$prop, lookupTable1)]
  quadframe2 <-SpatialPolygonsDataFrame(gridFile,data=as.data.frame(gridCountFile2))
  color.match2 = manual.col(length(unique(quadframe2@data$prop)))
  lookupTable2 = sort(unique(quadframe2@data$prop))
  quadframe2$color = color.match2[match(quadframe2@data$prop, lookupTable2)]
  {par(mfrow=c(1,2))
    plot(quadframe1, col=quadframe1$color, border="light gray", lwd=0.5, main = "Current Proportion Map", axes= TRUE)
    plot(pointFile1, col='pink', add =TRUE, cex = 0.05, pch = 1)
    plot(quadframe2, col=quadframe2$color, border="light gray", lwd=0.5, main = 'Final Proportion Map', axes= TRUE)
    plot(pointFile2, col='pink', add =TRUE, cex = 0.05, pch = 1)
  }
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$distPlot = renderPlot({
    vaud_m = getArea()
    if (input$map == "clu"){
      gps = getPoint(person6106, start = input$range[1], end = input$range[2]) # Key Metric
      grid = drawGrid(input$num2, input$num3) # Secondary Metric
      counter = gridCount(grid, gps)
      # generate bins based on input$bins from ui.R
      grid_t = trimCell(input$num1, grid, gps, counter) # Third Metric
      out = flexScan(grid_t, gps, nbQueen = input$var) # Default Policies 
      plotDuo(grid_t, gps, out) 
    } else if(input$map == "l1e"){
      gps1 = getPoint(person6106, start = 0, end = max(person6106$week))
      grid = drawGrid(input$num2, input$num3) # Secondary Metric
      final1 = gridCount(grid, gps1)
      gps2 = getPoint(person6106, start = 0, end = input$bins)
      final2 = gridCount(grid, gps2)
      plotL1map(grid, final1, final2)
    } else {
      gps1 = getPoint(person6106, start = 0, end = max(person6106$week))
      grid = drawGrid(input$num2, input$num3) # Secondary Metric
      final1 = gridCount(grid, gps1)
      gps2 = getPoint(person6106, start = 0, end = input$bins)
      final2 = gridCount(grid, gps2)
      plotPropmap(grid, gps2, gps1, final2, final1)
    }
  })
})
                                            
                                            
