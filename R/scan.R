#================================================================================
# Spatial scan statistic replicating Tango et al. (2005) paper
# Author: Zhihang Dong
# Last Update: 11/1/2018
#================================================================================

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

sw <- getData('GADM', country = 'CHE', level = 1)
sw$NAME_1
vaud <- sw[sw$NAME_1 == "Vaud",]


# check the CRS to know which map units are used
proj4string(vaud)
#+ proj='longlat' +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0)

# Create a grid of points within the bbox of the SpatialPolygonsDataFrame 
# colorado with decimal degrees as map units
grid <- makegrid(vaud, cellsize = 0.04) # cellsize in map units!

# grid is a data.frame. To change it to a spatial data set we have to
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(vaud)))

### Get long and lat from your data.frame. Make sure that the order is in lon/lat.

xy <- person6174[c(5,6)]
spdf <- SpatialPointsDataFrame(coords = xy, data = xy,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
proj4string(vaud)
proj4string(spdf)
# Rasterize the entire region by 0.05 unit of lon/lat
r <- raster(ext = extent(6, 7, 46, 47), res=c(0.05,0.05))
values(r) <- 1:ncell(r)
# Project raster to the same CRS
rA <- projectRaster(r,crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
quads <- as(rA, 'SpatialPolygons')

# Initial View (non-rasterized)
plot(vaud)
plot(grid, pch = ".", add = T)
plot(spdf, col='red', add =TRUE, cex = 0.05, pch = 1)

# Plot GPS Records on a (flat) spatial polygon
plot(quads)
plot(spdf, col='red', add =TRUE, cex = 0.05, pch = 1)

# Count how many GPS records are in each polygon
res<-over(spdf,quads)
tab<-table(res)
qid<-NULL
case<-NULL
for (j in 1:length(tab)){
  qid[j]<-names(tab[j])
  case [j]<-tab[j]
}
record<-data.frame(qid,case)

# Extracts Data from sp Polygon and coerce them into a dataframe
df <- data.frame(matrix(ncol = 4, nrow = length(quads)))
x <- c("id", "longitude", "latitude", "pop")
colnames(df) <- x
for (i in 1:length(quads)){
  df$id[i]<-quads@polygons[[i]]@ID
  df$longitude[i]<-quads@polygons[[i]]@labpt[1]
  df$latitude[i]<-quads@polygons[[i]]@labpt[2]
  df$pop[i]<-sum(tab)/length(quads)
}

# merge two tables with left join on df
final<-merge(x=df, y=record, by.x=c('id'), by.y=c('qid'), all.x=TRUE)

final[is.na(final)] <- 0
head(final)

xy = data.frame(final[2], final[3])
head(xy)

# Create adjacency matrix for the polygons you created.
nb <- poly2nb(quads)
mat<-nb2mat(nb)

# Sat-Scan Test
# check different parameters: coords, cases (floor are optional)
out = flex.test(coords = xy, cases = floor(final$case), w = mat,
                k = 4, pop = df$pop, nsim = 49, alpha = 0.12, lonlat = TRUE)

# Plot the clusters
plot(quads, col = color.clusters(out))
# ex is not required since we are dealing with GPS data

#write to file(optional)

#write.csv(out, file="flex_outcome.txt")
